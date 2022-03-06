;;;; tworp.lisp

(defpackage #:tworp
  (:use #:cl #:with-user-abort)
  (:export :main))

(in-package #:tworp)

(declaim (inline generate-link cache-id build-media-list
                 agetf has-mentions-p self-reply-p reply-p))

(defvar *tweet-buffer* nil)
(defvar *media-dir* nil)
(defvar *id-file* nil)

(opts:define-opts
  (:name :help
   :description "print this help text"
   :short #\h
   :long "help")
  (:name :config-file
   :description "config file FILE to load"
   :short #\c
   :long "config"
   :arg-parser #'identity
   :meta-var "FILE")
  (:name :version
   :description "prints the application version"
   :long "version"))

(defun generate-link (tweet)
  "generates a twitter link to TWEET"
  (format nil "https://~A/~A/status/~A"
          (if (conf:config :nitterize-links)
              (conf:config :nitter-domain "nitter.eu")
              "twitter.com")
          (chirp:screen-name (chirp:user tweet))
          (chirp:id tweet)))

(defun cache-id (tweet)
  "cache and save ID for TWEET"
  (with-open-file (out *id-file* :direction :output
                                 :if-does-not-exist :create
                                 :if-exists :supersede)
    (princ (chirp:id tweet) out)))

(defun build-media-list (media)
  "applies media descriptions to the uploaded media in a format that glacier knows how to handle"
  (if (conf:config :media-description)
      (loop :for m :in media
            :collect (list m (conf:config :media-description)))
   media))

(defun agetf (place indicator &optional default)
  (or (cdr (assoc indicator place))
      default))

(defun reply-p (tweet)
  (and (has-mentions-p tweet)
       (not (self-reply-p tweet))))

(defun has-mentions-p (tweet)
  (agetf (chirp:entities tweet) :user-mentions))

(defun self-reply-p (tweet)
  (and (string= (conf:config :twitter-user) (agetf (chirp:in-reply-to tweet) :screen-name))
       (string= (conf:config :twitter-user) (chirp:screen-name (chirp:user tweet)))))

(defun post-to-mastodon (tweet)
  "posts TWEET to mastodon"

  ;; when we have a tweet
  ;; and its a reply and we are allowing replies
  ;; or its not a reply at all
  (when (and tweet
             (or (not (reply-p tweet))
                 (conf:config :include-replies)))
      ;; the regex-replace-all turns all twitter @mentions into @mention@twitter.com
      ;;  this is to avoid name collision with anyone on masto
      (let ((media (mapcar #'download-tweet-media (agetf (chirp:entities tweet) :media))))
        (glacier:post (format nil "~A~%~%Source: ~A"
                              (ppcre:regex-replace-all "@(\\w*)" (chirp:xml-decode (chirp:full-text tweet))
                                                       "@\\1@twitter.com")
                              (generate-link tweet))
                      :cw (when (conf:config :enable-cw)
                            (conf:config :content-warning "twitter crosspost"))
                      :visibility (conf:config :visibility :unlisted)
                      :sensitive (or (conf:config :enable-cw) (conf:config :sensitive-media))
                      :media (build-media-list media))
              ;; once we post the tweet, we cache the ID so we dont post it again
        (cache-id tweet))))

(defun download-tweet-media (media)
  "downloads MEDIA and saves it to the filesystem for crossposting"
  (when media
    (let* ((url (chirp:media-url-https media))
           (filename (concatenate 'string *media-dir* (pathname-name url) "." (pathname-type url))))
      (with-open-file (out filename :direction :output
                                    :if-does-not-exist :create
                                    :if-exists :supersede
                                    :element-type '(unsigned-byte 8))
        ;; fetch and save our media to the hard drive
        (loop :with input := (drakma:http-request (chirp:media-url-https media) :want-stream t)
              :for byte := (read-byte input nil nil)
              :while byte
              :do (write-byte byte out)))
      ;; return the full file path to the newly downloaded media
      (merge-pathnames filename (uiop:getcwd)))))

(defun main ()
  "binary entry point"
  (macrolet ((when-option ((options opt) &body body)
              `(let ((it (getf ,options ,opt)))
                 (declare (ignorable it))
                 (when it
                   ,@body))))

    ;; parse all of the options we got passed 
    (multiple-value-bind (options free-args)
        (handler-case (opts:get-opts)
          (opts:missing-arg (condition)
            (format t "fatal: option ~s needs an argument!~&"
                    (opts:option condition))
            (opts:exit 1))
          (opts:arg-parser-failed (condition)
            (format t "fatal: cannot parse ~s as argument of ~s~&"
                    (opts:raw-arg condition)
                    (opts:option condition))
            (opts:exit 1))
          (opts:missing-required-option (con)
            (format t "fatal: ~a~&" con)
            (opts:exit 1))
          (opts:unknown-option (con)
            (format t "fatal: ~A~&" con)
            (opts:exit 1)))
      (declare (ignorable free-args))

      ;; when the user passes -h/--help
      (when-option (options :help)
        (opts:describe :prefix "repost tweets from twitter to mastodon"
                       :usage-of "tworp")
        (opts:exit 0))

      ;; when the user passed --version
      (when-option (options :version)
        (format t "tworp v~A~&" 
                ;; the #. allows this value to be calculated at compile time
                ;;  instead of run time (making this WAY faster lmao)
                #.(asdf:component-version (asdf:find-system :tworp)))
        (opts:exit 0))

      
      (handler-case
          (with-user-abort
               (glacier:run-bot ((make-instance 'glacier:mastodon-bot :config-file
                                                (getf options :config-file))
                                 :with-websocket nil)               

                ;; sets our variables once we have our config loaded
                (setf *id-file* (concatenate 'string (conf:config :twitter-user) ".id")
                      *media-dir* (concatenate 'string "./" (conf:config :twitter-user) "-media/"))
                                
                ;; if we dont have our twitter api keys set then we load them from our config
                ;;  (these get populated by the dev on a release build)
                (unless (or chirp:*oauth-api-key* chirp:*oauth-api-secret*)
                  (setf chirp:*oauth-api-key* (conf:config :twitter-api-key)
                        chirp:*oauth-api-secret* (conf:config :twitter-api-secret)))

                 ;; make sure our media directory exists
                (ensure-directories-exist (pathname *media-dir*))

                 ;; in a separate thread fetch new tweets as dictated by our interval 
                 ;;  set up in the config
                (bt:make-thread #'(lambda ()
                                    (chirp:map-timeline :user #'(lambda (status) (setf *tweet-buffer*
                                                                                       (append *tweet-buffer* (list status))))
                                                        :screen-name (conf:config :twitter-user) :tweet-mode "extended"
                                                        
                                                        :since-id (when (uiop:file-exists-p *id-file*)
                                                                    (with-open-file (in *id-file*)
                                                                      (read-line in)))
                                                        :cooldown (* (conf:config :interval 10) 60))))

                 ;; post to mastodon after each timeout, as dictated by our config
                (glacier:after-every ((conf:config :timeout 5) :minutes)
                  (post-to-mastodon (pop *tweet-buffer*)))))

        ;; if a user stops us, exit gracefully
        (user-abort ()
          (opts:exit 0))

        ;; if we hit an issue, tell the user and exit gracefully
        (error (e)
          (format t "encountered uncrecoverable error: ~A~&" e))))))
