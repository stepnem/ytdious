;;; ytdious.el --- browse YouTube via Invidious -*- lexical-binding: t; -*-

;; Author: Stefan Huchler
;;         Gabriele Rastello
;;         Štěpán Němec <stepnem@gmail.com>
;; Maintainer: Štěpán Němec <stepnem@gmail.com>
;; Version: 0.2b
;; Keywords: comm multimedia youtube
;; URL: https://github.com/stepnem/ytdious
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "25.1"))
;; Tested-with: GNU Emacs master branch

;;; Commentary:

;; This package provides means to search and view YouTube media via
;; the Invidious API: https://docs.invidious.io/

;; Corrections and constructive feedback appreciated.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'ring)

(defvar ytdious-sort-options
  (ring-convert-sequence-to-ring
   '("relevance" "rating" "upload_date" "view_count"))
  "Available sort options.")

(defvar-local ytdious-sort-reverse nil
  "Non-nil means sort videos in reverse order.")

(defvar ytdious-player-program "mpv"
  "Program for playing videos.")

(defvar ytdious-player-options '("--really-quiet")
  "List of options for `ytdious-player-program'.")

(defvar ytdious-date-options (ring-convert-sequence-to-ring
                              '("hour" "today" "week" "month" "year" "all"))
  "Available date options.")

(defvar ytdious-invidious-api-url "https://invidio.xamh.de"
  "URL to an Invidious instance (without trailing \"/\").
The instance must support the Invidious API.  A list of
available instances can be found at the URL
`https://api.invidious.io/?sort_by=api'.")

(defvar ytdious-fields
  "author,authorID,lengthSeconds,published,title,videoId,viewCount"
  "Fields of interest for video search.")

(defvar-local ytdious-videos nil
  "List of videos currently on display.")

(defvar ytdious-published-date-time-string "%Y-%m-%d"
  "Time-string used to render the published date of the video.
See `format-time-string' for information on how to edit this variable.")

(defvar-local ytdious-sort-criterion "relevance"
  "Criterion to date limit the results of the search query.")

(defvar-local ytdious-date-criterion "year"
  "Criterion to date limit the results of the search query.")

(defvar-local ytdious-current-page 1
  "Current page of the current `ytdious-search-term'.")

(defvar-local ytdious-search-term ""
  "Current search string as used by `ytdious-search'.")

(defvar-local ytdious-channel ""
  "Current channel as used by `ytdious-search'.")

(defvar ytdious-frame nil)
(defvar ytdious-enter-buffer-function #'ytdious-pop-up-frame)
(defun ytdious-pop-up-frame (buffer)
  "Switch to BUFFER in `ytdious-frame'."
  (select-frame (if (frame-live-p ytdious-frame)
                    ytdious-frame
                  (setq ytdious-frame
                        (make-frame '((name . "Emacs ytdious")
                                      (title . "ytdious"))))))
  (switch-to-buffer buffer)
  (set-window-dedicated-p nil t))

(defvar ytdious-video-author-width 20
  "Column width reserved for channel names in `ytdious' buffers.")

(defvar ytdious-video-title-width 100
  "Column width reserved for the video title in `ytdious' buffers.")

(defface ytdious-video-published-face
  '((((class color) (background light)) (:foreground "#a0a"))
    (((class color) (background dark))  (:foreground "#7a7")))
  "Face used for the video published date.")

(defface ytdious-channel-name-face
  '((((class color) (background light)) (:foreground "#aa0"))
    (((class color) (background dark))  (:foreground "#ff0")))
  "Face used for channel names.")

(defface ytdious-video-length-face
  '((((class color) (background light)) (:foreground "#aaa"))
    (((class color) (background dark))  (:foreground "#77a")))
  "Face used for the video length.")

(defface ytdious-video-view-face
  '((((class color) (background light)) (:foreground "#00a"))
    (((class color) (background dark))  (:foreground "#aa7")))
  "Face used for the video views.")

(defvar-local ytdious-mode-line-info nil
  "Construct prepended to `mode-line-misc-info' in `ytdious-mode' buffers.")
(define-derived-mode ytdious-mode tabulated-list-mode "ytdious"
  "Major mode for `ytdious' buffers."
  (setq-local mode-line-misc-info
	      (cons '(ytdious-mode-line-info
		      (:propertize ytdious-mode-line-info
		       face mode-line-buffer-id))
		    mode-line-misc-info))
  (setq-local split-height-threshold 22)
  (setq-local revert-buffer-function #'ytdious--revert-buffer))

(let ((map ytdious-mode-map))
  (define-key map "d" #'ytdious-rotate-date)
  (define-key map "D" #'ytdious-rotate-date-backwards)
  (define-key map "r" #'ytdious-rotate-sort)
  (define-key map "R" #'ytdious-rotate-sort-backwards)
  (define-key map "o" #'ytdious-toggle-sort-direction)
  (define-key map "t" #'ytdious-display-full-title)
  (define-key map "s" #'ytdious-search)
  (define-key map "S" #'ytdious-search-recent)
  (define-key map "c" #'ytdious-view-channel)
  (define-key map "C" #'ytdious-view-channel-at-point)
  (define-key map "w" #'ytdious-copy-url-at-point)
  (define-key map ">" #'ytdious-search-next-page)
  (define-key map "<" #'ytdious-search-previous-page)
  (define-key map "]" #'ytdious-search-next-page)
  (define-key map "[" #'ytdious-search-previous-page)
  (define-key map (kbd "RET") #'ytdious-play)
  (define-key map "e" #'ytdious-play-emms))

(defun ytdious-toggle-sort-direction ()
  "Toggle reverse sorting of the video list."
  (interactive)
  (setq ytdious-sort-reverse
        (not ytdious-sort-reverse))
  (ytdious--draw-buffer t))

(declare-function emms-play-url "emms-source-file")
(defun ytdious-play-emms ()
  "Call `emms-play-url' on URL of video at point."
  (interactive)
  (emms-play-url (concat ytdious-invidious-api-url
                         "/watch?v="
                         (tabulated-list-get-id))))

(defun ytdious-play ()
  "Play video at point."
  (interactive)
  (apply #'start-process "ytdious player" " *ytdious player*"
         ytdious-player-program
         `(,@ytdious-player-options
           ,(concat ytdious-invidious-api-url
                    "/watch?v="
                    (tabulated-list-get-id)))))

(defun ytdious-copy-url-at-point ()
  "Copy video URL at point to `kill-ring' and system clipboard.
The URL is also displayed in the echo area."
  (interactive)
  (let ((url (concat ytdious-invidious-api-url
                     "/watch?v="
                     (tabulated-list-get-id))))
    (gui-set-selection 'CLIPBOARD url)
    (kill-new url)
    (message "%s" url)))

(defun ytdious--format-author (name)
  "Format a channel NAME to be inserted in the `ytdious' buffer."
  (propertize name 'face 'ytdious-channel-name-face))

(defun ytdious--format-video-length (seconds)
  "Format SECONDS to be inserted in the `ytdious' buffer."
  (let ((formatted-string (concat (format-seconds "%.2h" seconds)
                                  ":"
                                  (format-seconds "%.2m" (mod seconds 3600))
                                  ":"
                                  (format-seconds "%.2s" (mod seconds 60)))))
    (propertize formatted-string 'face 'ytdious-video-length-face)))

(defun ytdious--format-video-views (views)
  "Format video VIEWS to be inserted in the `ytdious' buffer."
  (propertize (number-to-string views) 'face 'ytdious-video-view-face))

(defun ytdious--format-video-published (published)
  "Format video PUBLISHED date to be inserted in the `ytdious' buffer."
  (propertize (format-time-string ytdious-published-date-time-string
                                  (seconds-to-time published))
              'face 'ytdious-video-published-face))

(defun ytdious--revert-buffer (_ignore-auto _noconfirm)
  "Revert function for `ytdious-mode' buffers.
See `revert-buffer' for meaning of IGNORE-AUTO and NOCONFIRM."
  (ytdious--draw-buffer))

(defun ytdious--draw-buffer (&optional offline)
  "Draw a list of videos.
OFFLINE means don't query the API, just redraw the list."
  (let ((title (or ytdious-channel ytdious-search-term)))
    (setq tabulated-list-format
          `[("Date" 10 t)
            ("Author" ,ytdious-video-author-width t)
            ("Length" 8 t)
	    ("Title" ,ytdious-video-title-width t)
            ("Views" 10 nil . (:right-align t))])
    (unless offline
      (setq ytdious-videos
            (funcall
             (if ytdious-channel #'ytdious--query-channel #'ytdious--query)
             title)))
    (let* ((title-string
            (propertize
             (apply #'format "[%s: %s]"
                    (if ytdious-channel
                        (list "CHAN" (alist-get 'author (elt ytdious-videos 0)))
                      (list "SRCH" title)))
             'face 'ytdious-video-published-face))
           (new-buffer-name (format "*ytdious %s*" title-string)))
      (if (get-buffer new-buffer-name)
          (switch-to-buffer (get-buffer-create new-buffer-name))
        (rename-buffer new-buffer-name)))
    (setq ytdious-mode-line-info
	  (concat "page:" (number-to-string ytdious-current-page)
		  " date:" ytdious-date-criterion
		  " sort:" (pcase ytdious-sort-criterion
			     ("upload_date" "date")
			     ("view_count" "views")
			     (_ ytdious-sort-criterion))
		  " "))
    (setq tabulated-list-entries
          (cl-loop for video across (if ytdious-sort-reverse
					(reverse ytdious-videos)
				      ytdious-videos)
		   for id = (alist-get 'videoId video)
		   when id collect
		   (list id
			 (vector
			  (ytdious--format-video-published
			   (alist-get 'published video))
			  (ytdious--format-author (alist-get 'author video))
			  (ytdious--format-video-length
			   (alist-get 'lengthSeconds video))
			  (alist-get 'title video)
			  (ytdious--format-video-views
			   (alist-get 'viewCount video))))))

    (tabulated-list-init-header)
    (tabulated-list-print)))

(defun ytdious--query (string)
  "Query YouTube for STRING."
  (ytdious-api
   "search"
   :params
   `(("q" ,string)
     ("date" ,ytdious-date-criterion)
     ("sort_by" ,ytdious-sort-criterion)
     ("page" ,ytdious-current-page))))

(defun ytdious--query-channel (string)
  "Show YouTube channel STRING."
  (ytdious-api (concat "channels/videos/" string)))

(defvar ytdious-search-history nil
  "History list of `ytdious' searches.")

;;;###autoload
(defun ytdious-search (query)
  "Search YouTube for QUERY."
  (interactive (list (read-string "Search terms: " nil
				  'ytdious-search-history nil t)))
  (setq ytdious-current-page 1)
  (setq ytdious-search-term query)
  (setq ytdious-channel nil)
  (ytdious--draw-buffer))

;;;###autoload
(defun ytdious-search-region ()
  "Search YouTube for marked region."
  (interactive)
  (let* ((query
          (buffer-substring-no-properties
           (region-beginning)
           (region-end)))
         (ytdious-search-term query))
    (funcall ytdious-enter-buffer-function (ytdious-buffer))
    (ytdious-search query)))

(defun ytdious-search-recent ()
  "Start a search YouTube with recent search string.
Mostly this is useful to return from a channel view back to search overview"
  (interactive)
  (ytdious-search
   (read-string "Search terms: " ytdious-search-term
		'ytdious-search-history nil t)))

(defvar ytdious-channel-history nil
  "History list of channels searched in `ytdious'.")

;;;###autoload
(defun ytdious-view-channel (channel)
  "Browse YouTube channel CHANNEL."
  (interactive (list (read-string "Channel: " nil 'ytdious-channel-history)))
  (setq ytdious-current-page 1)
  (setq ytdious-channel channel)
  (ytdious--draw-buffer))

(defun ytdious-view-channel-at-point ()
  "Browse YouTube channel of the author of video at point."
  (interactive)
  (setq ytdious-current-page 1)
  (setq ytdious-channel
        (alist-get 'authorId (ytdious-get-current-video)))
  (ytdious--draw-buffer))

(defun ytdious-display-full-title ()
  "Print full title in the echo area."
  (interactive)
  (message "\n%s\n" (alist-get 'title (ytdious-get-current-video))))

(defun ytdious-rotate-sort (&optional reverse)
  "Rotate through sort criteria.
Optional argument REVERSE reverses the direction of the rotation."
  (interactive)
  (setq ytdious-sort-criterion
        (funcall (if reverse #'ring-previous #'ring-next)
                 ytdious-sort-options ytdious-sort-criterion))
  (ytdious--draw-buffer))

(defun ytdious-rotate-sort-backwards ()
  "Rotate through sorting backwards."
  (interactive)
  (ytdious-rotate-sort t))

(defun ytdious-rotate-date (&optional reverse)
  "Rotate through date limit.
Optional argument REVERSE reverses the direction of the rotation."
  (interactive)
  (setq ytdious-date-criterion
        (funcall (if reverse #'ring-previous #'ring-next)
                 ytdious-date-options ytdious-date-criterion))
  (ytdious--draw-buffer))

(defun ytdious-rotate-date-backwards ()
  "Rotate through date limit backwards."
  (interactive)
  (ytdious-rotate-date t))

(defun ytdious-search-next-page ()
  "Switch to the next page of the current search."
  (interactive)
  (cl-incf ytdious-current-page)
  (ytdious--draw-buffer))

(defun ytdious-search-previous-page ()
  "Switch to the previous page of the current search."
  (interactive)
  (when (> ytdious-current-page 1)
    (cl-decf ytdious-current-page)
    (ytdious--draw-buffer)))

(defun ytdious-get-current-video ()
  "Get the currently selected video."
  (when-let ((id (tabulated-list-get-id)))
    (cl-find id ytdious-videos
	     :test #'string=
	     :key (lambda (video) (alist-get 'videoId video)))))

(defun ytdious-buffer (&optional new)
  "Return an existing or new (with NEW non-nil) `ytdious' buffer."
  (cl-flet ((new-buffer ()
              (with-current-buffer (generate-new-buffer "*ytdious*")
                (ytdious-mode)
                (current-buffer))))
    (if new (new-buffer)
      (if-let ((buffers
		(cl-loop for buffer in (buffer-list)
                         if (string-prefix-p "*ytdious" (buffer-name buffer))
                         collect buffer)))
          (if (cdr buffers)
              (completing-read "ytdious buffer: " buffers)
            (car buffers))
        (new-buffer)))))

;;;###autoload
(defun ytdious (&optional new)
  "Enter ytdious.
With NEW (interactively, with a prefix argument), create new `ytdious'
buffer even when other exist."
  (interactive "P")
  (funcall ytdious-enter-buffer-function (ytdious-buffer new)))

(cl-defun ytdious-api (path &key fields params)
  "Perform a call to the Invidious API.
PATH is the query URL path (without the leading and trailing
slash).  PARAMS is a URL parameter alist passed to
`url-build-query-string'.  FIELDS can override `ytdious-fields'.
curl(1) is used to perform the request.  An error is signaled if
it exits with a non-zero exit code, otherwise the request body is
parsed by `json-read' and returned."
  (with-temp-buffer
    (let ((exit-code
           (call-process
            "curl" nil t nil
	    "-L"
            "--silent"
            "-X" "GET"
            (concat
	     ytdious-invidious-api-url "/api/v1/" path
             (concat "?" (concat "fields=" (or fields ytdious-fields))
		     (when params
		       (concat "&" (url-build-query-string params))))))))
      (unless (= exit-code 0)
        (error "Curl exited with code %d when querying Invidious" exit-code))
      (goto-char (point-min))
      (condition-case c
          (json-read)
        (t (error
            "`json-read' error when parsing data from Invidious: %S" c))))))

(provide 'ytdious)
;;; ytdious.el ends here
