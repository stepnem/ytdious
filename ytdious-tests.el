(require 'ert)
(require 'ytdious)

(ert-deftest ytdious--format-video-length-test ()
  "Test `ytdious--format-video-length'."
  (should (equal (ytdious--format-video-length 60)
		 "00:01:00"))
  (should (equal (ytdious--format-video-length 72)
		 "00:01:12"))
  (should (equal (ytdious--format-video-length 134)
		 "00:02:14"))
  (should (equal (ytdious--format-video-length 3600)
		 "01:00:00"))
  (should (equal (ytdious--format-video-length 5100)
		 "01:25:00"))
  (should (equal (ytdious--format-video-length 5430)
		 "01:30:30")))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
