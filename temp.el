(global-set-key "\C-c'" 'put-before-line)
(global-set-key "\C-c\"" 'cursorless-clear-overlays)

(defun copy-cons (x)
  (cons (car x) (cdr x)))

(defun put-before-line ()
  (interactive)
  (cursorless-clear-overlays)
  (let* ((pos (save-excursion (beginning-of-line) (point)))
         (overlay (make-overlay pos pos))
         (text (copy-sequence "x\n"))
         (face '((:background "coral"))) ;; debugging
         (image (let* ((w (window-font-width))
                       (r (/ w 5.0))
                       (h (* w (/ 55 89.0)))
                       (svg (svg-create w h)))
                  ;(svg-circle svg (/ w 2.0) (* h (/ 4 7.0)) r)
                  (svg-circle svg (/ w 2.0) (* h 0.5) r)
                  (svg-image svg :scale 1)))
         (image (let* ((w (window-font-width))
                       (r (/ w 5.0))
                       (h (* w (/ 55 89.0)))
                       (cols 100)
                       (svg (svg-create (* w cols) h)))
                  (cl-do ((i 0 (1+ i))) ((= i cols))
                    (svg-circle svg (+ (* w i) (/ w 2.0)) (* h 0.5) r))
                  (svg-image svg :scale 1))))
    ;; make the line height as short as possible
    (put-text-property 0 (length text) 'line-height t text)
    (put-text-property 0 (length text) 'face face text)
    (cl-do ((i 0 (1+ i))) ((= (1+ i) (length text)))
      (put-text-property i (1+ i) 'display (copy-cons image) text))
    (overlay-put overlay 'cursorless t)
    (overlay-put overlay 'before-string text)))

abcd
abcd

(seq-do-indexed (lambda (x i) (message "%s %s" x i)) "foo")

(cl-do ((i 0 (1+ i))) ((< i 5))
  (message i))
