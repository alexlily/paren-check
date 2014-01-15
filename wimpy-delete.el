;; wimpy-delete -- a cautious form of kill-region.
;; Copyright (C) Bard Bloom, June 1989
;; bard@theory.lcs.mit.edu
   ; sup yo.
;; This file is not yet part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;; WHAT THIS DOES:

;; It's a variant of kill-region (c-w) which, if the region is too big
;; (default 100 characters), asks you if you really want to delete it.  The
;; prompt gives a couple words at each end, and tells you how big the region
;; is.

;; There's a related command, describe-region, which simply messages a few
;; words at each end of the region, and the total size.

(require 'cl)

(defvar wimpy-delete-size 500
   "kill-region-wimpy will ask if you want to delete more than 
this many characters.  Setting it to nil inhibits this feature altogether.")

(defvar wimpy-delete-dopey-message "Gosh, that was a narrow escape!"
  "Message that kill-region-wimpy prints when you tell it not to \"
delete the region.")

(setq wimpy-delete-query-list
  '(((?y ?Y ?\  ?\r) kill     "Yes		" nil)
    ((?n ?N ?\C-?)   nil   "No		" nil)
    ((?c) copy "Copy region	" nil)
    ((?r)	(recursive-edit nil) "Recursive Edit	" nil)
    ((?a)            (keyboard-quit nil)  "Abort		" nil)
    ))

(defun kill-region-wimpy (beg end)
  "Like kill-region, this deletes the text between BEG and END, and stuffs it
in the kill ring.  (Interactively, uses the region.) 
However, it asks you if you really want to delete a large chunk of text.
"
  (interactive "*r")
  (let ((size (- end beg))
        action
        )
    (cond
     ((and wimpy-delete-size 
           (> size wimpy-delete-size))
      (setq action
            (one-char-question
             (region-description (- (screen-width) 6) "Delete `"  "'?")
             wimpy-delete-query-list
             "Delete the region?")))
     (t (setq action 'kill)))
    (case action
      (kill  (kill-region beg end))
      (copy  (copy-region-as-kill beg end)
             (message "I copied the region!"))
      (t     (message "%s" wimpy-delete-dopey-message)))))        


(defun copy-region-as-kill-wimpy (beg end)
  "Copy region into the kill-ring, and describe it as well."
  (interactive "r")
  (message
   "Taking %s"
   (region-description (- (screen-width) 8) nil nil beg end))
  (copy-region-as-kill beg end))



(defun describe-region ()
  "Gives a message briefly describing the text at the beginning and end 
of the region."
  (interactive)
  (message
   "%s" (region-description (- (screen-width) 2))))

(defun absdiff (m n) (if (< m n) (- n m) (- m n)))


(defun region-description (width &optional prefix suffix begin end) 
  "Returns a string containing a one-line description of the region.
Required argument WIDTH tells how the string can be; there's a lower
limit of about 20 characters.
Optional arguments:
  PREFIX and SUFFIX are strings to tack on the ends of the message.
     They *do* count towards its length. Defaults: null strings both of them.
  BEGIN and END are the beginning and end of the region to use, if it's
     not the real region."
  (unless prefix (setq prefix ""))
  (unless suffix (setq suffix ""))
  (when (and begin (not end)) (setq end (point)))
  (cond
   (begin
    (psetq begin (min begin end)
           end   (max begin end)))
   (t
    (setq begin (min (point) (mark))
          end   (max (point) (mark)))))
;  (message "%d `%s' `%s' %d %d" width prefix suffix begin end)
  (cond
   ((< (- end begin) (- width 2))
    (concat "\"" (buffer-substring begin end) "\""))
   (t
    (let*
        ((chars-string (format "...<%d chars>..." (- end begin)))
         (space-for-messages
          (+ (length prefix)
             (length suffix)
             (length chars-string)))
         (space-for-quote (/ (max 0 (- width space-for-messages)) 2))
         (beg-words (pick-some-words begin nil space-for-quote))
         (end-words (pick-some-words end   t   space-for-quote))
         )
      (concat prefix beg-words chars-string end-words suffix)
      )));let*,cond
)

(defun pick-some-words (pos direction limit)
  "Arguments  POS DIRECTION LIMIT.  Returns a string of 
at most LIMIT chars, with one end at POS.  It will try to fit as many words 
into the string as it can.  If it can't fit even one word, it will take 
LIMIT characters.  DIRECTION = nil for forward, t for backward."
  (save-excursion
    (goto-char pos)
    (let (p q)
      (if direction (backward-word 1) (forward-word 1))
      (if (> (absdiff (point) pos) limit)
          (buffer-substring pos (+ (if direction (- limit) limit) pos))
        (setq p (point) q t)
        (while (and q (not (eobp)) (not (bobp)))
          (if direction (backward-word 1) (forward-word 1))
          (if (<= (absdiff (point) pos) limit)
              (setq p (point))
            (goto-char p)
            (setq q nil)))
        (buffer-substring pos (point))))))
                

             


(defun one-char-question (question chars &optional help case-sensitive)
  "Ask user a question, expecting a one-character answer.
The question is a string QUESTION.  The answer must be one of 
CHARS, which is a list of lists of the form:
  (
    ((a b .. d) result1 help1 confirm1)
    ((e f .. g) result2 help2 confirm2)
    ...
  )
  
  result1 is a value for one-char-question to return if the user a or b or .. d.
    atoms are simply returned.
    If result1 is a list of the form (fun returnp) 
       then one-char-question calls fun with no arguments.
            (If it's a symbol, it calls its function)
       If returnp is true then one-char-question returns fun's value,
       Else one-char-question asks the question again.
  help1 is a string to print as the meaning of a, b, ... d
  confirm1 is t if an answer of a, b, ..., d should be confirmed
    via y-or-n-p.  

Special characters do special things (and cannot be overridden)
  ? prints a help screen
  c-r enters a recursive edit
  c-l refreshes the screen

Optional arguments:
HELP is inserted at the top of the help listing.
if CASE-SENSITIVE is true then the command is case sensitive.
"

  (save-window-excursion
    (let ((answered nil)
          char-analysis
          answer should-confirm
          c
          )
      (while (not answered)
        (message "%s" question)
        (setq c (read-char))
        (unless case-sensitive (setq c (downcase c)))
        (cond
         ((= c ?\C-r)
          (save-window-excursion
            (save-excursion
              (save-restriction
                (message "Entering a recursive edit...")
                (widen)
                (recursive-edit)))))
         ((= c ?\C-l)
          (recenter))
         ((= c ??)
          (one-char-question-help question chars help))
         ((progn
            (setq char-analysis (one-char-question-answer c chars))
            (first char-analysis))
          (setq answered (second char-analysis)
                answer (third char-analysis)
                should-confirm (fourth char-analysis)
                help (fifth char-analysis)
                )
          )
         (t (beep)))
        ;; now confirm it (maybe)
        (when should-confirm
          (setq answered
                (y-or-n-p
                 (format "Really %s?" help)))))
      answer)))

(defun one-char-question-help (question chars help)
  (with-output-to-help-buffer (concat "*" question "*")
   (princ help)
   (terpri)
   (dolist (x chars)
     (princ
      (format "%s: %s\n"
              (third x)
              (mapconcat 'char-to-string (first x) ", ")
              )))))

(defun one-char-question-answer (c chars)
  "Internal function for one-char-question.  Just keeping the form
of whilst violating the spirit.  Dont' ask what this does.  But if 
you must know, C is the character the user typed, 
CHARS is the chars argument ot one-char-question, and 
this thing returns:
  (true-if-C-is-one-of-the-chars-in-CHARS
   answered=t-if-one-char-question-should-be-satisfied
   result-for-ocq-to-return
   true-if-ocq-shoudl-ask-for-confirmation
   help-message)
"
  (let ((ocqa nil)
        result result-result
        (answered t)
        )
    (dolist (x chars)
      (when (memq c (first x))
        (setq result (second x))
        (when (consp result)
          (setq result-result (funcall (if (symbolp (first result))
                                           (symbol-function (first result))
                                         (first result))))
          (if (second result)
              (setq result result-result)
            (setq answered nil)))
        (setq ocqa
              (list
               t
               answered
               result
               (fourth x)
               (third x)))
        ))
    ocqa))

(defvar y-or-n-p-list
  '(((?y ?Y ?\  ?\r) t     "Yes		" nil)
    ((?n ?N ?\C-?)   nil   "No		" nil)
    ((?r)            (recursive-edit nil) "Recursive Edit	" nil)
    ((?a)            (keyboard-quit nil)  "Abort		" nil)
    )
  "List to make one-char-question act like y-or-n-p")

;(one-char-question "Yes or no?" y-or-n-p-list "Stuff")
  
