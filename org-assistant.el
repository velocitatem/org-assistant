;;; org-assistant.el --- org-assistant is a tool to help you build your org-agenda dynamically. It takes the events for each day, aswell as their category, generates a org-assistant-timeline and them implements that into your org agenda. -*- lexical-binding: t -*-

    ;; Author: Daniel Rosel
    ;; Maintainer: Daniel Rosel
    ;; Version: 0.1.0
    ;; Package-Requires: (cl)
    ;; Homepage: https://github.com/velocitatem/org-assistant
    ;; Keywords: org-agenda org-mode schedule


    ;; This file is not part of GNU Emacs

    ;; This program is free software: you can redistribute it and/or modify
    ;; it under the terms of the GNU General Public License as published by
    ;; the Free Software Foundation, either version 3 of the License, or
    ;; (at your option) any later version.

    ;; This program is distributed in the hope that it will be useful,
    ;; but WITHOUT ANY WARRANTY; without even the implied warranty of
    ;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    ;; GNU General Public License for more details.

    ;; You should have received a copy of the GNU General Public License
    ;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:


;;; Code:


(defvar org-assistant-files-path (first org-agenda-files))


(defun its (i)
  (int-to-string i))

(defun read-new-event ()
  (let ((event-name (read-string "Enter Event Name: ")))
    (progn
      (let ((event-time (read-string (concat "When does \"" event-name "\" happen? (hh:mm) ")))
            (event-duration (read-string (concat "How long does \"" event-name "\" take? (minutes) "))))
        (list event-name event-time event-duration)
        ))
    )
)


(defun str-to-time (_str)
  (let ((time (split-string _str ":")))
    (list (cl-parse-integer (first time)) (cl-parse-integer (second time))))
  )


(defun add-minutes-to-time (time minutes)
  (let ((hours-from-minutes (truncate (/ minutes 60)))
        (remaining-minutes (% minutes 60)))
    (list  (+ hours-from-minutes (first time)) (+ remaining-minutes (second time))))
  )


(defun wild-events/add (user-event)
  (setq org-assistant-wild-events
        (cons user-event org-assistant-wild-events)) ; TODO Maybe refactor this
  )

(defun timed-events/add (user-event)
  (setf (nth 1 user-event) (str-to-time (nth 1 user-event)))
  (setq org-assistant-timed-events
        (cons user-event org-assistant-timed-events))
  )

(defun timed-events/remove-item (event)
  (setq org-assistant-timed-events
        (remove event org-assistant-timed-events)
        )
  )

(defun compare-times (a b)
  (if (< (first a) (first b))
      a
      (progn
          (if (> (first a) (first b))
              b
              (progn
                  (if (< (second a) (second b))
                      a
                      b)))
      )))


(defun subtract-times (a b)
  (list (- (first a) (first b))
   (- (second a) (second b))))


(defun add-z (n)
  (if (< n 10 )
      (concat "0" (its n))
      (its n)))


(defun stamp-time (time)
  (format-time-string (concat
   "SCHEDULED: <%Y-%m-%d %a "
   (add-z (first time)) ":"
   (add-z (second time)) ">"))
   )

(defun orgify-event (event)
  (concat "* TODO " (first event) "\n" (stamp-time (second event))))

(defun generate-org-markdown (tline) ;tline is org-assistant-timeline
  (let ((agenda-string ""))
    (progn
      (cl-loop for event in tline do
               (setq agenda-string (concat agenda-string "\n" (orgify-event event))))
      agenda-string
      ))
)



;; dont feel like optimizing this too much lol
(defun org-assistant/generate-timeline ()
  (interactive)

  (setq org-assistant-user-events (list (list "Start" "09:00" "0"))
        org-assistant-timeline (list))

                                        ; SAMPLE INPUT
  ;; (setq user-events
  ;;       (list (list "Start" "09:00" "0")
  ;;             (list "Dog" "" "30")
  ;;             (list "Postoffice" "14:00" "30")
  ;;             (list "Exercise" "10:00" "45")
  ;;             )
  ;;       )


  (setq keep-reading t)
  (read-string "Let's get started with building your day [Enter to continue]")

                                        ; get users user-event
  (while keep-reading
    (if (not (equal (read-char "Add new event? [y/enter]: ") 121)) ;; 121 is char code for y
        (setq keep-reading nil)
      (setq org-assistant-user-events
            (cons (read-new-event) org-assistant-user-events)))
    )
  (setq keep-reading t)


  (setq org-assistant-timed-events (list)
        org-assistant-wild-events (list))

  (cl-loop for user-event in org-assistant-user-events do
        (if (not (= 0 (length (second user-event))))
            (timed-events/add user-event)
          (wild-events/add user-event)
          )
        )


  ; time specific asignment
  (while (> (length org-assistant-timed-events) 0)
    (setq min-time (str-to-time "23:59")
          min-event nil)
    (cl-loop for event in org-assistant-timed-events do
          (if (equal (second event) (compare-times (second event) min-time))
              (setq min-time (second event)
                    min-event event)))
    (timed-events/remove-item min-event)
    (setq org-assistant-timeline
          (cons min-event org-assistant-timeline))
    )


  (setq org-assistant-timeline (reverse org-assistant-timeline))

  ;; wildcard assignment
  ;; This could be a let
  (setq org-assistant-last-wildcard 0
        org-assistant-event-index 0)
  (cl-loop for event in org-assistant-timeline do
        (if (< org-assistant-event-index (- (length org-assistant-timeline) 1)) ; checking not to go over the last element since we need a +1 index
            (progn
              (message (first event))
              (setq next-event (nth (+ org-assistant-event-index 1) org-assistant-timeline) ; get next event
                    time-gap (subtract-times (second next-event) (second event)) ; calculate gap between events start-start
                    time-gap (+ (* (first time-gap) 60) (second time-gap) )
                    event-end-shift (+ (cl-parse-integer (third event)) 5)) ; find then event 1 ends and add 5 minutes
              (setq time-gap (- time-gap event-end-shift) ; subtract event duration from start-start time gap
                    selected-wildcard (nth org-assistant-last-wildcard org-assistant-wild-events))
              (if (and (not (equalp selected-wildcard nil)) (< (cl-parse-integer (third selected-wildcard)) time-gap) ) ; check if wildcard duration could fit into gap
                  (progn
                    (message "gap found") 
                    (setf (nth 1 selected-wildcard) (add-minutes-to-time (second event) event-end-shift)) 
                    (setq org-assistant-timeline
                          (-insert-at (+ 1 org-assistant-event-index) selected-wildcard org-assistant-timeline)
                          )
                    (setq org-assistant-last-wildcard (+ 1 org-assistant-last-wildcard))))

              (setq org-assistant-event-index (+ org-assistant-event-index 1)))
        ))
  (generate-org-markdown org-assistant-timeline) ;; last step

  )

(defun org-assistant/setup ()
  ;; TODO Define directory, add directory to org-agenda-files
  )

(defun org-assistant/export-to-file (markdown)
  (let ((file-name (concat (format-time-string "oas-%Y%m%d-%H%M") ".org")))
    (progn
      ;; this feels wrong
      (generate-new-buffer file-name)
      (switch-to-buffer-other-window file-name)
      (insert markdown)
      (write-file file-name)
      ))
  )
(org-assistant/export-to-file (generate-org-markdown org-assistant-timeline))


(provide 'org-assistant)
;;; org-assistant.el ends here(i)
