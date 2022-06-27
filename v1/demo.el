;;; org-assistant.el --- org-assistant is a tool to help you build your org-agenda dynamically. It takes the events for each day, aswell as their category, generates a timeline and them implements that into your org agenda. -*- lexical-binding: t -*-

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


(defun its 
  (int-to-string i))

(defun read-new-event ()
  (setq
   event-name (read-string "Enter Event Name: ")
   event-time (read-string (concat "When does \"" event-name "\" happen? (hh:mm) "))
   event-duration (read-string (concat "How long does \"" event-name "\" take? (minutes) "))
   )
  (list event-name event-time event-duration)
)


(setq user-events (list (list "Start" "09:00" "0"))
      timeline (list))

; SAMPLE INPUT
(setq user-events
      (list (list "Start" "09:00" "0")
            (list "Dog" "" "30")
            (list "Postoffice" "14:00" "30")
            (list "Exercise" "10:00" "45")
            )
      )


(setq keep-reading t)
(read-string "Let's get started with building your day [Enter to continue]")

; get users user-event
(while keep-reading
  (if (not (equal (read-string "Add new event? [y/enter]: ") "y"))
      (setq keep-reading nil)
      (setq user-events
            (cons (read-new-event) user-events)))
  )
(setq keep-reading t)

(defun str-to-time (str)
  (setq time (split-string str ":")
        hour (cl-parse-integer (first time))
        minute (cl-parse-integer (second time)))
  (list hour minute)
  )

(defun add-minutes-to-time (time minutes)
  (setq hours-from-minutes (truncate
                            (/ minutes 60))
        remaining-minutes (% minutes 60))
  (list  (+ hours-from-minutes (first time)) (+ remaining-minutes (second time)))
  )

(setq timed-events (list)
      wild-events (list))


(defun wild-events/add (user-event)
  (setq wild-events
        (cons user-event wild-events)) ; TODO Maybe refactor this
  )
(defun timed-events/add (user-event)
  (setf (nth 1 user-event) (str-to-time (nth 1 user-event)))
  (setq timed-events
        (cons user-event timed-events))
  )

(loop for user-event in user-events do
     (if (not (= 0 (length (second user-event))))
         (timed-events/add user-event)
         (wild-events/add user-event)
         )
     )

; helper
(defun timed-events/remove-item (event)
  (setq timed-events
        (remove event timed-events)
        )
  )

; helper
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


; time specific asignment
(while (> (length timed-events) 0)
  (setq min-time (str-to-time "23:59")
        min-event nil)
  (loop for event in timed-events do
        (if (equal (second event) (compare-times (second event) min-time))
            (setq min-time (second event)
                  min-event event)))
  (timed-events/remove-item min-event)
  (setq timeline
        (cons min-event timeline))
  )


(setq timeline (reverse timeline))

; wildcard assignment
(setq last-wildcard 0
      event-index 0)
(loop for event in timeline do
      (if (< event-index (- (length timeline) 1)) ; checking not to go over the last element since we need a +1 index
          (progn
            (message (first event))
            (setq next-event (nth (+ event-index 1) timeline) ; get next event
                  time-gap (subtract-times (second next-event) (second event)) ; calculate gap between events start-start
                  time-gap (+ (* (first time-gap) 60) (second time-gap) )
                  event-end-shift (+ (cl-parse-integer (third event)) 5)) ; find then event 1 ends and add 5 minutes
            (setq time-gap (- time-gap event-end-shift) ; subtract event duration from start-start time gap
                  selected-wildcard (nth last-wildcard wild-events))
            (if (and (not (equalp selected-wildcard nil)) (< (cl-parse-integer (third selected-wildcard)) time-gap) ) ; check if wildcard duration could fit into gap
                (progn
                  (message "gap found") 
                  (setf (nth 1 selected-wildcard) (add-minutes-to-time (second event) event-end-shift)) 
                  (setq timeline
                        (-insert-at (+ 1 event-index) selected-wildcard timeline)
                        )
                  (setq last-wildcard (+ 1 last-wildcard))))

            (setq event-index (+ event-index 1)))
      ))


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

(defun generate-org-markdown (tline) ;tline is timeline
  (setq agenda-string "")
  (loop for event in tline do
        (setq agenda-string (concat agenda-string "\n" (orgify-event event))))
  )



(generate-org-markdown timeline)


(defun org-assistant/generate-timeline ()
  
  )




;; SAMPLE OUTPUT
;; * TODO Start
;; SCHEDULED: <2022-06-27 Mon 09:00>
;; * TODO Dog
;; SCHEDULED: <2022-06-27 Mon 09:05>
;; * TODO Exercise
;; SCHEDULED: <2022-06-27 Mon 10:00>
;; * TODO Postoffice
;; SCHEDULED: <2022-06-27 Mon 14:00>"


(provide 'org-assistant)
;;; org-assistant.el ends here(i)
