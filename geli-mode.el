;;; geli-mode.el -*- lexical-binding: t; -*-

;; Function to customize the time format
(defvar geli-time-format "%-I:%M %p"
  "Time format for geli-mode headings.")

;; 'type-of-work' and 'created-timestamp' should be stored in buffer-local variables because it needs to pass to other functions when needed.
(defvar-local geli-current-type nil
  "Current type of work for the latest geli heading.")

(defvar-local geli-current-created-timestamp nil
  "Creation timestamp for the latest geli heading.")


;;; Function to control properties section syntax
;; Define the faces:
(defface geli-properties-key-face
  '((t :foreground "green"))
  "Face for keys in the properties section of geli-mode.")

(defface geli-properties-boundary-face
  '((t :foreground "red" :weight bold))
  "Face for the start and end of the properties section in geli-mode.")

(defface geli-emergency-face
  '((t :foreground "white" :background "red" :weight bold :height 1.2))
  "Face for Emergency headings in geli-mode.")

(defface geli-journal-face
  '((t :foreground "midnight blue" :background "light sky blue" :weight normal :height 1.1))
  "Face for Journal headings in geli-mode.")

(defface geli-database-face
  '((t :foreground "black" :background "light green" :weight normal :height 1.1))
  "Face for Database Insertion headings in geli-mode.")

(defface geli-epiphany-face
  '((t :foreground "dark violet" :background "lavender" :weight normal :height 1.1))
  "Face for Epiphany headings in geli-mode.")

(defface geli-scratch-face
  '((t :foreground "dark orange" :background "peach puff" :weight normal :height 1.1))
  "Face for Scratch Pad headings in geli-mode.")

(defface geli-learning-face
  '((t :foreground "royal blue" :background "light yellow" :weight normal :height 1.1))
  "Face for Learning headings in geli-mode.")

(defface geli-title-face
  '((t (:foreground "green" :background "gray" :weight bold :height 1.2)))
  "Face for title in geli-mode headings.")

(defface geli-display-time-face
  '((t (:foreground "blue" :background "yellow" :weight bold :height 1.2)))
  "Face for display time in geli-mode headings.")


(defun geli-apply-highlighting (string face)
  "Apply highlighting to a string using a given face."
  (propertize string 'face face))

(defvar geli-display-time-format "%-I:%M %p"
  "Time format for displaying in geli-mode headings.")

(defvar geli-internal-time-format "%Y-%m-%d %H:%M:%S"
  "Time format for internal calculations and database entries in geli-mode.")
;;;; Data Structures
(require 'cl-lib)

;;; Heading Structure - Start
;; Heading structure might include elements like the time, title, and the type of heading (e.g., "Emergency", "Journal", etc).
(cl-defstruct geli-heading
  time   ; Time when the heading was created
  title  ; Title of the heading
  type)  ; Type of heading (e.g., "Emergency", "Journal")

;;; Properties Structure
;; Properties structure can store additional information associated with a heading, such as creation timestamp, type of work, and time spent.
(cl-defstruct geli-properties
  created-on-timestamp ; Time when the heading or content was created
  ended-on-timestamp   ; Time when the activity ended
  type-of-work         ; Type of work or category
  time-spent           ; Time spent on the associated task or entry
  total-time)          ; Total time spent on the task
;; You can now create instances of heading and properties with specific attributes.
;; (setq my-heading (make-geli-heading :time (current-time)
;; :title "My First Heading"
;; :type "Journal"))
;; OR
;; (setq my-properties (make-geli-properties :created-on-timestamp (current-time)
;; :ended-on-timestamp (current-time)
;; :time-spent "1 hour"))
;;;; Data Sturucture - End

;;; geli-insert-properties
(defun geli-insert-heading ()
  "Insert a custom heading."
  (interactive)
  (let* ((title (read-string "Enter title: "))
         (options '("Emergency" "Start Journalling" "Insert to database" "Epiphany" "Scratch pad" "Learning"))
         (option (completing-read "Select option: " options))
         (display-time (format-time-string geli-display-time-format))
         (created-timestamp (format-time-string geli-internal-time-format))
         (formatted-time (propertize display-time 'face 'geli-display-time-face))
         (formatted-title (propertize title 'face 'geli-title-face)))
    (setq geli-current-type option)  ;; Save the option to buffer-local
    (setq geli-current-created-timestamp created-timestamp)
    ;; Insert the formatted time and title
    (insert (format "%s - %s\n" formatted-time formatted-title))))


;;; geli-insert-properties
(defun geli-insert-properties ()
  "Insert properties for a heading."
  (interactive)
  (let* ((ended-timestamp (format-time-string geli-internal-time-format)) ; End time is current time in internal format
         (time-spent (geli-calculate-time-spent geli-current-created-timestamp ended-timestamp))
         (properties (make-geli-properties :created-on-timestamp geli-current-created-timestamp
                                           :ended-on-timestamp ended-timestamp
                                           :type-of-work geli-current-type
                                           :time-spent time-spent
                                           :total-time time-spent)))
    (insert ":PROPERTIES:\n")
    (insert (format "created_on_timestamp: %s\n" (geli-properties-created-on-timestamp properties)))
    (insert (format "ended_on_timestamp: %s\n" (geli-properties-ended-on-timestamp properties)))
    (insert (format "type_of_work: %s\n" (geli-properties-type-of-work properties)))
    (insert (format "time_spent: %s\n" (geli-properties-time-spent properties)))
    (insert ":END:\n\n")))

(defun geli-calculate-time-spent (start-time end-time)
  "Calculate the time spent between start-time and end-time."
  (format-seconds "%Y, %D, %H, %M, %z"
                  (time-subtract (date-to-time end-time) (date-to-time start-time))))


;; Define the major mode
(define-derived-mode geli-mode fundamental-mode "Geli"
  "Major mode for editing .geli files."

  ;; Enable outline mode for folding
  (outline-minor-mode 1)
  (setq outline-regexp ".*:$")  ; Set the outline regex to match your headings  ;; Set key bindings

  ;; Set up key bindings
  ;; (define-key geli-mode-map [M-return] 'geli-insert-heading)
  (define-key geli-mode-map (kbd "C-c h") 'geli-insert-heading)
  (define-key geli-mode-map (kbd "C-c p") 'geli-insert-properties))

(add-to-list 'auto-mode-alist '("\\.geli\\'" . geli-mode))
