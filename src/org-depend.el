;;; org-depend.el --- Express dependencies between arbitrary headlines.

;; Copyright (C) 2016 Alexander Baier

;; Author: Alexander Baier <alexander.baier@mailbox.org>
;; Homepage: http://example.com/foo

;; Version: 0.0.1

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'dash)
(require 'dash-functional)
(require 'org)
(require 'org-id)

(defconst org-depend-property "DEPEND")
(defconst org-depend-symbol (intern (concat ":" org-depend-property)))

(defun org-depend--db-init ()
  (cons (make-hash-table :test 'equal) (make-hash-table :test 'equal)))

(defvar org-depend-db (org-depend--db-init))

(defvar org-depend-files org-refile-targets
  "Files scanned for dependant headlines in the format of `org-refile-targets'.")

;;; inner helper functions
(defun org-depend--db-put (id deps db) (puthash id deps (car db)))
(defun org-depend--get-update-time (file db) (gethash file (cdr db)))
(defun org-depend--put-update-time (time file db) (puthash file time (cdr db)))

(defun org-depend--get-depend-files ()
  (-flatten
   (-map
    (-lambda ((fn-or-list . _))
      (cond
       ((listp fn-or-list) fn-or-list)
       ((functionp fn-or-list) (let ((file (funcall fn-or-list)))
                                 (if (listp file) file (list file))))
       (t (error "Car of each cons in `org-depend-files' must be a function or a list"))))
    org-depend-files)))

(defun org-depend--get-dependencies (pom &optional buffer)
  (cl-assert (integer-or-marker-p pom))
  (cl-assert (bufferp buffer))
  (let* ((pos (if (markerp pom) (marker-position pom) pom))
         (buf (if (markerp pom) (marker-buffer pom) buffer)))
    (with-current-buffer buf
      (org-element-at-point)
      (org-entry-get-multivalued-property pos org-depend-property))))

(defun org-depend--add-dependency (dep-id pom &optional buffer)
  (cl-assert (stringp dep-id))
  (cl-assert (integer-or-marker-p pom))
  (unless (markerp pom) (cl-assert (bufferp buffer)))
  (let* ((pos (if (markerp pom) (marker-position pom) pom))
         (buf (if (markerp pom) (marker-buffer pom) buffer)))
    (with-current-buffer buf
      (org-element-at-point)
      (org-entry-add-to-multivalued-property pos org-depend-property dep-id))))

(defun org-depend--remove-dependency (dep-id pom &optional buffer)
  (cl-assert (stringp dep-id))
  (cl-assert (integer-or-marker-p dep-id))
  (unless (markerp pom) (cl-assert (bufferp buffer)))
  (let* ((pos (if (markerp pom) (marker-position pom) pom))
         (buf (if (markerp pom) (marker-buffer pom) buffer)))
    (with-current-buffer buf
      (org-element-at-point)
      (org-entry-remove-from-multivalued-property pos org-depend-property dep-id))))

(defun org-depend--pomoi-to-id (pomoi &optional create)
  (pcase pomoi
    ((pred integer-or-marker-p) (org-id-get pomoi create))
    ((pred stringp)             pomoi)
    (`nil                       (org-id-get (point) create))
    (_                          nil)))

(defun org-depend--pomoi-to-id-check (param pomoi &optional create)
  (-if-let (id (org-depend--pomoi-to-id pomoi create))
      id
    (error (concat param " must be an integer, marker, string or nil"))))

(defun org-depend--get-id-with-outline-path-completion (&optional targets)
  "Use `outline-path-completion' to retrieve the ID of an entry.
TARGETS may be a setting for `org-refile-targets' to define
eligible headlines.  When omitted, all headlines in the current
file are eligible.  This function returns the ID of the entry.
If necessary, the ID is created."
  (let* ((org-refile-targets (or targets '((nil . (:maxlevel . 10)))))
         (org-refile-use-outline-path nil)
         (org-refile-target-verify-function nil)
         (spos (org-refile-get-location "Entry" nil nil 'no-excludes))
         (pom (and spos (move-marker (make-marker) (nth 3 spos)
                                     (get-file-buffer (nth 1 spos))))))
    (prog1 (org-id-get pom 'create)
      (move-marker pom nil))))

;;; Dependency API
(defun org-depend-find (f h db)
  (let ((res (funcall f h)))
    (if res
        res
      (-when-let (dependencies (org-depend-db-get h db))
        (-some (lambda (h) (org-depend-find f h db)) dependencies)))))

(defun org-depend-compare (id1 id2 db)
  (let ((h1<h2 (org-depend-find (-partial #'equal id1) id2 db))
        (h1>h2 (org-depend-find (-partial #'equal id2) id1 db)))
    (cond
     ((eq h1<h2 h1>h2) 'equal)
     (h1>h2 'smaller)
     (h1<h2 'greater))))

(defun org-depend-db-get (id db)
  (gethash id (car db)))

(defun org-depend-db-get-transitive (id db)
  (-when-let (dependencies (org-depend-db-get id db))
    (append dependencies
            (-flatten
             (-map (-rpartial #'org-depend-db-get-transitive db)
                   dependencies)))))

(defun org-depend-sorting-strategy (a b)
  (let* ((ma (or (get-text-property 0 'org-marker a)
                 (get-text-property 0 'org-hd-marker a)))
         (mb (or (get-text-property 0 'org-marker b)
                 (get-text-property 0 'org-hd-marker b)))
         (id-a (org-id-get ma 'create))
         (id-b (org-id-get mb 'create)))
    (cl-case (org-depend-compare id-a id-b org-depend-db)
      ('equal nil)
      ('greater 1)
      ('smaller -1))))

;;; DB update functionality
(defun org-depend-update-db (file db)
  (with-current-buffer (find-file-noselect file)
    (org-map-entries
     (lambda ()
       (let* ((id (org-id-get (point)))
              (dependencies
               (and id
                    (org-depend--get-dependencies (point) (current-buffer)))))
         (when dependencies
           (org-depend--db-put id dependencies db)
           (org-depend--put-update-time (current-time) file db))))
     t 'file))
  db)

(defun org-depend-update-db-from-files (files db &optional force)
  (-map (lambda (file)
          (let ((last-update-time (or (org-depend--get-update-time file db)
                                      0))
                (mod-time (nth 5 (file-attributes file))))
            (when (or force
                      (time-less-p last-update-time mod-time))
              (org-depend-update-db file db))))
        files)
  db)

(defun org-depend-update-all-files (&optional force)
  (interactive "P")
  (let ((all-files (org-depend--get-depend-files)))
    (when (member (buffer-file-name) all-files)
      (org-depend-update-db-from-files all-files org-depend-db force))))

(defun org-depend-setup ()
  (add-hook 'after-save-hook #'org-depend-update-all-files))


;;; Capture onshot hook implementation
(defvar org-depend--capture-oneshot-functions nil
  "List of functions run by `org-depend--capture-hook-function'.")

(defconst org-depend--oneshot-hook-variable 'org-capture-prepare-finalize-hook)

(defun org-depend--capture-hook-function ()
  "Runs all functions in `org-depend--capture-oneshot-functions' and
then removes itself from `org-depend--oneshot-hook-variable'"
  (-map #'funcall org-depend--capture-oneshot-functions)
  (setq org-depend--capture-oneshot-functions nil)
  (remove-hook org-depend--oneshot-hook-variable #'org-depend--capture-hook-function))

(defun org-depend--capture-add-oneshot-hook (&rest fns)
  (setq org-depend--capture-oneshot-functions fns)
  (add-hook org-depend--oneshot-hook-variable #'org-depend--capture-hook-function))

;;; Graph functions
(defun org-depend--merge-graphs (graphs)
  (-reduce-from
   (-lambda ((vs1 . es1) (vs2 . es2))
             (cons (-union vs1 vs2) (-union es1 es2)))
   '(() . ())
   graphs))

(defun org-depend-graph (id db)
  (-if-let (deps (org-depend-db-get id db))
      (let ((edges (-map (-partial #'cons id) deps))
            (results (org-depend--merge-graphs
                      (-map (-rpartial #'org-depend-graph db) deps))))
        (cons (cons id (car results)) (append edges (cdr results))))
    (cons (list id) '())))

(defun org-depend-graphs (ids db)
  (org-depend--merge-graphs
   (-map (-rpartial #'org-depend-graph db) ids)))

(defun org-depend-get-headline-from-id (id)
  (with-current-buffer (find-file-noselect (car (org-id-find id)))
    (goto-char (cdr (org-id-find id)))
    (plist-get (cadr (org-element-at-point)) :title)))

(defun org-depend-graph-to-dot (graph)
  (let* ((edges (-map (-lambda ((from . to))
                        (cons (org-depend-get-headline-from-id from)
                              (org-depend-get-headline-from-id to))) (cdr graph)))
         (edges-dot (-map (-lambda ((from . to))
                            (format "\"%s\" -> \"%s\";" from to))
                          edges)))
    (s-join "\n"
            (append '("digraph org_dependencies {")
                    '("  size=\"8,8!\";")
                    (-map (-partial #'s-prepend "  ") edges-dot)
                    '("}")))))

;;; Commands
;;;###autoload
(defun org-depend-agenda-dependencies (&optional transitive id-or-pom)
  (interactive "P")
  (setq id-or-pom (org-depend--pomoi-to-id id-or-pom 'create))
  (unless id-or-pom
    (error "id-or-pom must be an integer, marker, string or nil"))
  (org-depend-update-db-from-files (org-depend--get-depend-files) org-depend-db)
  (with-current-buffer (find-file-noselect (car (org-id-find id-or-pom)))
    (goto-char (cdr (org-id-find id-or-pom)))
    (let* ((dependencies
            (if transitive
                (org-depend-db-get-transitive id-or-pom org-depend-db)
              (org-depend-db-get id-or-pom org-depend-db)))
           (org-agenda-overriding-header
            (format "Dependencies of: `%s'"
                    (plist-get (cadr (org-element-at-point)) :title)))
           (org-agenda-cmp-user-defined #'org-depend-sorting-strategy)
           (org-agenda-sorting-strategy '(user-defined-down))
           (org-agenda-files (list (buffer-file-name)))
           (org-agenda-skip-function
            (lambda () (if (member (org-id-get (point)) dependencies)
                           nil
                         (outline-next-heading)))))
      (org-tags-view nil "{.*}"))))

;;;###autoload
(defun org-depend-capture-dependency (&optional pomoi capture-keys)
  (interactive)
  (let* ((from (org-depend--pomoi-to-id-check "pomoi" pomoi 'create))
         (add-dependency-at-point
          (lambda ()
            (message "in-hook:\n  buffer: %S\n  point: %S" (current-buffer) (point))
            (unless (eq (car (org-element-at-point)) 'headline)
              (outline-back-to-heading 'invisible-ok))
            (org-depend-add-dependency from (point)))))
    (org-depend-update-db-from-files (org-depend--get-depend-files) org-depend-db)
    (org-depend--capture-add-oneshot-hook add-dependency-at-point)
    (org-capture nil capture-keys)))

;;;###autoload
(defun org-depend-add-dependency (&optional from to)
  (interactive "P")
  (setq to (if (called-interactively-p 'any)
               (org-depend--get-id-with-outline-path-completion nil)
             (org-depend--pomoi-to-id-check "to" to 'create)))
  (setq from (if (eq from '(4))
                 (org-depend--get-id-with-outline-path-completion nil)
               (org-depend--pomoi-to-id-check "from" from 'create)))
  (org-depend-update-db-from-files
   (org-depend--get-depend-files) org-depend-db)
  (org-depend--add-dependency to (org-id-find from 'as-marker)))

(declare-function image-transform-fit-to-height 'image-mode)
(declare-function image-transform-fit-to-width 'image-mode)
;;;###autoload
(defun org-depend-show-dependency-graph (&optional buffer)
  (interactive "bBuffer: ")
  (org-depend-update-db-from-files (org-depend--get-depend-files) org-depend-db)
  (let* ((ids (with-current-buffer (or buffer (current-buffer))
                (org-map-entries (lambda () (org-entry-get (point) "ID")))))
         (db (org-depend-update-db (buffer-file-name) org-depend-db))
         (graph (org-depend-graphs ids db))
         (dot (org-depend-graph-to-dot graph))
         (dot-file (with-temp-buffer
                     (insert dot)
                     (write-file (concat (make-temp-name "/tmp/org-depend-dot-") ".dot"))
                     (buffer-file-name)))
         (out-file (make-temp-file "/tmp/org-depend-dot-" nil ".png"))
         (ret-value (start-process
                     "dot" "*dot*" "/usr/bin/dot"
                     "-Tpng" (format "-o%s" out-file) dot-file)))
    (sit-for 5)
    (pop-to-buffer (find-file-noselect out-file))
    (image-transform-fit-to-height)
    (image-transform-fit-to-width)))

;;; test stuff
(defmacro org-depend-ignore-body (&rest body)
  nil)

(org-depend-ignore-body
 (setq org-depend-files
       (cons `(,(concat (file-name-directory (buffer-file-name)) "test.org") :maxlevel . 4)
             org-depend-files))
 )

(provide 'org-depend)
;;; org-depend.el ends here
