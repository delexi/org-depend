(defvar org-depend-db (make-hash-table :test 'equal))

(defun org-depend-db-init () (setq org-depend-db (make-hash-table :test 'equal)))

(defun org-depend-get (id db) (cons id (gethash id db)))
(defun org-depend-put (h db) (puthash (car h) (cdr h) db))

(defun org-depend-plist-get-all (key plist)
  (funcall (-compose
            (-partial #'-keep (-lambda ((k v)) (when (eq k key) v)))
            (-partial #'-partition 2))
           plist))

(defun org-depend-update-from-file (file db)
  (with-current-buffer (find-file-noselect file)
    (org-element-map (org-element-parse-buffer 'headline) 'headline
      (lambda (h)
        (let* ((props (cadr h))
               (id (org-id-get (plist-get props :begin) 'create))
               (depends (org-depend-plist-get-all :DEPEND props)))
          (org-depend-put (cons id depends) db))))))

(defun org-depend-find (f h db)
  (let ((res (funcall f h)))
    (if res
        res
      (-when-let (depends (cdr (org-depend-get h db)))
        (-some (lambda (h) (org-depend-find f h db)) depends)))))

(defun org-depend-compare (id1 id2 db)
  (let ((h1<h2 (org-depend-find (-partial #'equal id1) id2 db))
        (h1>h2 (org-depend-find (-partial #'equal id2) id1 db)))
    (cond
     ((eq h1<h2 h1>h2) 'equal)
     (h1>h2 'smaller)
     (h1<h2 'greater))))

(defun org-depend--merge-graphs (graphs)
  (message "Merging:\n  %S" graphs)
  (-reduce-from
   (-lambda ((vs1 . es1) (vs2 . es2))
             (cons (-union vs1 vs2) (-union es1 es2)))
   '(() . ())
   graphs))

(defun org-depend-graph (id db)
  (-if-let (deps (cdr (org-depend-get id db)))
      (progn
        (message "Looking at %S with deps: %S" id deps)
        (let ((new-edges (-map (-partial #'cons id) deps))
              (results (org-depend--merge-graphs
                        (-map (-rpartial #'org-depend-graph db) deps))))
          (cons (cons id (car results)) (append new-edges))))
    (message "Looking at %S with no deps." id)
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
                            (concat from " -> " to ";"))
                          edges)))
    (s-join "\n"
            (append '("digraph org_dependencies {")
                    (-map (-partial #'s-prepend "  ") edges-dot)
                    '("}")))))
