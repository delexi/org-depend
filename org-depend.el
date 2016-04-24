(equal "0e45f3bb-0768-4f3d-8149-4b9d1ad50068"
 "0e45f3bb-0768-4f3d-8149-4b9d1ad50068"
)

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
