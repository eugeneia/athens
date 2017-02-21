;;;; TRIVIAL-FEED.ATOM: Parse all kinds of ATOM feeds.

(in-package :trivial-feed.atom)

(defun atom-version (feed-tree)
  (string-equal "feed" (node-name feed-tree)))
;;  (and (string-equal "feed" (node-name feed-tree))
;;       (attribute (node-ns feed-tree)
;;                  '(("http://purl.org/atom/ns#" :0.3)
;;                    ("http://www.w3.org/2005/Atom" :1.0)))))

(defun atom-feed-p (feed-tree)
  (not (null (atom-version feed-tree))))

(defun feed-link (link-nodes)
  (or (find-if (lambda (node) (xmlrep-attrib-value "self" node nil))
               link-nodes)
      (first link-nodes)))

(defun date-node-p (node)
  (member (node-name node)
          '("updated" "modified" "published" "issued" "created")
          :test #'string-equal))

(defun cdate-node-p (node)
  (member (node-name node)
          '("published" "issued" "created")
          :test #'string-equal))

(defun author-node-p (node)
  (member (node-name node)
          '("author" "contributor")
          :test #'string-equal))

(defun description-node-p (node)
  (member (node-name node)
          '("subtitle" "tagline")
          :test #'string-equal))

(defun format-authors (author-nodes)
  (format nil "~{~a~^, ~}"
          (loop for node in author-nodes
             collect
               (let ((name-node (find-if (node-by-name "name") node))
                     (email-node (find-if (node-by-name "email") node))
                     (uri-node (find-if (node-by-name "uri") node)))
                 (format nil "~a~@[ <~a>~]~@[ ~a~]"
                         (node-text name-node)
                         (and email-node (node-text email-node))
                         (and uri-node (node-text uri-node)))))))

(defun parse-atom-feed-meta (feed-tree)
  (let ((link-node
         (feed-link (remove-if-not (node-by-name "link") feed-tree)))
        (date-node
         (find-if #'date-node-p feed-tree))
        (author-nodes
         (remove-if-not #'author-node-p feed-tree))
        (title-node
         (find-if (node-by-name "title") feed-tree))
        (description-node
         (find-if #'description-node-p feed-tree)))
    (values
     (and link-node (xmlrep-attrib-value "href" link-node nil))
      (and date-node (parse-date-time (node-text date-node)))
     (and author-nodes (format-authors author-nodes))
     (and title-node (node-text title-node))
     (and description-node (node-text description-node)))))

(defun parse-atom-entry (entry-node)
  (let ((nodes (node-children entry-node)))
    (let ((link-node
           (feed-link (remove-if-not (node-by-name "link") nodes)))
          (date-node
           (or (find-if #'cdate-node-p nodes)
               (find-if #'date-node-p nodes)))
          (author-nodes
           (remove-if-not #'author-node-p nodes))
          (title-node
           (find-if (node-by-name "title") nodes))
          (description-node
           (find-if #'description-node-p nodes)))
      (values
       (and link-node (xmlrep-attrib-value "href" link-node nil))
       (and date-node (parse-date-time (node-text date-node)))
       (and author-nodes (format-authors author-nodes))
       (and title-node (node-text title-node))
       (and description-node (node-text description-node))
       (xmlrep-attrib-value "lang" entry-node nil)))))

(defun parse-atom-feed (feed-tree)
  (multiple-value-bind (link date author title description)
      (parse-atom-feed-meta feed-tree)
    (make-feed
     (mapcar (lambda (entry-node)
               (multiple-value-bind
                     (link date* author* title description language)
                   (parse-atom-entry entry-node)
                 (make-feed-item
                  :link link
                  :date (or date* date)
                  :author (or author* author)
                  :title title
                  :description description
                  :language language)))
             (remove-if-not (node-by-name "entry") feed-tree))
     :link link
     :date date
     :title title
     :description description)))

(defun parse-atom (feed-tree)
  (parse-atom-feed feed-tree))
