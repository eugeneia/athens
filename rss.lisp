;;;; TRIVIAL-FEED.RSS: Parse all kinds of RSS feeds.

(in-package :trivial-feed.rss)

(defun rss-version (feed-tree)
  (attribute (node-name feed-tree) '(("rss" :rss) ("rdf" :rdf))))

(defun rss-feed-p (feed-tree)
  (not (null (rss-version feed-tree))))


;;; RDF parser.

(defun format-rdf-authors (author-nodes)
  (format nil "~{~a~^, ~}" (mapcar 'node-text author-nodes)))

(defun parse-rdf-node (rdf-nodes)
  ;; "date", "creator", "contributor" and "language" nodes are defined by
  ;; Dublin Core (http://web.resource.org/rss/1.0/modules/dc/).
  (let ((link-node
         (find-if (node-by-name "link") rdf-nodes))
        (date-node
         (find-if (node-by-name "date") rdf-nodes))
        (title-node
         (find-if (node-by-name "title") rdf-nodes))
        (author-nodes
         (append
          (remove-if-not (node-by-name "creator") rdf-nodes)
          (remove-if-not (node-by-name "contributor") rdf-nodes)))
        (description-node
         (find-if (node-by-name "description") rdf-nodes))
        (language-node
         (find-if (node-by-name "language") rdf-nodes)))
    (values
     (and link-node (node-text link-node))
     (and date-node (parse-date-time (node-text date-node)))
     (and title-node (node-text title-node))
     (and author-nodes (format-rdf-authors author-nodes))
     (and description-node (node-text description-node))
     (and language-node (node-text language-node)))))

(defun parse-rdf-feed (rdf-nodes)
  (let ((channel-node (find-if (node-by-name "channel") rdf-nodes))
        (item-nodes (remove-if-not (node-by-name "item") rdf-nodes)))
    (multiple-value-bind (link date title author description language)
        (parse-rdf-node (node-children channel-node))
      (make-feed
       (mapcar (lambda (node)
                 (multiple-value-bind (link date* title author*
                                       description language*)
                     (parse-rdf-node (node-children node))
                   (make-feed-item :link link
                                   :date (or date* date)
                                   :title title
                                   :author (or author* author)
                                   :description description
                                   :language (or language* language))))
               item-nodes)
       :link link
       :date date
       :title title
       :description description))))

(defun parse-rss-channel (channel-nodes)
  (let ((link-node
         (find-if (node-by-name "link") channel-nodes))
        (date-node
         (or (find-if (node-by-name "lastBuildDate") channel-nodes)
             (find-if (node-by-name "pubDate") channel-nodes)))
        (author-node
         (or (find-if (node-by-name "managingEditor") channel-nodes)
             (find-if (node-by-name "webMaster") channel-nodes)))
        (title-node
         (find-if (node-by-name "title") channel-nodes))
        (description-node
         (find-if (node-by-name "description") channel-nodes))
        (language-node
         (find-if (node-by-name "language") channel-nodes)))
    (values
     (and link-node (node-text link-node))
     (and date-node (parse-date-time (node-text date-node)))
     (and author-node (node-text author-node))
     (and title-node (node-text title-node))
     (and description-node (node-text description-node))
     (and language-node (node-text language-node)))))

(defun parse-rss-item (item-nodes)
  (let ((link-node
         (find-if (node-by-name "link") item-nodes))
        (date-node
         (find-if (node-by-name "lastBuildDate") item-nodes))
        (author-node
         (find-if (node-by-name "author") item-nodes))
        (title-node
         (find-if (node-by-name "title") item-nodes))
        (description-node
         (find-if (node-by-name "description") item-nodes)))
    (values (and link-node (node-text link-node))
            (and date-node (parse-date-time (node-text date-node)))
            (and author-node (node-text author-node))
            (and title-node (node-text title-node))
            (and description-node (node-text description-node)))))

(defun parse-rss-feed (nodes)
  (let* ((channel-node (find-if (node-by-name "channel") nodes))
         (item-nodes (remove-if-not (node-by-name "item")
                                    (node-children channel-node))))
    (multiple-value-bind (link date author title description language)
        (parse-rss-channel (node-children channel-node))
      (make-feed
       (mapcar (lambda (item-node)
                 (multiple-value-bind
                       (link date* author* title description)
                     (parse-rss-item (node-children item-node))
                   (make-feed-item
                    :link link
                    :date (or date* date)
                    :author (or author* author)
                    :title title
                    :description description
                    :language language)))
               item-nodes)
       :link link
       :date date
       :title title
       :description description))))

(defun parse-rss (feed-tree)
  (case (rss-version feed-tree)
    (:rdf (parse-rdf-feed (node-children feed-tree)))
    (:rss (parse-rss-feed (node-children feed-tree)))))
