;;;; JavaScript browser for an Athens instance.

;;;; BUG: update-widget doesn't work.

(in-package :athens.magnifier)

(defmacro send (method object &rest arguments)
  `(,(if (and (listp object)
              (eq '@ (first object)))
         (append object (list method))
         `(@ ,object ,method))
     ,@arguments))

(defun hash-keys (o)
  (send keys |Object| o))

(defmacro with-event ((object event) (&rest arguments) &body body)
  "Attach BODY with ARGUMENTS to EVENT for OBJECT."
  `(send add-event-listener ,object ,event
         (lambda (,@arguments)
           ,@body)))

(defun make-text-node (string)
  "Make text node for STRING."
  (send create-text-node document string))

(defun make-node (type &rest children)
  "Make DOM node of TYPE with CHILDREN."
  (let ((node (send create-element document type)))
    (loop for child in children do
         (send append-child
               node
               (if (stringp child)
                   (make-text-node child)
                   child)))
    node))

(defun clear-node (node)
  "Remove children of NODE."
  (loop while (send has-child-nodes node)
     do (send remove-child node (@ node last-child))))

(defmacro style (node &rest rules &aux (node-sym (gensym "node")))
  (unless (evenp (length rules))
    (error "Uneven number of arguments (key/value pairs) in RULES."))
  `(let ((,node-sym ,node))
     (setf ,@(loop for head = rules then (cddr head)
                while head
                collect `(@ ,node-sym style ,(first head))
                collect (second head)))
     ,node-sym))

(defun load-html (string &optional (parent-type :div))
  "Parse DOM from STRING and return it in a container of PARENT-TYPE."
  (let ((node (make-node parent-type)))
    (setf (@ node |innerHTML|) string)
    node))

(defun make-widget (element &optional update-fn)
  "Make widget for ELEMENT with UPDATE-FN."
  (create :element element
          :update-fn update-fn))

(defun widget-element (widget)
  "Get element of WIDGET."
  (@ widget :element))

(defun append-widget (element widget)
  "Append WIDGET element to ELEMENT."
  (send append-child element (widget-element widget)))

(defun update-widget (widget &rest args)
  "Update WIDGET with ARGS."
  (apply (@ widget :update-fn) args))

(defun make-button (label action-fn &optional (type :button))
  "Make button widget with element of TYPE with LABEL that calls
ACTION-FN when pressed."
  (let ((button (make-node type label)))
    (send add-event-listener button "click" action-fn)
    (make-widget button)))

(defvar *cl-1970* 2208992400
  "1970 as a CL timestamp.")

(defun cl-date (js-date)
  "Convert JS timestamp to CL timestamp (e.g. seconds since 1900). This
   operation looses millsecond precision."
  (+ (floor (/ (send get-time js-date) 1000)) *cl-1970*))

(defun js-date (cl-date)
  "Convert CL timestamp to JS timestamp (e.g. milliseconds since 1970)."
  (if (>= cl-date *cl-1970*)
      (new (|Date| (* (- cl-date *cl-1970*) 1000)))
      (throw "Can not convert CL-DATE: Before 1970.")))

(defun get-universal-time ()
  "Get universal time as a CL date."
  (cl-date (new (|Date|))))

(defun parse-date (string)
  "Parse CL date from STRING."
  (let ((result (send parse |Date| string)))
    (if (and (numberp result)
             (not (|isNaN| result)))
        (new (|Date| result))
        (throw (+ "Could not parse: " string)))))

(defun text-node-p (node)
  "Predicate to test if NODE is a text node."
  (= 3 (@ node node-type)))

(defun node-string (node)
  "Concatenate string for text nodes of NODE."
  (let ((s ""))
    (loop for node in (@ node child-nodes)
       do (setf s (+ s " " (if (text-node-p node)
                               (@ node node-value)
                               (node-string node)))))
    s))

(defun blacklisted-p (string)
  (aref (create "the" t)
        string))

(defun string-words (string)
  "Return list of words for STRING."
  (remove-if
   blacklisted-p
   (let ((re (regex "/[a-z-]+[a-z0-9-]+[a-z0-9-]+/gi")))
     (loop for match = (send exec re string)
        while match collect (send to-lower-case (aref match 0))))))

(defun record-occurence (db string source &optional (delta 1))
  "Record occurence (scaled by DELTA) of STRING in SOURCE to DB."
  (let ((record #1=(aref db string)))
    (if record
	(if #2=(aref record source)
	    (incf #2# delta)
	    (setf #2# delta))
        (let ((record (create)))
          (setf (aref record source) delta
                #1# record))))
  (values))

(defun keyword-score (occurrences sources total)
  "Compute score for keyword with OCCURRENCES, SOURCES and TOTAL.

   * OCCURRENCES is the number of occurrences in the source.
   * SOURCES is the number of sources where the keyword in occurs.
   * TOTAL is the number of all sources."
  (* occurrences (log (+ 1 (/ total sources)))))

(defun get-scores (db source n)
  "Return a list of keyword/score pairs for SOURCE in DB with N sources."
  (loop for string in (hash-keys db)
        for record = (aref db string)
        for occurrences = (aref record source)
     when occurrences collect
       (list string
             (keyword-score
              occurrences (@ (hash-keys record) :length) n))))

(defun unique-n (scores n)
  "Return N most unique keyword/score pairs ordered by relevance for
SCORES."
  (send slice
        (send sort scores
              (lambda (x y)
                ;; Sort scores by score (descending).
                (- (aref y 1) (aref x 1))))
        0 n))

(defun common-n (keywords n)
  "Return N most common keywords."
  (let ((scores (create)))
    ;; Count occurrences.
    (loop for keyword in keywords do
         (if #1=(aref scores keyword)
             (incf #1#)
             (setf #1# 1)))
    ;; Sort, truncate.
    (map (lambda (x) (aref x 0))
         (send slice
               (send sort
                     (loop for keyword in (hash-keys scores)
                        collect (list keyword (aref scores keyword)))
                     (lambda (x y)
                       ;; Sort scores by score (descending).
                       (- (aref y 1) (aref x 1))))
               0 n))))

(defun request (url callback error)
  "Perform 'GET' request to URL. Call CALLBACK with parsed JSON response
on success and ERROR on failure."
  (let ((request (new (|XMLHttpRequest|))))
    (setf (@ request :onreadystatechange)
          (lambda ()
            (when (= (@ request ready-state) 4)
              (if (= (@ request :status) 200)
                  ;; Request successful, call resultFn with parsed
                  ;; object.
                  (callback
                   (send parse *JSON* (@ request response-text)))
                  ;; Request failed, call ERROR.
                  (error)))))
    (send open request "GET" url t)
    (send send request nil)))

(defun post (url payload callback error)
  "Post PAYLOAD to URL. Call CALLBACK with request on success, and ERROR on
failure."
  (let ((request (new (|XMLHttpRequest|))))
    (setf (@ request :onreadystatechange)
          (lambda ()
            (when (= (@ request ready-state) 4)
              (if (= (@ request status) 200)
                  (callback request)
                  (error)))))
    (send open request "POST" url t)
    (send send request payload)))

(defun hsv-to-rgb (h s v)
  (let* ((h-i (parse-int (* h 6)))
         (f (- (* h 6) h-i))
         (p (* v (- 1 s)))
         (q (* v (- 1 (* f s))))
         (tt (* v (- 1 (* (- 1 f) s)))))
    (case h-i
      (0 (list v tt p))
      (1 (list q v p))
      (2 (list p v tt))
      (3 (list p q v))
      (4 (list tt p v))
      (5 (list v p q))
      (otherwise (throw (+ "Bad hue value: " h))))))

(defvar *golden-ratio* 0.618033988749895)

(defun keyword-color (keyword)
  (let ((n 0))
    (loop for i from 0 to (1- (@ keyword length)) do
         (incf n (* (send char-code-at keyword i) *golden-ratio*)))
    (let ((rgb (hsv-to-rgb (% n 1) 0.2 0.98)))
      (flet ((to-hex (n) (send to-string (parse-int (* n 256)) 16)))
        (+ "#"
           (to-hex (aref rgb 0))
           (to-hex (aref rgb 1))
           (to-hex (aref rgb 2)))))))

(defun item-title-words (item)
  "Return words of ITEM title."
  (string-words (@ item "TITLE")))

(defun item-description-words (item)
  "Return words of ITEM description."
  (string-words (node-string (@ item "DESCRIPTION"))))

(defun make-timespan (callback &optional init-from init-to)
  "Timespan selector widget."
  (let* ((from (make-node :input))
         (to (make-node :input))
         (select (make-button "Select" (lambda ()
                                         (callback (@ from :value)
                                                   (@ to :value))))))
    (send set-attribute from :type "datetime-local")
    (send set-attribute to :type "datetime-local")
    (when init-from
      (setf (@ from :value) (send to-string init-from)))
    (when init-to
      (setf (@ to :value) (send to-string init-to)))
    (make-widget (make-node :nav
                            from to
                            (widget-element select)))))

(defun build-context (db items size)
  ;; Scan words.
  (loop for hash in (hash-keys items)
        for item = (aref items hash)
     do
       (loop for word in (item-title-words item)
          do (record-occurence db word hash 4))
       (loop for word in (item-description-words item)
          do (record-occurence db word hash 1)))
  ;; Assign keywords to items.
  (loop for hash in (hash-keys items)
        for item = (aref items hash)
     do (setf (@ item :keywords)
              (unique-n (get-scores db hash size) 4)))
  ;; Return nothing.
  (values))

(defun make-progress ()
  "Widget that displays progress messages."
  (let ((status (style (make-node :div)
                       :position "absolute"
                       :top "0.5em" :left "0.5em"
                       :color "#ccc")))
    (flet ((on (message)
             (send append-child status
                   (make-text-node (+ message " "))))
           (off ()
             (clear-node status)))
      (make-widget status (lambda (action &optional message)
                            (case action
                              (:status (on message))
                              (:done   (off))))))))

(defvar *progress* (make-progress)
  "Global progress-meter.")

(defun load-context (from to callback)
  "Load context for timespan defined by FROM and TO and pass it to
CALLBACK."
  (update-widget *progress* :status "Fetching data...")
  (request (+ "/news.json?start=" from "&end=" to)
           ;; On success.
           (lambda (response)
             (update-widget *progress* :status "Building context...")
             (let ((items (create)))
               ;; Sort items in RESPONSE into items table.
               (loop for (hash item) in (@ response "ITEMS") do
                    (setf #1=(@ item "DESCRIPTION") (load-html #1#)
                          (@ item :hash) hash
                          (aref items hash) item))
               (let ((size (@ (hash-keys items) :length))
                     (db (create)))
                 ;; Fill DB.
                 (build-context db items size)
                 ;; Call CALLBACK with context.
                 (update-widget *progress* :done)
                 (callback (create :items items
                                   :size size
                                   :db db)))))
           ;; On failure.
           (lambda () (alert "Could not retrieve context."))))

(defmacro with-context ((var from to) &body body)
  `(load-context ,from ,to (lambda (,var) ,@body)))

(defun context-keywords (context)
  (with-slots (items size db)
      context
    (common-n
     (map (lambda (x) (aref x 0))
          (loop for hash in (hash-keys items)
             append (@ (aref items hash) :keywords)))
     32)))

(defun context-keyword-edges (context keywords)
  "Find edges of KEYWORDS in CONTEXT."
  (with-slots (items size db)
      context
    (let ((edges (create)))
      (loop for keyword in keywords
            for keyword-edges = (setf (aref edges keyword) (create))
         do (loop for hash in (hash-keys (aref db keyword))
                  for item = (aref items hash)
               do (loop for (item-keyword . ()) in (@ item :keywords) do
                       (when (and (not (eql item-keyword keyword))
                                  (find item-keyword keywords))
                         (if #1=(aref keyword-edges item-keyword)
                             (incf #1#)
                             (setf #1# 1)))))
           (setf (aref edges keyword)
                 (send sort (hash-keys keyword-edges)
                       (lambda (x y)
                         (> (aref keyword-edges x)
                            (aref keyword-edges y))))))
      edges)))

(defun make-keywords ()
  (let ((span (make-node :ul)))
    (make-widget
     span
     (lambda (context)
       (clear-node span)
       (let* ((keywords (context-keywords context))
              (edges (context-keyword-edges context keywords)))
         (loop for keyword in keywords
               for keyword-edges = (aref edges keyword)
            do (send append-child span
                     (style
                      (apply make-node :li
                             (make-node :b keyword)
                             (loop for edge in keyword-edges
                                collect (make-text-node " ")
                                collect (style
                                         (make-node :a edge)
                                         :font-size "xx-small"
                                         :background (keyword-color edge)
                                         :border "1px solid"
                                         :border-radius "0.2em")))
                      :background (keyword-color keyword)
                      :border "1px solid white"
                      :border-radius "0.2em"))))))))

(defun query (context keywords)
  (let ((results nil))
    (loop for keyword in keywords
          for records = (aref (@ context :db) keyword)
       do (if records
              (setf results (loop for hash in (hash-keys records)
                                  for item = (aref (@ context :items) hash)
                               when (or (= results nil)
                                        (find item results))
                               collect item))
              (setf results (list))))
    (when (and results (> (@ results :length) 0))
      (send sort results (lambda (x y) (- (@ x "DATE") (@ y "DATE")))))))

(defun render-item (item)
  (let ((header (make-node :a (@ item "TITLE")))
        (permalink (make-node :a "[link]"))
        (words ""))
    (send set-attribute header :href (@ item "LINK"))
    (send set-attribute header :target "_blank")
    (send set-attribute permalink :href (+ "/item/" (@ item :hash) ".html"))
    (send set-attribute permalink :target "_blank")
    (loop for keyword in (@ item :keywords) do
         (incf words (+ (aref keyword 0) " ")))
    (make-node
     :details
     (make-node :summary
                (make-node :em (send to-string (js-date (@ item "DATE"))))
                (make-node :br)
                header " " permalink
                (make-node :br)
                (make-node :code words))
     (@ item "DESCRIPTION"))))

(defun make-query ()
  (let ((input (make-node :input))
        (results (make-node :ul))
        (last-context nil))
    (flet ((update (context)
             (setf last-context context)
             (clear-node results)
             (let ((matches (query context
                                   (remove-duplicates
                                    (string-words (@ input :value))))))
               (if matches
                   (loop for item in matches
                         for li = (make-node :li (render-item item))
                      do (send append-child results
                               (style li
                                      :background
                                      (keyword-color
                                       (first (first (@ item :keywords))))
                                      :margin-top "0.5em"
                                      :border-radius "0.2em")))
                   (send append-child results
                         (make-node :p "No matches."))))))
      (send set-attribute input :type :text)
      (make-widget
       (make-node :div
                  input
                  (widget-element
                   (make-button "Query"
                                (lambda ()
                                  (if last-context
                                      (update last-context)
                                      (alert "No context selcted.")))))
                  results)
       update))))

(defun make-add-feed ()
  (let ((url (make-node :input))
        (result (make-node :p)))
    (flet ((add-feed ()
             (clear-node result)
             (post "/feed" (@ url :value)
                   (lambda (request)
                     (let ((a (make-node :a "Feed added!")))
                       (send set-attribute a :href (@ request |responseURL|))
                       (send append-child result a)))
                   (lambda ()
                     (send append-child result
                           (make-text-node
                            "Request failed, try again later?"))))))
      (make-widget
       (make-node :div
                  url
                  (widget-element (make-button "Add feed" add-feed))
                  result)))))

;; Kickstart.
(with-event (window :load) ()
  (let* ((body (@ document :body))
         (keywords (make-keywords))
         (query (make-query))
         (timespan (make-timespan
                    (lambda (from to)
                      (try
                       (let ((from (cl-date (parse-date from)))
                             (to (cl-date (parse-date to))))
                         (cond
                           ((< to from)
                            (throw "Negative timeframe."))
                           ((> (- to from) 604800)
                            (throw "Maximum timeframe is one week.")))
                         (with-context (context from to)
                           (update-widget keywords context)
                           (update-widget query context)))
                       (:catch (error)
                         (alert (+ "Failure: " error)))))
                    (new (|Date| (- (new (|Date|)) (* 1000 60 60 24 7))))
                    (new (|Date|))))
         (add-feed (make-add-feed)))
    (send append-child body
          (widget-element *progress*))
    (send append-child body
          (make-node :header (make-node :h1 "Athens Magnifier")))
    (send append-child body
          (make-node :header (make-node :b "Timeframe:")))
    (append-widget body timespan)
    (append-widget body keywords)
    (send append-child body
          (make-node :header (make-node :b "Query by tokens:")))
    (append-widget body query)
    (send append-child body
          (make-node :header (make-node :b "Add feed:")))
    (append-widget body add-feed)))
