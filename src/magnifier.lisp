;;;; JavaScript browser for an Athens instance.

;;;; BUG: update-widget doesn't work.

(in-package :athens.magnifier)

;;;; ** BASICS **

(defmacro send (method object &rest arguments)
  `(,(if (and (listp object)
              (eq '@ (first object)))
         (append object (list method))
         `(@ ,object ,method))
     ,@arguments))

(defun hash-keys (o)
  (send keys |Object| o))

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

(defmacro style (node &body rules
                      &aux (node-sym (gensym "node")))
  (unless (evenp (length rules))
    (error "Uneven number of arguments (key/value pairs) in RULES."))
  `(let ((,node-sym ,node))
     (setf ,@(loop for head = rules then (cddr head) while head
                collect `(@ ,node-sym style ,(first head))
                collect (second head)))
     ,node-sym))

(defmacro attributes (node &body attributes
                           &aux (node-sym (gensym "node")))
  (unless (evenp (length attributes))
    (error "Uneven number of arguments (key/value pairs) in ATTRIBUTES."))
  `(let ((,node-sym ,node))
     ,@(loop for head = attributes then (cddr head) while head
          collect `(send set-attribute ,node-sym ,(first head) ,(second head)))
     ,node-sym))

(defmacro events (node &body events
                       &aux (node-sym (gensym "node")))
  `(let ((,node-sym ,node))
     ,@(loop for (event arguments . body) in events
          collect `(send add-event-listener ,node-sym ,event
                         (lambda (,@arguments)
                           ,@body)))
     ,node-sym))

(defun load-html (string &optional (parent-type :div))
  "Parse DOM from STRING and return it in a container of PARENT-TYPE."
  (let ((node (make-node parent-type)))
    (setf (@ node |innerHTML|) (or string ""))
    node))

(defun inject-css (rule)
  (let ((payload (make-node :div)))
    (setf (@ payload |innerHTML|)
          (+ "<style>" rule "</style>"))
    (send append-child (@ document :body) payload)))

(defun text-node-p (node)
  "Predicate to test if NODE is a text node."
  (= 3 (@ node node-type)))

(defun node-string (node)
  "Concatenate string for text nodes of NODE."
    (loop for node in (@ node child-nodes) sum
         (if (text-node-p node)
             (@ node node-value)
             (node-string node))))

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

;;;; ** WIDGET GENERICS **

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
    (send add-event-listener button :click action-fn)
    (make-widget button)))

(defun make-toggle (action-fn &rest states)
  (when (< (length states) 2)
    (throw "Need at least two states."))
  (let ((label (make-text-node (first states)))
        (next (rest states)))
    (make-widget
     (events (style (make-node :button label)
               :font-family "monospace"
               :font-size "x-small")
       (:click ()
         (setf (@ label data) (first next)
               next           (or (rest next) states))
         (action-fn (@ label data)))))))

;;;; ** DATE/TIME HANDLNG **

(defvar *cl-1970* 2208992400
  "1970 as a CL timestamp.")

(defun date (&optional n)
  (if n
      (new (|Date| n))
      (new (|Date|))))

(defun cl-date (js-date)
  "Convert JS timestamp to CL timestamp (e.g. seconds since 1900). This
   operation looses millsecond precision."
  (+ (floor (/ (send get-time js-date) 1000)) *cl-1970*))

(defun js-date (cl-date)
  "Convert CL timestamp to JS timestamp (e.g. milliseconds since 1970)."
  (if (>= cl-date *cl-1970*)
      (date (* (- cl-date *cl-1970*) 1000))
      (throw "Can not convert CL-DATE: Before 1970.")))

(defun get-universal-time ()
  "Get universal time as a CL date."
  (cl-date (date)))

(defun parse-date (string)
  "Parse CL date from STRING."
  (let ((result (send parse |Date| string)))
    (if (and (numberp result)
             (not (|isNaN| result)))
        (date result)
        (throw (+ "Could not parse: " string)))))

;;;; ** TEXT PROCESSING / KEYWORD ELECTION **

(defvar *word-regex* (regex "/[a-z-][a-z0-9-]+/gi"))
(defvar *item-keywords-size* 4)
(defvar *title-word-scale* 4)
(defvar *description-word-scale* 1)
(defvar *context-size* 86400) ; seconds
(defvar *context-keywords-size* 16)

(defun blacklisted-p (string)
  (aref (create "the" t)
        string))

(defun string-words (string)
  "Return list of words for STRING."
  (remove-if
   blacklisted-p
     (loop for match = (send exec *word-regex* string)
        while match collect (send to-lower-case (aref match 0)))))

(defun item-title-words (item)
  "Return words of ITEM title."
  (string-words (@ item "TITLE")))

(defun item-description-words (item)
  "Return words of ITEM description."
  (string-words (node-string (@ item "DESCRIPTION"))))

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

(defun build-context (db items size)
  ;; Scan words.
  (loop for hash in (hash-keys items)
        for item = (aref items hash)
     do
       (loop for word in (item-title-words item)
          do (record-occurence db word hash *title-word-scale*))
       (loop for word in (item-description-words item)
          do (record-occurence db word hash *description-word-scale*)))
  ;; Assign keywords to items.
  (loop for hash in (hash-keys items)
        for item = (aref items hash)
     do (setf (@ item :keywords)
              (unique-n (get-scores db hash size) *item-keywords-size*)))
  ;; Return nothing.
  (values))

(defun timeframe-contexts (start end)
  (loop for x from start to end by (1+ *context-size*)
     collect (list x (min (+ x *context-size*) end))))

(defun load-contexts (from to callback)
  "Load contexts for timeframe defined by FROM and TO and pass it to
CALLBACK."
  (update-widget *progress* :status "Loading articles...")
  (let* ((contexts (list))
         (chunks (timeframe-contexts from to))
         (n-chunks (length chunks)))
    (loop for (context-start context-end) in chunks do
         (request (+ "/news.json?start=" context-start "&end=" context-end)
                  ;; On success.
                  (lambda (response)
                    (update-widget *progress* :status "...")
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
                        ;; Append context
                        (send push contexts
                              (create :items items :size size :db db))
                        (decf n-chunks)
                        (unless (> n-chunks 0)
                          ;; Call CALLBACK with context.
                          (update-widget *progress* :done)
                          (callback contexts)))))
                  ;; On failure.
                  (lambda () (alert "Could not retrieve context."))))))

(defun contexts-keywords (contexts)
  (common-n
   (map (lambda (x) (aref x 0))
        (loop for context in contexts append
             (with-slots (items) context
               (loop for hash in (hash-keys items) append
                    (@ (aref items hash) :keywords)))))
   *context-keywords-size*))

(defun context-keyword-edges (context keywords)
  "Find edges of KEYWORDS in CONTEXT."
  (with-slots (items size db)
      context
    (let ((edges (create)))
      (loop for keyword in keywords
            for keyword-edges = (setf (aref edges keyword) (create))
         when (aref db keyword) do
           (loop for hash in (hash-keys (aref db keyword))
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
                         ;; Sort edges by weight (descending).
                         (- (aref keyword-edges y)
                            (aref keyword-edges x))))))
      edges)))

(defun items-date-ascending (x y)
  (- (@ x "DATE") (@ y "DATE")))

(defun query-items-and (contexts keywords)
  (let (results)
    (loop for keyword in keywords
          for records = (loop for context in contexts
                           when #1=(aref (@ context :db) keyword) append
                             (loop for hash in (hash-keys #1#) collect
                                  (aref (@ context :items) hash)))
       do (if (> (length records) 0)
              (setf results (loop for item in records
                               when (or (null results) (find item results))
                               collect item))
              (setf results (list))))
    (when (and results (> (@ results :length) 0))
      (send sort results items-date-ascending))))

(defun query-items-or (contexts keywords)
  (let ((results (list)))
    (loop for context in contexts do
         (loop for keyword in keywords
               for items = (aref (@ context :db) keyword)
            when items do
              (loop for hash in (hash-keys items) do
                   (send push results (aref (@ context :items) hash)))))
    (when (> (@ results :length) 0)
      (send sort (remove-duplicates results) items-date-ascending))))

;;;; ** COLOR ROLL **

(defvar *hash-color-interval* 0.618033988749895) ; golden ratio
(defvar *hash-color-saturation* 0.2)
(defvar *hash-color-value* 0.98)

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
      (6 (list v p q)))))

(defun hash-color (n)
  (let ((rgb (hsv-to-rgb (% (* n *hash-color-interval*) 1)
                         *hash-color-saturation*
                         *hash-color-value*))
        (to-hex (lambda (n) (send to-string (parse-int (* n 256)) 16))))
    (+ "#" (to-hex (aref rgb 0)) (to-hex (aref rgb 1)) (to-hex (aref rgb 2)))))

(defun keyword-color (keyword)
  (hash-color (loop for i from 0 to (1- (@ keyword length)) sum
                   (* (send char-code-at keyword i) *hash-color-interval*))))

;;;; ** UI LOGIC / SPECIALIZED WIDGETS **

(defvar *timeframe-help*
"1. Select a timeframe by choosing start and end dates. This
causes the relevant feed data to be retrieved and crunched
which may take a while—be patient. On completion, a timeline
of the prominent keywords of the timeframe is displayed.")

(defvar *query-control-help*
"2. Query the timeframe for keywords. The timeline is updated
accordingly, and a list of matching articles is displayed.

You can click on keywords in the timeline and search results
to add or remove them from the query.

You can toggle between the search modes “AND” and “OR” to
match articles that contain all or any of the selected
keywords respectively.")

(defvar *add-feed-help*
"3. Are you missing a particular feed? You can add new RSS or
ATOM feeds to be archived.")

(defun make-progress ()
  (let ((status (style (make-node :div)
                  :position "absolute"
                  :bottom "1em" :right "1em"
                  :color "grey"
                  :font-style "italic"
                  :background "white"
                  :border-radius "0.2em")))
    (flet ((on (message)
             (send append-child status (make-text-node message)))
           (off ()
             (clear-node status)))
      (make-widget status (lambda (action &optional message)
                            (case action
                              (:status (on message))
                              (:done   (off))))))))

(defun render-header (title &optional help)
  (style (attributes (make-node :header title) :title (or help ""))
    :font-weight "bold"
    :cursor (and help "help")))

(defun render-date-input (initial-value)
  (attributes (make-node :input)
    :value (send to-string initial-value)
    :type "datetime-local"
    :placeholder "YYYY-MM-DD"))

(defun make-timeframe (select-fn &optional init-from init-to)
  (let* ((label (render-header "Timeframe:" *timeframe-help*))
         (from (render-date-input init-from))
         (to (render-date-input init-to))
         (select (make-button "Select" (lambda ()
                                         (select-fn (@ from :value)
                                                    (@ to :value))))))
    (make-widget (make-node :div label from "→" to (widget-element select)))))

(defun render-edge (keyword)
  (let ((edge (style (make-node :a keyword)
                :cursor "cell"
                :font-size "x-small"
                :font-style "italic"
                :color "grey"
                :background (keyword-color keyword)
                :border "thin solid grey"
                :border-radius "0.2em"
                :padding "0 0.2em")))
    (events edge
      (:mouseenter ()
        (let ((row (update-widget *timeline* :get-row keyword)))
          (when row
            (style edge :color "black" :border-color "black")
            (style row :outline "medium solid red"))))
      (:mouseleave ()
        (let ((row (update-widget *timeline* :get-row keyword)))
          (when row
            (style edge :color "grey" :border-color "grey")
            (style row :outline nil))))
      (:click ()
        (update-widget *query-control* keyword)))
    edge))

(defun render-column (keyword edges start-p)
  (style (apply make-node :td
                (if start-p
                    (events (style (make-node :a keyword)
                              :cursor "cell"
                              :font-weight "bold"
                              :font-style "italic")
                      (:click ()
                        (update-widget *query-control* keyword)))
                    "")
                (loop for edge in edges
                   collect (make-text-node " ")
                   collect (render-edge edge)))
    :background (keyword-color keyword)
    :border-width "medium"
    :border-color "white"
    :border-style (+ "solid none solid " (if start-p "solid" "none"))
    :border-radius (if start-p "0.6em 0 0 0.6em" "0")
    :padding "0.1em 0.2em"))

(defun make-timeline ()
  (let ((timeline (style (make-node :table)
                    :width "100%"
                    :margin-top "0.5em"
                    :display "block"
                    :overflow-x "auto"
                    :white-space "nowrap"
                    :border-collapse "collapse"))
        (rows nil))
    (labels ((get-row (keyword)
               (aref rows keyword))
             (start-column-p (row)
               (or (not #1=(@ row last-child))
                   (= (send get-attribute #1# :class) "empty")))
             (render-timeline-headers (contexts)
               (send append-child timeline
                     (apply make-node :tr
                            (loop for i from 0 to (1- (length contexts))
                                  for title = (+ (* i 24) "h")
                               collect (style (make-node :th title)
                                         :border-left "thin solid grey"
                                         :color "grey"
                                         :font-weight "normal"
                                         :text-align "left"
                                         :padding-left "0.5em"
                                         :padding-right "2em")))))
             (render-timeline (contexts keywords)
               (clear-node timeline)
               (setf rows (create))
               (render-timeline-headers contexts)
               (loop for keyword in keywords do
                    (send append-child timeline
                          (setf (aref rows keyword) (make-node :tr))))
               (loop for context in contexts
                     for edges = (context-keyword-edges context keywords)
                  do (loop for keyword in keywords
                           for keyword-edges = (aref edges keyword)
                           for row = (get-row keyword)
                        do (send append-child row
                                 (if (aref (@ context :db) keyword)
                                     (render-column keyword keyword-edges
                                                    (start-column-p row))
                                     (attributes (make-node :td)
                                       :class "empty")))))))
      (make-widget timeline
                   (lambda (op &optional x y)
                     (case op
                       (:get-row (get-row x))
                       (:render (render-timeline x y))
                       (otherwise (throw (+ "Bad OP: " op)))))))))

(defun render-item-permalink (item)
  (style (attributes (make-node :a (send to-string (js-date (@ item "DATE"))))
           :href (+ "/item/" (@ item :hash) ".html")
           :target "_blank")
    :text-decoration "none"
    :font-size "x-small"))

(defun render-item-header (item)
  (style (attributes (make-node :a (@ item "TITLE"))
           :href (@ item "LINK")
           :target "_blank")
    :text-decoration "none"
    :font-weight "bold"))

(defun render-item-edges (item)
  (apply make-node :span
         (loop for (edge ()) in (@ item :keywords)
            collect (render-edge edge)
            collect " ")))

(defun render-item (item)
  (make-node :article
             (make-node :header
                        (render-item-permalink item)
                        (make-node :br)
                        (render-item-header item)
                        (make-node :br)
                        (render-item-edges item))
             (@ item "DESCRIPTION")))

(defun make-query-control (query-fn)
  (let ((input (attributes (make-node :input) :placeholder "CIA bolivia…"))
        (mode "AND"))
    (labels ((query-keywords ()
               (remove-duplicates (string-words (@ input :value))))
             (query-action ()
               (query-fn (query-keywords) mode))
             (mode-action (new-mode)
               (setf mode new-mode)
               (query-action))
             (toggle-keyword (keyword)
               (let ((keywords (query-keywords)))
                 (if (find keyword keywords)
                     (setf (@ input :value)
                           (send join (remove keyword keywords) " "))
                     (incf (@ input :value) (+ " " keyword)))
                 (query-action))))
      (make-widget
       (make-node :div
                  (render-header "Keywords:" *query-control-help*)
                  input
                  (widget-element (make-toggle mode-action mode "OR"))
                  (widget-element (make-button "Query" query-action)))
       toggle-keyword))))

(defun render-matches (items)
  (let ((ol (make-node :ol)))
    (loop for item in items do
         (send append-child ol
               (style (make-node :li (render-item item))
                 :background (keyword-color (first (first (@ item :keywords))))
                 :margin-top "0.5em"
                 :padding "0.1em 0.2em"
                 :border-radius "0.2em")))
    ol))

(defun make-query-results ()
  (let ((container (make-node :div))
        (no-matches (style (make-node :p "No matches.")
                      :font-style "italic"
                      :margin "1em")))
    (make-widget container
                 (lambda (matches)
                   (clear-node container)
                   (send append-child container (if matches
                                                    (render-matches matches)
                                                    no-matches))))))

(defun make-add-feed ()
  (let ((url (attributes (make-node :input)
               :type :url
               :placeholder "http://example.com/feed.xml"))
        (result (style (make-node :div)
                  :font-style "italic")))
    (labels ((post-ok (request)
               (send append-child result
                     (style (attributes (make-node :a "Feed added!")
                              :href (@ request |responseURL|)
                              :target "_blank")
                       :text-decoration "none"))
               (setf (@ url :value) ""))
             (post-error ()
               (send append-child result
                     (make-text-node
                      "Request failed, try again later?")))
             (add-feed ()
               (clear-node result)
               (if (= (@ url :value) "")
                   (send focus url)
                   (post "/feed" (@ url :value) post-ok post-error))))
      (make-widget
       (make-node :div
                  (render-header "New feed:" *add-feed-help*)
                  url
                  (widget-element (make-button "Add" add-feed))
                  result)))))

(defun render-menus (&rest widgets)
  (apply make-node :nav (loop for widget in widgets collect
                             (style (widget-element widget)
                               :display "inline-block"
                               :vertical-align "top"
                               :margin-right "0.5em"))))

;;;; ** BOOT **

(defvar *contexts*)

(defun magnifier-refresh (keywords &optional (mode "AND"))
  (cond ((null *contexts*)
         (return))
        ((> (length keywords) 0)
         (update-widget *timeline* :render *contexts* keywords)
         (update-widget *query-results*
                        (case mode
                          ("AND" (query-items-and *contexts* keywords))
                          ("OR"  (query-items-or *contexts* keywords)))))
        (t
         (update-widget *timeline* :render *contexts*
                        (contexts-keywords *contexts*))
         (update-widget *query-results* nil))))

(defun magnifier-load (from to)
  (try (let ((from (cl-date (parse-date from)))
             (to (cl-date (parse-date to))))
         (cond ((< to from)
                (throw "Negative timeframe."))
               ((> (- to from) 604800)
                (throw "Maximum timeframe is one week.")))
         (load-contexts from to
                        (lambda (contexts)
                         (setf *contexts* contexts)
                         (magnifier-refresh '()))))
       (:catch (error)
         (alert (+ "Failure: " error)))))

(defvar *now*           (date))
(defvar *last-week*     (date (- *now* (* 1000 60 60 24 7))))

(defvar *timeframe*     (make-timeframe magnifier-load *last-week* *now*))
(defvar *query-control* (make-query-control magnifier-refresh))
(defvar *add-feed*      (make-add-feed))
(defvar *timeline*      (make-timeline))
(defvar *query-results* (make-query-results))
(defvar *progress*      (make-progress))

(events window
  (:load ()
    (inject-css "body { padding: 0.5em; font-family: sans-serif; }")
    (inject-css "p { margin: 0.2em 0 0 0; }")
    (inject-css
     "input { border: none;
              border-bottom: medium solid lightgrey; }
      input:focus { border-color: grey; }")
    (inject-css
     "button { margin: 0 0.2em;
               font-size: x-small;
	       font-weight: bold;
	       color: #6666ff;
	       border: medium solid #6666ff;
	       border-radius: 5em;
	       background: none; }
      button:hover { background: #6666ff;
	             color: white; }")
    (let ((body (@ document :body)))
      (send append-child body
            (render-menus *timeframe*
                          *query-control*
                          *add-feed*))
      (append-widget body *timeline*)
      (append-widget body *query-results*)
      (append-widget body *progress*))))
