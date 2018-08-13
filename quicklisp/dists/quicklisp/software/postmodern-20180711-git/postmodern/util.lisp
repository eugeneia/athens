(in-package :postmodern)

(defun to-identifier (name)
  "Used to allow both strings and symbols as identifier - converts
symbols to string with the S-SQL rules."
  (if (stringp name)
      name
      (to-sql-name name)))

(defun sequence-next (sequence)
  "Shortcut for getting the next value from a sequence."
  (query (:select (:nextval (to-identifier sequence))) :single))

(defmacro make-list-query (relkind)
  "Helper macro for the functions that list tables, sequences, and
views."
  `(sql (:select 'relname :from 'pg-catalog.pg-class
         :inner-join 'pg-catalog.pg-namespace :on (:= 'relnamespace 'pg-namespace.oid)
         :where (:and (:= 'relkind ,relkind)
                 (:not-in 'nspname (:set "pg_catalog" "pg_toast"))
                 (:pg-catalog.pg-table-is-visible 'pg-class.oid)))))

(defmacro make-exists-query (relkind name)
  "Helper macro for the functions that check whether an object
exists."
  `(sql (:select (:exists (:select 'relname :from 'pg_catalog.pg_class :inner-join 'pg_catalog.pg_namespace :on
                                   (:= 'pg_class.relnamespace 'pg_namespace.oid)
                                   :where (:and (:= 'pg_class.relkind ,relkind)
                                                (:= 'pg_namespace.nspname (:any* (:current_schemas "true")))
                                                (:= 'pg_class.relname (to-identifier ,name))))))))

(defun list-tables (&optional strings-p)
  "Return a list of the tables in a database. Turn them into keywords
if strings-p is not true."
  (let ((result (query (make-list-query "r") :column)))
    (if strings-p result (mapcar 'from-sql-name result))))

(defun table-exists-p (table-name)
  "Check whether a table exists. Takes either a string or a symbol for
the table name."
  (query (make-exists-query "r" (to-sql-name table-name)) :single))

(defun list-sequences (&optional strings-p)
  "Return a list of the sequences in a database. Turn them into
keywords if strings-p is not true."
  (let ((result (query (make-list-query "S") :column)))
    (if strings-p result (mapcar 'from-sql-name result))))

(defun sequence-exists-p (sequence)
  "Check whether a sequence exists. Takes either a string or a symbol
for the sequence name."
  (query (make-exists-query "S" (to-sql-name sequence)) :single))

(defun list-views (&optional strings-p)
  "Return a list of the views in a database. Turn them into keywords
if strings-p is not true."
  (let ((result (query (make-list-query "v") :column)))
    (if strings-p result (mapcar 'from-sql-name result))))

(defun view-exists-p (view)
  "Check whether a view exists. Takes either a string or a symbol for
the view name."
  (query (make-exists-query "v" view) :single))

(defun table-description (table-name &optional schema-name)
  "Return a list of (name type null-allowed) lists for the fields of a
table."
  (setf table-name (to-sql-name table-name))
  (when schema-name (setf schema-name (to-sql-name schema-name)))
  (let ((schema-test (if (and schema-name (schema-exists-p schema-name) (table-exists-p table-name))
                         (sql (:= 'pg-namespace.nspname schema-name))
                         "true")))
    (mapcar #'butlast
            (query (:order-by (:select 'attname 'typname (:not 'attnotnull) 'attnum :distinct
                                       :from 'pg-catalog.pg-attribute
                                       :inner-join 'pg-catalog.pg-type :on (:= 'pg-type.oid 'atttypid)
                                       :inner-join 'pg-catalog.pg-class :on (:and (:= 'pg-class.oid 'attrelid)
                                                                                  (:= 'pg-class.relname (to-identifier table-name)))
                                       :inner-join 'pg-catalog.pg-namespace :on (:= 'pg-namespace.oid 'pg-class.relnamespace)
                                       :where (:and (:> 'attnum 0) (:raw schema-test)))
                              'attnum)))))

(defun coalesce (&rest args)
  (some (lambda (x) (if (eq x :null) nil x)) args))

(defun database-version ()
  "Returns the version of the current postgresql database."
  (query (:select (:version)) :single))

(defun num-records-in-database ()
  "Returns a list of lists with schema, table name and approximate number of records
in the currently connected database."
  (query (:order-by (:select 'schemaname 'relname 'n_live_tup
			     :from 'pg_stat_user_tables)
		    (:desc 'n_live_tup))))

(defun current-database ()
  "Returns the string name of the current database."
  (query (:select (:current-database)) :single))

(defun database-exists-p (database-name)
  "Determine if a particular database exists. "
  (setf database-name (to-sql-name database-name))
  (if (member database-name (list-databases :size nil) :test 'equal) t nil))

(defun database-size (&optional (name nil))
  "Given the name of a database, will return the name, a pretty-print string of
the size of the database and the size in bytes. If a database name is not provided,
it will return the result for the currently connected database."
  (unless name
    (setf name (current-database)))
  (first (query
          (:select 'datname
                   (:pg-size-pretty
                    (:pg-database-size 'pg-database.oid))
                   (:pg-database-size 'pg-database.oid)
                   :from 'pg-database
                   :where (:= 'datname '$1))
          (to-sql-name name))))

(defun list-databases (&key (order-by-size nil) (size t))
  "Returns a list of lists where each sub-list contains the name of the
database, a pretty-print string of the size of that database and the size in bytes.
The default order is by database name. Pass t as a parameter to :order-by-size for order by size.
Setting size to nil will return just the database names in a single list
ordered by name. This function excludes the template databases."
  (if order-by-size
      (setf order-by-size (sql (:desc (:pg-database-size 'pg-database.oid))))
      (setf order-by-size " datname"))
  (cond (size
         (query
          (:order-by
           (:select 'datname
                    (:pg-size-pretty
                     (:pg-database-size 'pg-database.oid))
                    (:pg-database-size 'pg-database.oid)
                    :from 'pg-database
                    :where (:not (:like 'datname "template%")))
           (:raw order-by-size))))
        (t
         (loop for x in (query
                         (:order-by
                          (:select 'datname
                                   :from 'pg-database
                                   :where (:not (:like 'datname "template%")))
                          (:raw order-by-size)))
            collect (first x)))))


;;;; Schemas
(defun list-schemas ()
  "List schemas in the current database, excluding the pg_* system schemas."
  (loop for x in (query (:select 'nspname
                                 :from 'pg_namespace
                                 :where (:!~* 'nspname "^pg_.*")))
       collect (first x)))

;;;; Tablespaces
(defun list-tablespaces ()
  "Lists the tablespaces in the currently connected database."
  (loop for x in (query (:order-by (:select (:as 'spcname 'name)
                                            :from 'pg_tablespace)
                                   'spcname))
       collect (first x)))

(defun list-available-types ()
  "List the available types in this postgresql version."
  (query (:select 'oid (:as (:format-type :oid :NULL) 'typename)
                  :from 'pg-type
                  :where (:= 'typtype "b"))))


;;; Table info
(defun list-table-sizes (&key (schema "public") (order-by-size nil) (size t))
  "Returns a list of lists (table-name, size in 8k pages) of tables in the current database.
Providing a name to the schema parameter will return just the information for tables in that schema.
It defaults to just the tables in the public schema. Setting schema to nil will return all tables, indexes etc
in the database in descending order of size. This would include system tables, so there
are a lot more than you would expect. If :size is set to nil, it returns only a flat list of table names.
Setting order-by-size to t will return the result in order of size instead of by table name."
  (setf schema (to-sql-name schema))
  (if order-by-size
      (setf order-by-size (sql (:desc 'relpages)))
      (setf order-by-size " relname"))
  (cond ((and size schema)
         (query (:order-by (:select 'relname 'relpages
                                    :from 'pg_class
                                    :where (:in 'relname
                                                (:set (:select 'table-name
                                                               :from 'information-schema.tables
                                                               :where (:= 'table-schema '$1)))))
                           (:raw order-by-size))
                schema))
         (size (query (:order-by (:select 'relname 'relpages
                                       :from 'pg_class)
                                 (:raw order-by-size))))
         (schema (query (:order-by (:select 'relname
                                    :from 'pg_class
                                    :where (:in 'relname
                                                (:set (:select 'table-name
                                                               :from 'information-schema.tables
                                                               :where (:= 'table-schema '$1)))))
                                   'relname)
                        schema))
         (t (loop for x in (query (:order-by (:select 'relname
                                        :from 'pg_class)
                                             'relname))
               collect (first x)))))


(defun table-size (table-name)
  "Return the size of a postgresql table in k or m. Table-name can be either a string or quoted."
  (query (:select (:pg_size_pretty (:pg_total_relation_size '$1)))
         :single
         (to-sql-name table-name)))

(defun more-table-info (table-name)
  "Returns more table info than table-description. Table can be either a string or quoted."
  (query (:order-by (:select (:as 'a.attnum 'ordinal-position)
                             (:as 'a.attname 'column-name)
                             (:as 'tn.typname 'data-type)
                             (:as 'a.attlen  'character-maximum-length)
                             (:as 'a.atttypmod 'modifier)
                             (:as 'a.attnotnull 'notnull)
                             (:as 'a.atthasdef 'hasdefault)
                             :from (:as 'pg_class 'c)
                             (:as 'pg_attribute 'a)
                             (:as 'pg_type 'tn)
                             :where (:and
                                     (:= 'c.relname '$1)
                                     (:> 'a.attnum 0)
                                     (:= 'a.attrelid 'c.oid)
                                     (:= 'a.atttypid 'tn.oid)))
                    'a.attnum)
         (to-sql-name table-name)))

;; Columns
(defun list-columns (table-name)
  "Returns a list of strings of just the column names in a table.
Pulls info from the postmodern table-description function
rather than directly."
  (when (table-exists-p table-name)
    (loop for x in (table-description table-name)
       collect (first x))))

(defun list-columns-with-types (table-name)
  "Return a list of (name type) lists for the fields of a table. Goes directly to the pg-catalog tables."
  (setf table-name (to-sql-name table-name))
  (when (table-exists-p table-name)
    (query
     (:select (:as 'a.attname 'column)
              (:as (:pg-catalog.format_type 'a.atttypid  'a.atttypmod)
                   'datatype)
              :from (:as 'pg-catalog.pg-attribute 'a)
              :where (:and
                      (:> 'a.attnum 0)
                      (:not 'a.attisdropped)
                      (:= 'a.attrelid
                          (:select
                           'c.oid
                           :from (:as 'pg-catalog.pg-class 'c)
                           :left-join (:as 'pg-catalog.pg-namespace 'n)
                           :on (:= 'n.oid 'c.relnamespace)
                           :where (:and
                                   (:= 'c.relname '$1)
                                   (:pg-catalog.pg-table-is-visible 'c.oid))))))
     table-name)))

(defun column-exists-p (table-name column-name)
  "Determine if a particular column exists. Table name and column-name can be either strings or symbols."
  (query (:select 'attname :from 'pg_attribute
		  :where (:= 'attrelid
			     (:select 'oid :from 'pg-class
				      :where (:and (:= 'relname '$1)
						   (:= 'attname '$2)))))
         (to-sql-name table-name) (to-sql-name column-name)
         :single))

;;; Views
(defun describe-views (&optional (schema "public"))
  "Describe the current views in the specified schema. Defaults to public schema."
  (setf schema (to-sql-name schema))
  (query
   (:order-by
    (:select 'c.oid 'c.xmin 'c.relname
	     (:as (:pg_get_userbyid 'c.relowner) 'viewowner)
	     'c.relacl 'description
	     (:as (:pg_get-viewdef 'c.oid 't) 'code)
	     :from (:as 'pg_class 'c)
	     :left-join (:as 'pg_description 'des)
	     :on (:and (:= 'des.objoid 'c.oid)
		       (:= 0 'des.objsubid))
	     :left-join (:as 'pg_catalog.pg_namespace 'n)
	     :on (:= 'n.oid 'c.relnamespace)
	     :where
             (:and
              (:or
               (:and 'c.relhasrules
                     (:exists
                      (:select 'r.rulename
                               :from (:as 'pg_rewrite 'r)
                               :where (:and (:= 'r.ev_class 'c.oid)
                                            (:= (:bpchar 'r.ev_type)
                                                (:type "I" bpchar))))))
               (:= 'c.relkind (:type "v" char)))
              (:= 'n.nspname '$1)))
    'relname)
   schema))

;;;; Functions
(defun list-database-functions ()
  "Returns a list of the functions in the database from the information_schema."
  (query (:select 'routine-name :from 'information-schema.routines
                  :where
                  (:and
                   (:not-in 'specific-schema
                            (:set "pg_catalog" "information-schema"))
                   (:!= 'type-udt-name "trigger")))))

;;;; Indices
(defun list-indices (&optional strings-p)
  "Return a list of the indexs in a database. Turn them into keywords if strings-p is not true."
  (let ((result (query (make-list-query "i") :column)))
    (if strings-p result (mapcar 'from-sql-name result))))

(defun list-table-indices (table-name &optional strings-p)
  "List the index names and the related columns in a table. "
  (when (table-exists-p (to-sql-name table-name))
    (let ((result (query
                   (:order-by
                    (:select
                     (:as 'i.relname 'index-name) (:as 'a.attname 'column-name)
                     :from (:as 'pg-class 't1) (:as 'pg-class 'i) (:as 'pg-index 'ix)
                     (:as 'pg-attribute 'a)
                     :where
                     (:and (:= 't1.oid 'ix.indrelid)
                           (:= 'i.oid 'ix.indexrelid)
                           (:= 'a.attrelid 't1.oid)
                           (:= 'a.attnum (:any* 'ix.indkey))
                           (:= 't1.relkind "r")
                           (:= 't1.relname '$1)))
                    'i.relname)
                   (to-sql-name table-name))))
      (if strings-p result (mapcar 'from-sql-name result)))))

(defun list-indexed-column-and-attributes (table-name)
  "List the indexed columns and their attributes in a table. Includes primary key."
  (setf table-name (to-sql-name table-name))
  (when (table-exists-p table-name)
   (query
    (:select 'pg_attribute.attname
             (:format_type 'pg_attribute.atttypid 'pg_attribute.atttypmod)
             :from 'pg_index 'pg_class 'pg_attribute
             :where (:and (:= 'pg_class.oid (:type '$1 :regclass))
                          (:= 'indrelid 'pg_class.oid)
                          (:= 'pg_attribute.attrelid 'pg_class.oid)
                          (:= 'pg_attribute.attnum
                              (:any* 'pg_index.indkey))))
    table-name)))

(defun list-index-definitions (table-name)
  "Returns a list of the definitions used to create the current indexes for the table."
  (setf table-name (to-sql-name table-name))
  (when (table-exists-p table-name)
    (query (:select (:pg_get_indexdef 'indexrelid)
                    :from 'pg_index
                    :where (:= 'indrelid (:type '$1 :regclass)))
	   table-name)))

;;;; Keys
(defun list-foreign-keys (table-name)
  "List the foreign keys in a table."
  (setf table-name (to-sql-name table-name))
  (when (table-exists-p table-name)
  (query (:select 'tc.constraint_name
                  'tc.table_name 'kcu.column_name
                  (:as 'ccu.table_name 'foreign_table_name)
                  (:as 'ccu.column_name 'foreign_column_name)
                  :from (:as 'information_schema.table_constraints 'tc)
                  :inner-join
                  (:as 'information_schema.key_column_usage 'kcu)
                  :on (:= 'tc.constraint_name 'kcu.constraint_name)
                  :inner-join
                  (:as 'information_schema.constraint_column_usage 'ccu)
                  :on (:= 'ccu.constraint_name 'tc.constraint_name)
                  :where (:and (:= 'constraint_type "FOREIGN KEY")
                               (:= 'tc.table_name '$1)))
         table-name)))

;;;; Constraints
(defun list-unique-or-primary-constraints (table-name)
  "List constraints on a table."
  (setf table-name (to-sql-name table-name))
  (when (table-exists-p table-name)
    (query (:select
            'relname
            :from 'pg-class
            :where
            (:in 'oid (:select 'indexrelid
                               :from 'pg-index 'pg-class
                               :where (:and
                                       (:= 'pg-class.relname '$1)
                                       (:= 'pg-class.oid 'pg-index.indrelid)
                                       (:or (:= 'indisunique "t")
                                            (:= 'indisprimary "t"))))))
           table-name)))

(defun list-all-constraints (table-name)
  "Uses information_schema to list all the constraints in a table. Table-name
can be either a string or quoted."
  (setf table-name (to-sql-name table-name))
  (when (table-exists-p table-name)
    (query (:select 'constraint-name 'constraint-type
		    :from 'information-schema.table-constraints
		    :where (:= 'table-name '$1))
	   table-name)))

(defun describe-constraint (table-name constraint-name)
  "Return a list of alists of the descriptions a particular constraint given
the table-name and the  constraint name using the information_schema
table."
  (setf table-name (to-sql-name table-name))
  (when (table-exists-p table-name)
    (first
     (query
      (:select 'tc.constraint-name
               'tc.constraint-type
               'tc.table-name
               'kcu.column-name
               'tc.is-deferrable
               'tc.initially-deferred
               (:as 'rc.match-option 'match-type)
               (:as 'rc.update-rule 'on-update)
               (:as 'rc.delete-rule 'on-delete)
               (:as 'ccu.table-name 'references-table)
               (:as 'ccu.column-name 'references-field)
               :from (:as 'information-schema.table-constraints 'tc)
               :left-join (:as 'information-schema.key-column-usage 'kcu)
               :on (:and (:= 'tc.constraint-catalog 'kcu.constraint-catalog)
                         (:= 'tc.constraint-schema 'kcu.constraint-schema)
                         (:= 'tc.constraint-name 'kcu.constraint-name))
               :left-join (:as 'information-schema.referential-constraints
                               'rc)
               :on (:and (:= 'tc.constraint-catalog 'rc.constraint-catalog)
                         (:= 'tc.constraint-schema 'rc.constraint-schema)
                         (:= 'tc.constraint-name 'rc.constraint-name))
               :left-join (:as 'information-schema.constraint-column-usage
                               'ccu)
               :on (:and (:= 'rc.unique-constraint-catalog
                             'ccu.constraint-catalog)
                         (:= 'rc.unique-constraint-schema
                             'ccu.constraint-schema)
                         (:= 'rc.unique-constraint-name 'ccu.constraint-name))
               :where (:and (:= 'tc.table-name '$1)
                            (:= 'tc.constraint-name
                                (to-sql-name constraint-name))))
      table-name :alists))))

(defun describe-foreign-key-constraints ()
  "Generates a list of lists of information on the foreign key constraints"
  (query (:order-by (:select 'conname
                             (:as 'conrelid 'table)
                             (:as 'pgc.relname 'tabname)
                             (:as 'a.attname 'columns)
                             (:as 'confrelid 'foreign-table)
                             (:as 'pgf.relname 'ftabname)
                             (:as 'af.attname 'fcolumn)
                             :from
                             (:as 'pg_attribute 'af)
                             (:as 'pg_attribute 'a)
                             (:as 'pg_class 'pgc)
                             (:as 'pg_class 'pgf)
                             (:as
                              (:select 'conname 'conrelid 'confrelid
                                       (:as (:[] 'conkey 'i) 'conkey)
                                       (:as (:[] 'confkey 'i) 'confkey)
                                       :from (:as
                                              (:select
                                               'conname
                                               'conrelid 'confrelid
                                               'conkey 'confkey
                                               (:as
                                                (:generate-series
                                                 '1
                                                 (:array-upper 'conkey 1))
                                                'i)
                                               :from 'pg_constraint
                                               :where (:= 'contype "f" ))
                                              'ss)
                                       ) 'ss2)
                             :where (:and (:= 'af.attnum 'confkey)
                                          (:= 'af.attrelid 'confrelid)
                                          (:= 'a.attnum 'conkey)
                                          (:= 'a.attrelid 'conrelid)
                                          (:= 'pgf.relfilenode 'confrelid)
                                          (:= 'pgc.relfilenode 'conrelid)))
                    'ftabname 'fcolumn 'tabname 'columns)))


;;;; Triggers
(defun list-triggers (&optional table-name)
  "List distinct trigger names from the information_schema table. Table-name can be either quoted or string."
  (if table-name
      (progn
        (setf table-name (to-sql-name table-name))
        (when (table-exists-p table-name)
          (loop for x in (query
                          (:select (:as 'trg.tgname 'trigger-name)
                                   :from (:as 'pg-trigger 'trg) (:as 'pg-class 'tbl)
                                   :where (:and (:= 'trg.tgrelid 'tbl.oid)
                                                (:= 'tbl.relname '$1)))
                          table-name)
               collect (first x))))
      (loop for x in (query
                      (:select 'trigger-name :distinct
                               :from 'information-schema.triggers
                               :where
                               (:not-in 'trigger-schema
                                        (:set "pg-catalog" "information-schema"))))
           collect (first x))))

(defun list-detailed-triggers ()
  "List detailed information on the triggers from the information_schema table."
  (query
   (:select '*
            :from 'information-schema.triggers
            :where
            (:not-in 'trigger-schema
                     (:set "pg_catalog" "information_schema")))))



;;; Roles
(defun list-database-users ()
  "List database users."
  (loop for x in (query (:order-by
                         (:select 'usename :from 'pg_user)
                         'usename))
     collect (first x)))


(defun change-toplevel-database (new-database user password host)
  "Just changes the database assuming you are using a toplevel connection.
Recommended only for development work."
  (disconnect-toplevel)
  (connect-toplevel (to-sql-name new-database) user password host)
  (current-database))

(defun list-connections ()
  "Returns info from pg_stat_activity on open connections"
  (query (:select '* :from 'pg-stat-activity)))
