(export find-package)
(import :std/srfi/1 :std/format)

;; Define a class
(defclass package
  (expander-context)
  constructor: %make-package)

(defmethod  {%make-package package}
  (lambda (self e)
    (class-instance-init! self)
    (set! (@ self expander-context ) e)
    self))

;; Cache the expander-context.
(def package<-expander-context-hash
  (make-hash-table-eq))

(def (package<-expander-context expander-context)
  (or (hash-get package<-expander-context-hash expander-context)
      (let (package (make-package  expander-context))
	(begin0 package
	  (hash-put! package<-expander-context-hash expander-context package)))))

;; package-name
(def (package-name package)
  (gx#expander-context-id
   (package-expander-context package)))

;; Find
(def package<-symbol-hash
  (make-hash-table-eq))

(def (package<-symbol symbol)
  (or (let (package (hash-get package<-symbol-hash symbol))
	(if (and (package? package)
		 (eq? (package-name package) symbol))
	  package
	  #f))
      (let ((package-list (filter (lambda (p) (eq? (package-name p) symbol))
				  (list-all-packages))))
	(if (pair? package-list)
	  (begin0 (car package-list)
	    (hash-put! package<-symbol-hash symbol (car package-list)))
	  #f))))

(def (package<-string name)
  (package<-symbol (string->symbol name)))

(def (find-package name)
  (cond
   ((package? name) name)
   ((string? name)
    (package<-string name))
   ((symbol? name)
    (package<-symbol name))
   ((gx#expander-context? name)
    (package<-expander-context name))
   (#t (error (format "Not a valid package designator: ~A" name)))))


;; List
(def (list-all-packages)
 (map find-package
      (delete-duplicates
       (filter
	gx#module-context?
	(map cdr (hash->list (gx#current-expander-module-registry)))))))

(def (package-external-symbols package)
  (set! package (find-package package))
  (let* ((context (package-expander-context package))
	 (exports (gx#module-context-export
		   context))
	 (symbols (map gx#module-export-name exports)))
    #;(values (map gx#module-export-name exports) exports)
    symbols))
