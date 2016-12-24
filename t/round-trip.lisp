(in-package :cl-user)
(defpackage cl-yaml-test.round-trip
  (:use :cl :fiveam)
  (:export :round-trip)
  (:documentation "Round-trip Emitter/Parser tests."))
(in-package :cl-yaml-test.round-trip)

(def-suite round-trip
    :description "YAML emitter/parser tests.")
(in-suite round-trip)

(defclass invoice ()
  ((number :initarg :number :accessor invoice-number)
   (date :initarg :date :accessor invoice-date)
   (bill-to :initarg :bill-to :accessor invoice-bill-to)
   ;; skip ship-to until aliases and anchors are more developed)
   (product :initarg :product :accessor invoice-product)
   (tax :initarg :tax :accessor invoice-tax)
   (total :initarg :total :accessor invoice-total)
   (comments :initarg :comments :accessor invoice-comments)))

(defclass address ()
  ((lines :initarg :lines :accessor address-lines)
   (city :initarg :city :accessor address-city)
   (state :initarg :state :accessor address-state)
   (postal :initarg :postal :accessor address-postal)))

(defclass product-order ()
  ((sku :initarg :sku :accessor product-order-sku)
   (quantity :initarg :quantity :accessor product-order-quantity)
   (description :initarg :description :accessor product-order-description)
   (price :initarg :price :accessor product-order-price)))

(defun make-test-invoice ()
  (make-instance 'invoice
		 :number 34843
		 :date "2001-01-23"
		 :bill-to (list "Chris" "Dumars"
				(make-instance 'address
					       :lines "458 Walkman Dr.
Suite #292"
					       :city "Royal Oak"
					       :state "MI"
					       :postal "48046"))
		 :product (list (make-instance 'product-order
					       :sku "BL394D"
					       :quantity 4
					       :description "Basketball"
					       :price 450.00)
				(make-instance 'product-order
					       :sku "BL4438H"
					       :quantity 1
					       :description "Super Hoop"
					       :price 2392.00))
		 :tax 251.42
		 :total 4443.52
		 :comments "Late afternoon is best. Backup contact is Nancy Billsmer @ 338-4338."))

;;; Methods to emit CLOS objects

(defmethod yaml.emitter:emit-object (emitter (obj address))
  (yaml.emitter:emit-mapping (emitter :style :block-mapping-style)
    (yaml.emitter:emit-scalar emitter "lines")
    (yaml.emitter:emit-scalar emitter (address-lines obj)
			      :style :literal-scalar-style)
    (yaml.emitter:emit-scalar emitter "city")
    (yaml.emitter:emit-scalar emitter (address-city obj))
    (yaml.emitter:emit-scalar emitter "state")
    (yaml.emitter:emit-scalar emitter (address-state obj))
    (yaml.emitter:emit-scalar emitter "postal")
    (yaml.emitter:emit-scalar emitter (address-postal obj))))

(defmethod yaml.emitter:emit-object (emitter (obj product-order))
  (yaml.emitter:emit-mapping (emitter :style :block-mapping-style)
    (yaml.emitter:emit-scalar emitter "sku")
    (yaml.emitter:emit-scalar emitter (product-order-sku obj))
    (yaml.emitter:emit-scalar emitter "quantity")
    (yaml.emitter:emit-scalar emitter (product-order-quantity obj))
    (yaml.emitter:emit-scalar emitter "description")
    (yaml.emitter:emit-scalar emitter (product-order-description obj))
    (yaml.emitter:emit-scalar emitter "price")
    (yaml.emitter:emit-scalar emitter (product-order-price obj))))

(defmethod yaml.emitter:emit-object (emitter (obj invoice))
  (yaml.emitter:emit-mapping (emitter :style :block-mapping-style
				      :tag "clarkevans.com,2002:invoice")
    (yaml.emitter:emit-scalar emitter "invoice")
    (yaml.emitter:emit-scalar emitter (invoice-number obj))
    (yaml.emitter:emit-scalar emitter "date")
    (yaml.emitter:emit-scalar emitter (invoice-date obj))
    (yaml.emitter:emit-scalar emitter "bill-to")
    (yaml.emitter:emit-mapping (emitter :style :block-mapping-style)
      (yaml.emitter:emit-scalar emitter "given")
      (yaml.emitter:emit-scalar emitter (first (invoice-bill-to obj)))
      (yaml.emitter:emit-scalar emitter "family")
      (yaml.emitter:emit-scalar emitter (second (invoice-bill-to obj)))
      (yaml.emitter:emit-scalar emitter "address")
      (yaml.emitter:emit-object emitter (third (invoice-bill-to obj))))
    (yaml.emitter:emit-scalar emitter "product")
    (yaml.emitter:emit-sequence (emitter :style :block-sequence-style)
      (dolist (product (invoice-product obj))
	(yaml.emitter:emit-object emitter product)))
    (yaml.emitter:emit-scalar emitter "tax")
    (yaml.emitter:emit-scalar emitter (invoice-tax obj))
    (yaml.emitter:emit-scalar emitter "total")
    (yaml.emitter:emit-scalar emitter (invoice-total obj))
    (yaml.emitter:emit-scalar emitter "comments")
    (yaml.emitter:emit-scalar emitter (invoice-comments obj)
			      :plain-implicit t
			      :quoted-implicit t
			      :style :plain-scalar-style)))

;;; Functions to construct objects from YAML

(defun construct-address (mapping)
  (make-instance 'address
		 :lines (gethash "lines" mapping)
		 :city (gethash "city" mapping)
		 :state (gethash "state" mapping)
		 :postal (gethash "postal" mapping)))

(defun construct-product-order (mapping)
  (make-instance 'product-order
		 :sku (gethash "sku" mapping)
		 :quantity (gethash "quantity" mapping)
		 :description (gethash "description" mapping)
		 :price (gethash "price" mapping)))

(defun construct-invoice (mapping)
  (let ((bill-to-mapping (gethash "bill-to" mapping)))
    (make-instance 'invoice
		   :number (gethash "invoice" mapping)
		   :date (gethash "date" mapping)
		   :bill-to (list (gethash "given" bill-to-mapping)
				  (gethash "family" bill-to-mapping)
				  (construct-address (gethash "address" bill-to-mapping)))
		   :product (mapcar #'construct-product-order
				    (gethash "product" mapping))
		   :tax (gethash "tax" mapping)
		   :total (gethash "total" mapping)
		   :comments (gethash "comments" mapping))))

;;; Register a construction function with a YAML tag

(eval-when (:load-toplevel)
  (yaml.parser:register-mapping-converter "clarkevans.com,2002:invoice"
					  #'construct-invoice))

;;; Helper

(defun test-emit (obj)
  (yaml.emitter:with-emitter-to-string (emitter)
    (yaml.emitter:emit-stream (emitter)
      (yaml.emitter:emit-document (emitter :implicit t)
	(yaml.emitter:emit-object emitter obj)))))

;;; Round-trip test

(test invoice
  (let* ((invoice-a (make-test-invoice))
	 (yaml-a (test-emit invoice-a))
	 (invoice-b (yaml:parse yaml-a))
	 (yaml-b (test-emit invoice-b)))
    (is (string= yaml-a yaml-b))))

