\version "2.16.0"
\relative c' { r8. d4 cis8.. d4 f a c\breve }

#(use-modules (ice-9 regex)
	     (srfi srfi-1)
	     (lily)
	     (oop goops))

#(define (dtd-header)
  (string-append
   "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>
   <!DOCTYPE score-partwise PUBLIC \"-//Recordare//DTD MusicXML 3.0 Partwise//EN\" \"http://www.musicxml.org/dtds/partwise.dtd\">\n"
  ))
  
#(define xml-output (open-file "output.xml" "w"))
#(define print (lambda(x) (display (musicxml-node->string x) xml-output)))
#(define print-all (lambda(mylist) (map (lambda (x) (print x)) mylist)))

#(define ly2musicxml-pitches
  '((0 . "C")
    (1 . "D")
    (2 . "E")
    (3 . "F")
    (4 . "G")
    (5 . "A")
    (6 . "B")))
#(define ly2musicxml-accidentals
  '((-1 . "flat-flat")
    (-1/2 . "flat")
    (1/2 . "sharp")
    (1 . "double-sharp")))
#(define ly2musicxml-types
  '((1/128 . "128th")
    (1/64 . "64th")
    (1/32 . "32nd")
    (1/16 . "16th")
    (1/8 . "eighth")
    (1/4 . "quarter")
    (1/2 . "half")
    (1 . "whole")
    (2 . "breve")
    (4 . "long")
    (8 . "maxima")))
#(define ly2musicxml-divisions 6720)

#(define xml-entities-alist
  '(("\"" . "&quot;")
    ("<" . "&lt;")
    (">" . "&gt;")
    ("'" . "&apos;")
    ("&" . "&amp;")))

#(define (repeat what n)
  (if (<= n 1)
     (list what)
     (append (list what) (repeat what (- n 1)))))

#(define-class <xml-node> ()
  (name #:init-value "" #:accessor node-name #:init-keyword #:name)
  (value #:init-value "" #:accessor node-value #:init-keyword #:value)
  (attributes #:init-value '()
	      #:accessor node-attributes
	      #:init-keyword #:attributes)
  (children #:init-value '()
	    #:accessor node-children
	    #:init-keyword #:children))

#(define-method (append-child (node <xml-node>) (child <xml-node>))
       (set! (node-children node) (append (node-children node) (list child))))

#(define (musicxml-node->string node)
  (let ((xml-name (node-name node)))
    (string-append
     (if (and (equal? (node-value node) "") (equal? (node-children node) '()))
	(if xml-name (open-close-tag xml-name (node-attributes node)) "")
	(string-append
	    (if xml-name (open-tag xml-name (node-attributes node)) "")
	    (if (equal? (node-value node) "")
		(string-append
		(if xml-name "\n" "")
		(apply string-append (map musicxml-node->string (node-children node))))
		(node-value node))
	    (if xml-name (close-tag xml-name) "")))
     (if xml-name "\n" ""))))

#(define (measure->xml-node number implicit)
    (make <xml-node>
      #:name 'measure
      #:attributes
      (apply
       append
	(if (integer? number) '((number . number)) '())
	(if (or (equal? implicit "yes") (equal? implicit "no")) '((implicit . implicit)) '())
	'())))

#(define (note->xml-node event)
  (let* ((pitch (ly:event-property event 'pitch))
	(alteration (ly:pitch-alteration pitch))
	(dlength (ly:event-property event 'length))
	(duration (ly:event-property event 'duration))
	(dots (ly:duration-dot-count duration)))
    (make <xml-node>
      #:name 'note
      #:children
      (apply
       append
	(if (ly:pitch? pitch) (list (pitch->xml-node pitch)) '())
	(if (ly:moment? dlength) (list (duration->xml-node dlength)) '())
	(if (ly:moment? dlength) (list (type->xml-node dlength)) '())
	(if (!= alteration 0) (list (accidental->xml-node alteration)) '())
	(if (>= dots 1) (repeat (make-xml-node 'dot) dots) '())
	'()))))

#(define (rest->xml-node event)
  (let* ((dlength (ly:event-property event 'length))
	(duration (ly:event-property event 'duration))
	(dots (ly:duration-dot-count duration)))
    (make <xml-node>
      #:name 'note
      #:children
      (apply
       append
	(list (make-xml-node 'rest))
	(if (ly:moment? dlength) (list (duration->xml-node dlength)) '())
	(if (ly:moment? dlength) (list (type->xml-node dlength)) '())
	(if (>= dots 1) (repeat (make-xml-node 'dot) dots) '())
	'()))))

#(define (pitch->xml-node pitch)
  (let* ((step (ly:pitch-notename pitch))
	(octave (ly:pitch-octave pitch))
	(alteration (ly:pitch-alteration pitch)))
    (make <xml-node>
      #:name 'pitch
      #:children
      (apply
       append
       (list (step->xml-node step))
       (if (!= alteration 0) (list (alter->xml-node alteration)) '())
       (list (octave->xml-node octave))
       '()))))

#(define (step->xml-node step)
    (make <xml-node>
      #:name 'step
      #:value (ly:assoc-get step ly2musicxml-pitches)))

#(define (alter->xml-node alteration)
    (make <xml-node>
      #:name 'alter
      #:value (number->string (* 2 alteration))))

#(define (octave->xml-node octave)
    (make <xml-node>
      #:name 'octave
      #:value (number->string (+ 4 octave))))

#(define (duration->xml-node duration)
  (make <xml-node>
    #:name 'duration
    #:value (number->string (* (* 4 ly2musicxml-divisions) (/ (ly:moment-main-numerator duration) (ly:moment-main-denominator duration))))))

#(define (accidental->xml-node alteration)
    (make <xml-node>
      #:name 'accidental
      #:value (ly:assoc-get alteration ly2musicxml-accidentals)))

#(define (type->xml-node duration)
  (make <xml-node>
    #:name 'type
    ;; transforms the rational fraction to the largest rational power of 2 not larger than  the fraction
    #:value (ly:assoc-get (expt 2 (inexact->exact (floor (/ (log (/ (ly:moment-main-numerator duration) (ly:moment-main-denominator duration))) (log 2))))) ly2musicxml-types)))

#(define (make-xml-node name)
    (make <xml-node>
      #:name name))

#(define (assert x)
  (if x
      #t
      (ly:error (_ "assertion failed: ~S") x)))

#(define (re-sub re to string)
  (regexp-substitute/global #f re string 'pre to 'post))

#(define (re-sub-alist string alist)
  (if (null? alist)
      string
      (re-sub (caar alist) (cdar alist)
	      (re-sub-alist string (cdr alist)))))

#(define (dump-attr sym-val)
    (let* ((sym (car sym-val))
	   (val (cdr sym-val)))

      (string-append
       " "
       (symbol->string sym)
       "=\""
       (let ((s (call-with-output-string (lambda (port) (display val port)))))
	 (re-sub-alist s xml-entities-alist))
       "\"")))

#(define (open-tag tag attrs)
  (string-append
   "<" (symbol->string tag)
   (apply string-append (map dump-attr attrs))
   ">"))

#(define (open-close-tag tag attrs)
  (string-append
   "<" (symbol->string tag)
   (apply string-append (map dump-attr attrs))
   "/>"))

#(define (close-tag name)
  (string-append "</" (symbol->string name) ">"))

#(define (music-to-musicxml music port)
  "Dump MusicXML-ish stuff to @var{port}."

  (display (dtd-header) port)
  (display (open-tag 'music '((type . score))) port)
;;  (display (musicxml-node->string (music->xml-node music)) port)
  (display (close-tag 'music) port))

\layout {
  \context {
    \Voice
    \consists
    #(let ((instance-counter 0))
      (lambda (context)
       (set! instance-counter (1+ instance-counter))
       (let ((instance-id instance-counter))
	(make-engraver
	 (listeners
	  ((note-event engraver event)
	   (print (note->xml-node event)))
	  ((rest-event engraver event)
	   (print (rest->xml-node event)))
	  ((music-event engraver event)
	   (print (measure->xml-node 1 "yes"))))
	 ((finalize trans)
	  (display "\nFinalize context\n\n")
	  (display context)
	  (display (list "\n\n"
		    (ly:context-current-moment
		     (ly:translator-context trans)) "\n\n")))))))
  }
}