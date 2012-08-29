\version "2.16.0"

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

#(define-class <xml-node> ()
  (name #:init-value "" #:accessor node-name #:init-keyword #:name)
  (value #:init-value "" #:accessor node-value #:init-keyword #:value)
  (attributes #:init-value '()
	      #:accessor node-attributes
	      #:init-keyword #:attributes)
  (children #:init-value '()
	    #:accessor node-children
	    #:init-keyword #:children))

#(define (musicxml-node->string node)
  (let ((xml-name (node-name node)))
    (string-append
     (if xml-name (open-tag xml-name '() '()) "")
     (if (equal? (node-value node) "")
	 (string-append
	  (if xml-name "\n" "")
	  (apply string-append (map musicxml-node->string (node-children node))))
	 (node-value node))
     (if xml-name (close-tag xml-name) "")
     (if xml-name "\n" ""))))

#(define (xml-node->string node)
  (string-append
   "\n"
   (open-tag (node-name node) (node-attributes node) '())
   (if (equal? (node-value node) "")
       (string-append
	(apply string-append (map xml-node->string (node-children node))))
       (node-value node))
   "\n"
   (close-tag (node-name node))))

#(define (note->xml-node event)
  (let* ((pitch (ly:event-property event 'pitch))
	(alteration (ly:pitch-alteration pitch))
	(duration (ly:event-property event 'length)))
    (make <xml-node>
      #:name 'note
      #:children
      (apply
       append
	(if (ly:pitch? pitch) (list (pitch->xml-node pitch)) '())
	(if (ly:moment? duration) (list (duration->xml-node duration)) '())
	(if (ly:moment? duration) (list (type->xml-node duration)) '())
	(if (!= alteration 0) (list (accidental->xml-node alteration)) '())
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
    #:value (ly:assoc-get (/ (ly:moment-main-numerator duration) (ly:moment-main-denominator duration)) ly2musicxml-types)))

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

#(define (open-tag tag attrs exceptions)
  (define (candidate? x)
    (not (memq (car x) exceptions)))

  (define (dump-attr sym-val)
    (let* ((sym (car sym-val))
	   (val (cdr sym-val)))

      (string-append
       " "
       (symbol->string sym)
       "=\""
       (let ((s (call-with-output-string (lambda (port) (display val port)))))
	 (re-sub-alist s xml-entities-alist))
       "\"")))

  (string-append
   "<" (symbol->string tag)
   (apply string-append (map dump-attr (filter candidate? attrs)))
   ">"))

#(define (close-tag name)
  (string-append "</" (symbol->string name) ">"))

#(define (music-to-xml music port)
  "Dump XML-ish stuff to @var{port}."

  (display (dtd-header) port)
  (display (open-tag 'music '((type . score)) '()) port)
;;  (display (xml-node->string (music->xml-node music)) port)
  (display (close-tag 'music) port))

#(define (music-to-musicxml music port)
  "Dump MusicXML-ish stuff to @var{port}."

  (display (dtd-header) port)
  (display (open-tag 'music '((type . score)) '()) port)
;;  (display (musicxml-node->string (music->xml-node music)) port)
  (display (close-tag 'music) port))

\relative c' { c8 cis d4 f a c\breve c\maxima }

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
	   (print (note->xml-node event))))
	 ((finalize trans)
	  (display "\nFinalize context\n\n")
	  (display context)
	  (display (list "\n\n"
		    (ly:context-current-moment
		     (ly:translator-context trans)) "\n\n"))
	 )))))
  }
}