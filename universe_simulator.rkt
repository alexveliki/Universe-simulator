#lang racket


(define hb "Higgs-boson")
(define zb "Z-boson")
(define wb "W-boson")


(define tq "Top-quark")
(define taq "Top-antiquark")

(define uq "Up-quark")
(define uaq "Up-antiquark")

(define dq "Down-quark")
(define daq "Down-antiquark")

(define bq "Bottom-quark")
(define baq "Bottom-antiquark")

(define chq "Charm-quark")
(define chaq "Charm-antiquark")

(define sq "Strange-quark")
(define saq "Strange-antiquark")


(define tl "Tau-lepton")
(define atl "Antitau-lepton")

(define muon "Muon")
(define amuon "Antimuon")

(define photon "Photon")
(define gluon "Gluon")
(define positron "Positron")
(define electron "Electron")

(define neutrino "Neutrino")
(define aneutrino "Anti-neutrino")



(define (get-random) (random 1000000))

(define (higgs-decay-check) (< (get-random) 433))

(define (higgs-decay)
  (define x (get-random))
  (cond ((< x 216)    (list tq taq ))
        ((< x 460)    (list muon amuon))
        ((< x 1570)   (list zb photon))
        ((< x 3800)   (list photon photon))
        ((< x 19700)  (list zb zb))
        ((< x 52400)  (list chq chaq))
        ((< x 122800) (list tl atl))
        ((< x 211000) (list gluon gluon))
        ((< x 352000) (list wb wb))
        ((< x 1000000)(list bq baq))))

(define (boson-decay-check) (< (get-random) 500000))

(define (w-boson-decay)
  (define x (get-random))
  (cond ((< x 333333) (list positron neutrino))
        ((< x 666666) (list amuon neutrino))
        ((< x 999999) (list atl neutrino))
        (else (w-boson-decay))))

(define (z-boson-decay)
  (define x (get-random))
  (cond ((< x 206000) (list neutrino aneutrino))
        ((< x 240000) (list electron positron))
        ((< x 274000) (list tl atl))
        ((< x 308000) (list muon amuon))
        ((< x 460000) (list dq daq))
        ((< x 612000) (list sq saq))
        ((< x 764000) (list bq baq))
        ((< x 882000) (list uq uaq))
        (else         (list chq chaq))))

(define (quark-decay-check) (< (get-random) 129500))

(define (top-quark-decay)
  (define x (get-random))
  (cond ((< x 333333) (list wb dq))
        ((< x 666666) (list wb sq))
        ((< x 999999) (list wb bq))
        (else (top-quark-decay))))

(define (top-antiquark-decay)
  (define x (get-random))
  (cond ((< x 333333) (list wb daq))
        ((< x 666666) (list wb saq))
        ((< x 999999) (list wb baq))
        (else (top-antiquark-decay))))


(define unstable-particles (list hb wb zb tq taq))

(define (is-unstable x)
  (if(member x unstable-particles) #t #f))

(define (has-unstable lst)
  (if (null? lst) #f
      (if (not (pair? lst)) (is-unstable lst)
      (if (is-unstable (car lst)) #t
          (has-unstable (cdr lst))))))

(define (decay-all-unstable lst)
  (if (null? lst) '()
      (cond ((and (equal? (car lst) hb) (higgs-decay-check)) (append (higgs-decay) (decay-all-unstable (cdr lst))))
            ((and (equal? (car lst) wb) (boson-decay-check)) (append (w-boson-decay) (decay-all-unstable (cdr lst))))
            ((and (equal? (car lst) zb) (boson-decay-check)) (append (z-boson-decay) (decay-all-unstable (cdr lst))))
            ((and (equal? (car lst) tq) (quark-decay-check)) (append (top-quark-decay) (decay-all-unstable (cdr lst))))
            ((and (equal? (car lst) taq)(quark-decay-check)) (append (top-antiquark-decay) (decay-all-unstable (cdr lst))))
            (else (append (list (car lst)) (decay-all-unstable (cdr lst)))))))



(define all-particles (list hb wb zb tq taq uq uaq dq daq bq baq chq chaq sq saq tl atl muon amuon photon gluon positron electron neutrino aneutrino))


(define (print-symbol s x)
  (if (= x 1) s
      (string-append s (print-symbol s (- x 1)))))

(define (print-lines x) (print-symbol "|" x))
(define (print-dots x)  (print-symbol "." x))
(define (print-spaces x)(print-symbol " " x))

(define (get-percentage-of-total x lst total)
  (define cnt (length (filter (lambda (y) (equal? y x)) lst)))
  (round (* 100 (/ cnt total))))

(define (make-percentage-line x lst)
  (define percentage (get-percentage-of-total x lst (length lst)))
  (cond ((> percentage 0) (display (print-lines percentage))))
  (cond ((< percentage 100) (display (print-dots (- 100 percentage))))))

(define (print-particle-info x lst)
  (display x)
  (display (print-spaces (- 25 (string-length x))))
  (make-percentage-line x lst)
  (display (print-spaces 5))
  (display (length (filter (lambda (y) (equal? y x)) lst)))
  (display " (")
  (display (/ (* 100.0 (length (filter (lambda (y) (equal? y x)) lst))) (length lst)))
  (display "%)")
  (display "\n"))

(define (make-graph-for-all-particles lst)
  (define (Iter particles lst)
    (cond ((pair? particles)  (print-particle-info (car particles) lst) (Iter (cdr particles) lst))))
  (Iter all-particles lst))


(define (slow-down x)
  (cond ((> x 0) (slow-down (- x 1)))))

(define (visualise-particles time lst)
  ;(sleep 0.1)
  ;(display "\n")
  ;(display "\n")
  ;(display "\n")
  (display "\n")
  (display "\n")
  (display "tick: ")
  (display time)
  (display "\n")
  (make-graph-for-all-particles lst)
  (display (print-spaces 24))
  (display 0)
  (display "%")
  (display (print-spaces 97))
  (display 100)
  (display "%")
  (display "\n")
  

  )
  
  
  

(define (simulate-universe lst)
  (define (Iter current prev time)
    (cond ((not (equal? current prev)) (display time) (display ": ") (display current) (display "\n")))
    (if (has-unstable current) (Iter (decay-all-unstable current) current (+ time 1))
        (display "All particles in the universe are stable")))
  (Iter lst '() 1))


(define (simulate-universe-with-visualization lst)
  (define (Iter current prev time)
    (cond ((not (equal? current prev)) (visualise-particles time current)))
    (if (has-unstable current) (Iter (decay-all-unstable current) current (+ time 1))
        (display "All particles in the universe are stable")))
  (Iter lst '() 1))
   

(define (generate-higgs x)
  (if (= x 0) '()
      (append (list hb) (generate-higgs (- x 1)))))
;;;;;;;;;;;;;;;;





















;;;;;;;;;;;;;;;