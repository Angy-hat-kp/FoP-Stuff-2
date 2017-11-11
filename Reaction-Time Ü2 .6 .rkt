;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |Reaction-Time Ü2 .6 |) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct person (age sex code))

(define-struct subject (person times))

(define VP01 (make-subject (make-person 22 'm 'MW17K) (list 220 301 189 272 311)))
(define VP02 (make-subject (make-person 25 'f 'MP25G) (list 234 197 253 257 206)))
(define VP03 (make-subject (make-person 23 'f 'CT03R) (list 197 202 214 222 233)))
(define VP04 (make-subject (make-person 20 'm 'MM09R) (list 273 314 257 264 217)))
(define VP05 (make-subject (make-person 19 'm 'KR22I) (list 198 197 228 253 199)))
(define VP06 (make-subject (make-person 26 'm 'FR01B) (list 212 204 289 294 223)))
(define VP07 (make-subject (make-person 28 'f 'RA15R) (list 258 323 189 247 303)))
(define VP08 (make-subject (make-person 22 'm 'RP18R) (list 221 307 182 271 316)))
(define VP09 (make-subject (make-person 24 'f 'GH31W) (list 230 295 304 264 237)))
(define VP10 (make-subject (make-person 19 'f 'OM29Q) (list 299 194 242 303 243)))

(define subjects (list VP01 VP02 VP03 VP04 VP05 VP06 VP07 VP08 VP09 VP10))




;; Exercise 6.1

;; youngest-oldest-subject: list symbol -> number
;; Explanation: youngest-oldest-subject gibt entweder das Alter des jüngsten oder des ältesten in der Liste
;; subject wieder. Bei einer leeren Liste wird 0 ausgegeben
;; Example: (youngest-oldest-subject subject 'youngest) -> 19
(define (youngest-oldest-subject people comparator)
  (cond  [ (and (empty? people) (symbol=? comparator 'youngest)) +inf.0]
         [ (and (empty? people) (symbol=? comparator 'oldest)) 0]
         [(symbol=? comparator 'youngest) (first (sort-auf (subject-age-list people)))]
         [(symbol=? comparator 'oldest) (first (sort-ab (subject-age-list people)))]))

;; Tests
(check-expect (youngest-oldest-subject (list VP01 VP03 VP05) 'youngest) 19)
(check-expect (youngest-oldest-subject (list VP01 VP03 VP07) 'oldest) 28)
(check-expect (youngest-oldest-subject empty 'oldest) 0)


;; HILFSPROZEDUREN

;;subject-age-list: list of subjects -> list of numbers
;; Explanation: subject-age-list erstellt eine Liste die nur das Alter der Probanten enthält.
;; Example: (subject-age-list (list VP01 VP02 VP03) -> (22 25 23)
(define (subject-age-list subjects)
  (cond [(empty? subjects) empty]
        [else (cons(person-age(subject-person(first subjects))) (subject-age-list (rest subjects)))]
         ))
;; Tests subject-age-list
(check-expect(subject-age-list (list VP01 VP02 VP03)) (list 22 25 23))
(check-expect(subject-age-list (list VP02 VP04 VP06)) (list 25 20 26))
(check-expect(subject-age-list empty) empty)


;; sort-auf: list of number -> list of numbers
;; Explanation: sort-auf sortiert eine Liste aufsteigend.
;; Example: (sort-auf (list 2 9 5 3))->(2 3 5 9)
(define (sort-auf list)
  (cond [(empty? list) empty]
        [else (insert-auf (first list)(sort-auf (rest list)))]))
;; Tests sort-auf
(check-expect (sort-auf (list 2 9 5 3))(list 2 3 5 9))
(check-expect (sort-auf(list 6 3 9))(list 3 6 9))
(check-expect (sort-auf empty) empty)


;; sort-ab: list of number -> list of numbers
;; Explanation: sort-ab sortiert eine Liste absteigend.
;; Example:(sort-ab (list 2 9 5 3)) -> (9 5 3 2)
(define (sort-ab list)
  (cond [(empty? list) empty]
        [else (insert-ab (first list) (sort-ab (rest list)))]))
;; Tests sort-ab
(check-expect (sort-ab (list 2 9 5 3))(list 9 5 3 2))
(check-expect (sort-ab(list 6 3 9))(list 9 6 3))
(check-expect (sort-ab empty) empty)


;; insert-auf: number list -> list
;; Explanation: insert-auf fügt ein Element vor allen Elementen die größer sind ein, in einer Sortierten Liste.
;; Example:(insert-auf 5 (list 3 6 8)) -> ( 3 5 6 8)
(define (insert-auf x list)
  (cond [(empty? list) (cons x empty)]
        [else (cond [(<= x (first list)) (cons x list)]
                    [(> x(first list))
                         (cons (first list) (insert-auf x (rest list)))])]))
;; Tests insert-auf
(check-expect (insert-auf 5 (list 3 6 8))(list 3 5 6 8))
(check-expect (insert-auf 7 (list 3 6 8))(list 3 6 7 8))
(check-expect (insert-auf 3 empty) (list 3))

;; insert-ab: number list -> list 
;; Explanation: insert-ab fügt ein Element vor allen Elementen die kleiner sind ein, in einer Sortierten Liste.
;; Example:(insert-ab 5 (list 8 6 3)) -> (8 6 5 3)
(define (insert-ab x list)
  (cond [(empty? list) (cons x empty)]
        [else (cond [(>= x (first list)) (cons x list)]
                    [(< x(first list))
                         (cons (first list) (insert-ab x (rest list)))])]))
;; Tests insert-ab
(check-expect (insert-ab 5 (list 8 6 3))(list 8 6 5 3))
(check-expect (insert-ab 7 (list 8 6 3))(list 8 7 6 3))
(check-expect (insert-ab 3 empty) (list 3))







;; Exercise 6.2

;; balanced-sex: list -> boolean
;; Explanation: balanced-sex überprüft die ob die gleiche Anzahl an männlichen und
;; weiblichen Probanten an der Studie teilgenommen haben. Sollte dies der Fall sein wird true ausgegeben,
;; andernfalls false.
;; Example: (balanced-sex (list VP01 VP02 VP03 VP04)) -> true
(define(balanced-sex people)
  (cond [(empty? people) (error "no_subjects_existing")]
        [ (equal? (count-sex (subject-sex-list people) 'f 0) (count-sex (subject-sex-list people) 'm 0))true]
        [else false]))
;; Tests
(check-expect (balanced-sex (list VP01 VP02 VP03 VP04)) true)
(check-expect (balanced-sex (list VP01 VP02 VP03 VP10)) false) 
(check-error (balanced-sex empty) "no_subjects_existing")


;; HILFSPROZEDUREN

;;subject-sex-list: list of subjects -> list of numbers
;; Explanation: subject-sex-list erstellt eine Liste die nur das Geschlecht der Probanden enthält.
;; Example: (subject-sex-list (list VP01 VP02 VP03) -> ('m 'f 'f)
(define (subject-sex-list subjects)
  (cond [(empty? subjects) empty]
        [else (cons(person-sex(subject-person(first subjects))) (subject-sex-list (rest subjects)))]))
;; Tests subject-sex-list
(check-expect (subject-sex-list (list VP01 VP02 VP03))(list 'm 'f 'f))
(check-expect (subject-sex-list (list VP01 VP02 VP04))(list 'm 'f 'm))
(check-expect (subject-sex-list empty) empty)

;; count-sex: list symbol number -> number
;; Explanation: count-sex zählt die Anzahl eines Geschlecht in einer List von Gschlechtern, je nach dem welches Symbol übergeben wird.
;; Example:(count-sex(list 'm 'f 'f) 'f 0) -> 2
(define (count-sex list f s)
  (cond [(empty? list) s]
        [(equal?(first list) f)
         (count-sex (rest list) f (+ s 1))]
        [else (count-sex (rest list) f s)]))
;; Tests count-female
(check-expect(count-sex(list 'm 'f 'f) 'f 0) 2)
(check-expect(count-sex(list 'm 'f 'f 'm 'f 'f 'm) 'm 0) 3)
(check-expect(count-sex empty 'f 0) 0)







;; Exercise 6.3

;; mean-of-subject: liste symbol-> number
;; Explanation: mean-of-subject berechnet den Durchschnitt der Reaktionszeit des Probanden mit dem Versuchspersonencode "code",
;; aus einer Liste von Probanden "people"
;; Example:(mean-of-subject(list VP01 VP02) 'MW17K) -> 258.6
(define (mean-of-subject people code)
  (mean (subject-times (get-code people code))))
;; Tests
(check-within(mean-of-subject(list VP01 VP02) 'MW17K) 258.6 0.001) 
(check-within(mean-of-subject(list VP01 VP02 VP03) 'CT03R) 213.6 0.001)


;;HILFSPROZEDUREN

;; get-code: list symbol -> 
;; Explanation: get-code 
;; Example:  (get-code (list VP01 VP02 VP03)'MW17K) -> VP01
(define(get-code subjects code)
  (cond [(empty? subjects) empty]
        [(symbol=? code (person-code (subject-person (first subjects)))) (first subjects)]
        [else (get-code (rest subjects) code)]))
;; Tests get-code
(check-expect (get-code (list VP01 VP02 VP03)'MW17K) VP01)
(check-expect (get-code empty 'MW17K) empty)

;; mean: list -> number
;; Explanation: mean
;; Example:(mean (list 220 301 189 272 311)) -> 258.6
(define (mean list)
  (cond [(empty? list) empty]
        [else (/ (sum-times list) (count-times list))]))
;;Tests mean
(check-within(mean (list 220 301 189 272 311)) 258.6 0.001)
(check-expect(mean empty) empty)


;; sum-times: list -> number
;; Explanation: sum-times addiert alle Elemente einer Liste
;; Example: (sum-times (list 220 301 189 272 311)) -> 1313
(define (sum-times list)
  (cond [(empty? list) 0]
        [else (+ (first list) (sum-times (rest list)))]))
;; Tests sum-times
(check-expect (sum-times (list 220 301 189 272 311)) 1293)
(check-expect (sum-times empty) 0)

;; count-times list -> number
;;Explanation: count-times zählt, aus wie vielen Elementen eine Liste besteht.
;;Example: (count-times (list 6 8 2 9 0)) -> 5
(define (count-times list)
  (cond [(empty? list) 0]
        [else (+ (count-times (rest list)) 1)]))
;;Test
(check-expect (count-times (list 7 3 5 6)) 4)
(check-expect (count-times (list 0 1 2)) 3)




;; Excercise 6.4
;; std-of-subject: list symbol -> number
;; Explanation: std-of-subject berechnet die Standartabweichung der Reaktionszeiten einer Person mit dem Versuchspersonencode "code" aus einer Liste von
;; Probanden "people"
;; Example: (std-of-subject (list VP01 VP03) 'MW17K) -> 52.576

(define (std-of-subject people code)
  (s (subject-times (get-code people code))))

;;Tests
(check-within(std-of-subject (list VP02 VP03) 'MP25G) 27.097 0.001)
(check-within(std-of-subject (list VP05 VP01) 'KR22I) 24.909 0.001)

;;HILFSPROZEDUREN


;; s: list of number -> number
;;Explanation: s berechnet die Standartabweichung einer Liste
;;Example: (s (list 5 7 3 8)) -> 2.217

(define (s list)
  (cond [(empty? list )0]
        [else (sqrt (* (/ 1(- (count-times list) 1)) (sum-mean list (mean list))))]))

;;Tests
(check-within(s (list 22 3 9 6 3))7.893 0.001)
(check-within(s (list 9 5 0 4 1))3.563 0.001)

;; sum-mean: list of number number -> number
;; Explanation: sum-mean berechnet die Differenz aus dem ersten Element einer Liste von Zahlen und dem Druchschnitt "mean"
;; und quadriert das Ergebnis. Die Prozedur wird so lange wiederholt, bis jedes Element
;; aus der Liste ein mal vorgekommen ist.

(define (sum-mean list mean)
  (cond [(empty? list )0]
        [else (+ (sqr (- (first list) mean)) (sum-mean (rest list) mean))]))

;;Tests
(check-expect (sum-mean (list 1 4 7) 4) 18)
(check-expect (sum-mean (list 1 2) 3) 5)  


