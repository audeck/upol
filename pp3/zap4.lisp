#|

Dependencies:
 - P(rototype)OOP embedded language @ "Zdrojové kódy/10 src"
    - load.lisp
    - 10_basics.lisp

|#

;;; 4. Prototypy:
; 6 z úkolů 1-9,
; 2 operace z úkolu 10,
; 1 z úkolů 11-15.

;;; ---------------------------- Úkol #1 ---------------------------- ;;;

; Doplňte do jazyka logickou negaci (zpráva not) a disjunkci (zpráva or).

; not:
[true add "NOT" :value false]
[false add "NOT" :value true]

; or:
[true add "OR" :value true]
[false add "OR" :value {(arg1) arg1}]

; testy:
; [[true not] name] -> "FALSE"
; [[false not] name] -> "TRUE"
;
; [[true or true] name] -> "TRUE"
; [[true or false] name] -> "TRUE"
; [[false or true] name] -> "TRUE"
; [[false or false] name] -> "FALSE"




;;; ---------------------------- Úkol #4 ---------------------------- ;;;

; Přidejte primitivum error na vyvolání chyby. Informace o chybě by měla
; obsahovat i text s chybovou hláškou (jak je v Lispu obvyklé). Primitivum
; byste umístili do objektu Object, do Lobby, nebo jinam?
; 
; A: Osobne do Lobby, stejne jako napriklad metodu force.

; error:
[lobby add "ERROR" :value (lambda (self arg1) (error arg1))]

; test (bezne pouziti):
; [{} error "Something went wrong!"]



;;; ---------------------------- Úkol #5 ---------------------------- ;;;

; Lispový výraz (random n) vrací pseudonáhodné číslo od nuly včetně 
; do n vyjma. Napište primitivum na generování pseudonáhodných čísel
; pomocí funkce random. Kam byste je umístili?
; 
; A: Osobne do objektu Number.

; random:
[number add "RANDOM" :value #'random]

; test:
; (dotimes (i 10) [[10 random] print])




;;; ---------------------------- Úkol #7 ---------------------------- ;;;

; Doplňte do jazyka seznamy tak, že vytvoříte prototyp tečkového páru a
; prázdný seznam. Tečkový pár by měl rozumět zprávám car a cdr. Je potřeba
; vytvořit prototyp pro obecný seznam? (Tj. jak prázdný, tak neprázdný?)
; Zdůvodněte.
;
; A: Obecny (cisty) seznam je definovan pouze pomoci prazdneho seznamu (nil)
;    a teckovych paru (cons). Zacatek seznamu by mohl mit obalku list, ale 
;    ta by byla jen navic.
;
; NOTE: Jako seznam chapu zde (i v dalsich ukolech) cisty seznam
;       (i.e. (1 2 3) seznam je, ale (1 2 . 3) seznam neni), jelikoz ackoli
;       (typep '(1 2 . 3) 'list) se vyhodnocuje na T,
;       (length '(1 2 . 3)) z jasnych duvodu vraci chybu.

;; cons:
[lobby add "CONS" :value [object clone]]
[cons set-name "CONS"]
[cons add "CAR"]  ; defaults to :value nihil
[cons add-setter "CAR"]
[cons add "CDR"]
[cons add-setter "CDR"]

;; nil (used exclusively to represent empty lists):
[lobby add "NIL" :value [object clone]]
[nil set-name "NIL"]



;;; ---------------------------- Úkol #8 ---------------------------- ;;;

; Napište zprávu is-list, která vrátí true, pokud je příjemce seznam,
; jinak vrátí false.

;; is-list:
[object add "IS-LIST" :value false]
[nil add "IS-LIST" :value true]
[cons add "IS-LIST" :value {() [[self cdr] is-list]}]

#| testy:
[[3 is-list] name]            -> "FALSE"
[["Hello" is-list] name]      -> "FALSE"
[[nil is-list] name]          -> "TRUE"

; (1 2 "three")
[lobby add "MY-LIST" :value [[[cons clone] set-car 1] set-cdr
                             [[[cons clone] set-car 2] set-cdr
                              [[[cons clone] set-car "three"] set-cdr
                               [nil clone]]]]]
[[my-list is-list] name] -> "TRUE"

; (1 2 . "three")
[[my-list cdr] set-cdr "three"]
[[my-list is-list] name] -> "FALSE"
|#



;;; ---------------------------- Úkol #9 ---------------------------- ;;;

; Definujte zprávu length pro zjišťování délky seznamu vyjádřenou esoterickým
; číslem. Např. výraz [list length] vrátí délku seznamu list.

;; zjišťování délky seznamu:
[nil add "LENGTH" :value [0 esoteric]]
[cons add "LENGTH" :value {() [[1 esoteric] + [[self cdr] length]]}]

#| testy:
[[nil length] name] -> 0

[lobby add "MY-LIST" :value [[[cons clone] set-car 1] set-cdr
                             [[[cons clone] set-car 2] set-cdr
                              [[[cons clone] set-car "three"] set-cdr
                               [nil clone]]]]]
[[my-list length] name] -> 3

[[my-list cdr] set-cdr "three"]
[[my-list length] name] -> Error (jehoz chybova zprava by mohla byt zmenena
                                  v [cons length] za pomoci if-true)
|#



;;; ---------------------------- Úkol #10 ---------------------------- ;;;

; Definujte další operace pro seznamy, např. spojování, prohledávání,
; mazání prvku, převrácení.

;; kopírování:
[nil add "COPY" :value {() [nil clone]}]
[cons add "COPY" :value {() [[[cons clone] set-car [self car]]
                                           set-cdr [[self cdr] copy]]}]

;; spojování:
[nil add "APPEND" :value {(arg1) [[arg1 is-list] 
                                  if-true [arg1 copy]
                                  :else {() [{} error "Can only append lists"]}]}]
; chybuje (Field cdr not found) pokud nekonci nil (= neni cisty seznam)
[cons add "APPEND" :value {(arg1) [[[cons clone] set-car [self car]]
                                                 set-cdr [[self cdr] append arg1]]}]

;; mazání prvku (NOTE: (remove 3 '(1 2 (3 5) 3)) -> (1 2 (3 5))):
[nil add "DELETE" :value {(arg1) [self copy]}]
[cons add "DELETE" :value {(arg1) [[[self car] equals arg1]
                                   if-true [[self cdr] delete arg1]
                                   :else [[[cons clone] set-car [self car]]
                                          set-cdr [[self cdr] delete arg1]]]}]

#| testy:
; (1 2 2)
[lobby add "MY-LIST" :value [[[cons clone] set-car 1] set-cdr
                             [[[cons clone] set-car 2] set-cdr
                              [[[cons clone] set-car 2] set-cdr
                               [nil clone]]]]]
; (4 5)
[lobby add "MY-LIST2" :value [[[cons clone] set-car 4] set-cdr
                              [[[cons clone] set-car 5] set-cdr
                               [nil clone]]]]

[my-list append my-list2] -> (1 2 2 4 5)

[my-list delete 2] -> (1)
[[my-list delete 2] delete 1] -> ()
|#



;;; ---------------------------- Úkol #13 ---------------------------- ;;;

; Definujte násobení ~~a podle výběru i další operace~~ s esoterickými čísly.

;; násobení:
[zero add "*" :value {(arg1) self}]
[one add "*" :value {(arg1) arg1}]
[lobby add "TWO" :value [one succ]]
[two add "*" :value {(arg1) [[[self super] * arg1] + arg1]}]

#| testy:
[[[0 esoteric] * [3 esoteric]] name] -> 0
[[[1 esoteric] * [3 esoteric]] name] -> 3
[[[6 esoteric] * [3 esoteric]] name] -> 18
|#
