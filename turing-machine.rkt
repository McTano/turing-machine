;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname turing-machine) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; Simulation of A Turing Machine

;; CONSTANTS

;; =========================
(define WIDTH 600)
(define HEIGHT 300)
(define MTS (empty-scene WIDTH HEIGHT))

(define CELL-SIZE 40)
(define CELL (square CELL-SIZE "outline" "black"))
(define EYE (square CELL-SIZE "outline"
                             (make-pen "red" 4 "solid" "round" "round")))
(define CELL-S0 (square CELL-SIZE "outline" "black"))
(define CELL-S1 (overlay CELL-S0 (text "1" 20 "black")))
(define ELLIPSIS (text "..." 20 "black"))

;; DATA DEFINITIONS

;; =========================

;; Cell is one-of:
;; - "0"
;; - "1"
;; interp. The two possible values for a cell.

(define S0 "0")
(define S1 "1")

#;
(define (fn-for-cell sn)
  (cond [(string=? S0 sn) (...)]
        [(string=? S1 sn) (...)]))

;; Template rules used:
;; -one-of: 2 cases
;; -atomic distinct: S0
;; -atomic distinct: S1

;; ----------------------

;; TapeSection is one-of:
;; -empty
;; -(cons Cell TapeSectionl)
;; interp. a continuous section of tape, s.t.:
;; -first cell is closest to the read/write head.
;; -empty means the tape is, from that point on, an infinite sequence of blank cells.

(define TS0 empty)
(define TS1 (list S1))
(define TS2 (cons S0 empty))
(define TS3 (cons S1 (cons S1 (cons S1 (cons S0 empty)))))
(define TS4 (cons S1 (cons S0 (cons S0 (cons S1 empty)))))

#;
(define (fn-for-ts ts)
  (cond [(empty? ts) (...)]
        [else
         (fn-for-cell (first ts))
         (fn-for-ts (rest ts))]))

;; Template rules used:
;; one-of: 2 cases
;; atomic distinct: empty
;; compound: 2 fields
;; Reference: (first ts) is Cell
;; Self-Reference: (rest ts) is TapeSection

;; ------------------------------------------

(define-struct tape (left head right))
;; Tape is (make-tape TapeSection Cell TapeSection)
;; interp:
;; - left is the tape to the left of the head, starting with the nearest cell
;; - head is the cell being scanned by the read-write-head
;; - right is the tape to the right of the head, starting with the nearest cell

(define T0 (make-tape empty S0 empty))
(define T1 (make-tape empty S1 empty))
(define T2 (make-tape empty S1 (list S1)))
(define T3 (make-tape empty S1 (list S1 S1)))
(define T4B5 (make-tape empty S1 (list S1 S1 S1
                                        S0
                                        S1 S1 S1 S1 S1)))

#;
(define (fn-for-tape t)
  (... (fn-for-ts (tape-left t)_
       (fn-for-cell (tape-head t))
       (fn-for-ts (tape-right t)))))

;; Template rules:
;; -compound: 3 fields
;; -Reference: (tape-left t) is TapeSection
;; -Reference: (tape-head t) is Cell
;; -Reference: (tape-right t) is cOfCell

;; -------------------------------------------

;; Action is one of:
;; - "LEFT"
;; - "RIGHT"
;; - S0
;; - S1
;; interp. the overt action to be performed: move left/right or print S1/S0

(define L "LEFT")
(define R "RIGHT")

#;
(define (fn-for-action act)
  (cond [(string=? act L) (...) ]
        [(string=? act R) (...) ]
        [(string=? act S0) (...) ]
        [(string=? act S1) (...) ]))

;; -----------------------------------------

(define-struct instruction (action next-qnum))
;; Instruction is one-of:
;; - "HALT"
;; - (make-instruction Action QNum)
;; when this instruction is triggered, do Action and go to the state qn

(define HALT "HALT")
(define S1-Q0 (make-instruction S1 0))
(define R-Q1 (make-instruction R 1))

#;
(define (fn-for-instruction instruct)
  (cond [(equal? HALT instruct) (...)]
        [else (... (fn-for-action (instruction-action instruct))
                   (instruction-next-qnum instruct))]))            ; QNum

;; -----------------------------------------

(define-struct qstate (if0 if1))
;; QState is (make-qstate Instruction Instruction)
;; interp. the actions to perform when scanning s0 or s1

(define Q0 (make-qstate HALT HALT))
(define Q1 (make-qstate S1-Q0 R-Q1))
(define HALTING-STATE Q0)

#;
(define (fn-for-qstate qn)
  (... (fn-for-instruction (qstate-if0))
       (fn-for-instruction (qstate-if1))))

;; Template-rules:
;; - compound: 2 fields
;; - Reference: (qstate-if0 qn) and (qstate-if1 qn) are Instruction

;; -----------------------------------------

;; Machine is one-of:
;; - empty
;; -(cons QState Machine)

;; [(first tm) should always be HALTING-STATE]

(define TM1 (list HALTING-STATE Q1))
(define TM:X+Y (list HALTING-STATE
                      (make-qstate ; q1
                       (make-instruction S1 2)
                       (make-instruction R 1))
                      (make-qstate ; q2
                       (make-instruction R 3)
                       (make-instruction L 2))
                      (make-qstate ; q3
                       (make-instruction R 0)
                       (make-instruction S0 3))))
(define TM:EX.4 (list HALTING-STATE
                      (make-qstate ;q1
                       (make-instruction S0 0)
                       (make-instruction R 2))
                      (make-qstate ;q2
                       (make-instruction L 0)
                       (make-instruction S1 3))
                      (make-qstate ;q3
                       (make-instruction S0 0)
                       (make-instruction R 1))))

#;
(define (fn-for-tm tm)
  (cond [(empty? tm) (...)]
        [else
         (fn-for-qstate (first tm))
         (fn-for-tm (rest tm))]))

;; Template rules:
;; -one-of: 2 cases
;; -atomic distinct: empty
;; -compound: 2 fields
;; -Reference: (first tm) is QState
;; -Self-reference: (rest tm) is Machine

;; -------------------------------------------

;; QNum is Natural
;; interp. Index of current state:
;; - By convention, 0 will be halting state, 1 will be initial state.

(define N0 0)
(define N1 1)
(define N3 3)

#;
(define (fn-for-qnum n)
  (... n))
;; Template rules:
;; - atomic non-distinct: Natural

;; -------------------------------------------

(define-struct config (tape qnum tm))
;; Config is (make-config Tape Natural Machine)
;; interp. the current configuration of the machine:
;; tape is the state of the tape
;; qnum is the index of the current state [0->halting, 1->initial]
(define C1 (make-config T1 1 TM1)) 
(define C-ONE-THIRD (make-config T0
                                 1
                                 (list Q0
                                       (make-qstate ;q1
                                        (make-instruction R 2)
                                        HALT)
                                       (make-qstate ;q2
                                        (make-instruction S1 2)
                                        (make-instruction R 1)))))
(define C:4+5 (make-config T4B5 1 TM:X+Y))
(define C:EX.4:1 (make-config T1 1 TM:EX.4))

#;
(define (fn-for-config cf)
  (fn-for-tape (config-tape cf))
  (fn-for-qnum (config-qnum cf))
  (fn-for-tm (config-tm cf)))

;; Template rules:
;; -compound: 3 fields
;; -Reference: (config-tape t) isTape
;; -Reference: (config-qnum t) is qnum
;; -Reference: (config-tm t) is Machine


;; Functions:
;; ===========================================

;; Config -> Config
;; start the world with some configuration
;; 
(define (main cf)
  (big-bang cf                     ; Config
            (on-tick   tock 1)     ; Config -> Config
            (to-draw   render)))   ; Config -> Image
            ;(stop-when ...)       ; Config -> Boolean
            ;(on-mouse  ...)       ; Config Integer Integer MouseEvent -> Config
            ;(on-key    ...)       ; Config KeyEvent -> Config

;; -----------------------------------------

;; Config -> Config
;; produce the next configuration
; (define (tock cf) cf) ; stub

(check-expect (tock C1)
              (make-config (make-tape (list S1) S0 empty)
                           1
                           (config-tm C1)))

(define (tock cf)
  (if (equal? (config-instruction cf) HALT)
      cf
      (make-config (perform-action (config-action cf) (config-tape cf))
                   (config-next-qnum cf)
                   (config-tm cf))))

;; -----------------------------------------

;; Action Tape -> Tape
;; Change the tape based on the given Action

(define (perform-action act t)
  (cond [(string=? act L) (shift-left t)]
        [(string=? act R) (shift-right t)]
        [(string=? act S0) (print S0 t)]
        [(string=? act S1) (print S1 t)]))

;; -----------------------------------------

;; Tape -> Tape
;; Move the head one cell to the left (tape moves right)

(check-expect (shift-left (make-tape (list S1 S0 S0) S0 (list S0 S1)))
              (make-tape (list S0 S0) S1 (list S0 S0 S1)))

(define (shift-left t)
  (make-tape (if (empty? (tape-left t))
                         empty
                         (rest (tape-left t)))   ; new left
             (if (empty? (tape-left t))
                 S0
                 (first (tape-left t)))          ; new head
             (cons (tape-head t)
                   (tape-right t))))             ; new right

;; -----------------------------------------

;; Tape -> Tape
;; Move the head one cell to the right (tape moves left)

(define (shift-right t)
       (make-tape (cons (tape-head t)            ; new left
                        (tape-left t))
                  (if (empty? (tape-right t))
                      S0
                      (first (tape-right t)))    ; new head
                  (if (empty? (tape-right t))
                      empty
                      (rest (tape-right t)))))        ; new right

;; -----------------------------------------

;; Cell Tape -> Tape
;; Replace the scanned cell (head) with sn

(define (print sn t)
  (make-tape (tape-left t)
       sn
      (tape-right t)))

;; -----------------------------------------

;; Config -> QState
;; Get the current QState from cf

(define (config-qn cf)
  (list-ref (config-tm cf) (config-qnum cf)))

;; -----------------------------------------

;; Config->Cell
;; Get the scanned cell from cf

(define (config-head cf)
  (tape-head (config-tape cf)))

;; -----------------------------------------

;; Config -> Instruction
;; Get the next instruction from c

(check-expect (config-instruction C1)
              (make-instruction R 1))
(check-expect (config-instruction (tock C1))
              (make-instruction S1 0))
(check-expect (config-instruction (tock (tock C1)))
              HALT)

(define (config-instruction cf)
  (if (string=? (config-head cf) S0)
                          (qstate-if0 (config-qn cf))
                          (qstate-if1 (config-qn cf))))

;; -----------------------------------------

;; Config -> Action
;; Get the next action from c

(define (config-action cf)
      (instruction-action (config-instruction cf)))

;; -----------------------------------------

;; Config -> QNum
;; Get the next qnum from c

(define (config-next-qnum cf)
  (instruction-next-qnum (config-instruction cf)))

;; -----------------------------------------

;; Config -> Image
;; render the current config

(check-expect (render C1)
              (tape->img (config-tape C1)))

(define (render cf)
  (overlay (tape->img (config-tape cf))
           MTS))

;; -----------------------------------------

;; Tape -> Image
;; render tape

(check-expect (tape->img T1)
  (overlay (beside ELLIPSIS
                   CELL
                   (overlay EYE (cell->img S1))
                   CELL
                   ELLIPSIS)
           MTS))
(check-expect (tape->img (config-tape (tock C1)))
  (overlay (beside ELLIPSIS
                   CELL-S0 CELL-S1
                   (overlay EYE CELL-S0)
                   CELL-S0
                   ELLIPSIS)
           MTS))

(define (tape->img t)
  (overlay (beside ELLIPSIS
                   (left->img (tape-left t))
                   (overlay EYE (cell->img (tape-head t)))
                   (right->img (tape-right t))
                   ELLIPSIS)
           MTS))

;; -----------------------------------------

;; TapeSection -> Image
;; Render the left section of the tape

(check-expect (left->img (list S1 S0 S1 S1 S0 S0))
              (beside CELL-S0 CELL-S1 CELL-S1 CELL-S0 CELL-S1))

(define (left->img left)
  (cond [(ts-empty? left) CELL]
        [else
         (beside (left->img (rest left))
                 (cell->img (first left)))]))

;; -----------------------------------------

;; TapeSection -> Image
;; Render the right section of the tape

(check-expect (right->img (list S1 S0 S1 S1 S0 S0))
              (beside CELL-S1 CELL-S0 CELL-S1 CELL-S1 CELL-S0))

(define (right->img right)
  (cond [(ts-empty? right) CELL]
        [else
         (beside (cell->img (first right))
                 (right->img (rest right)))]))

;; -----------------------------------------

;; Cell -> Image
;; Render the Cell as an image

(check-expect (cell->img S0) CELL-S0)
(check-expect (cell->img S1) CELL-S1)

(define (cell->img c)
  (cond [(string=? S0 c) CELL-S0]
        [(string=? S1 c) CELL-S1]))

;; -----------------------------------------

;; TapeSection -> Boolean
;; true if the TapeSection contains only S0

(check-expect (ts-empty? (list S1)) false)
(check-expect (ts-empty? (list S0)) true)
(check-expect (ts-empty? (list S0 S1)) false)
(check-expect (ts-empty? (list S0 S0)) true)

(define (ts-empty? ts)
  (or (empty? ts)
      (and (s0? (first ts))
           (ts-empty? (rest ts)))))

;; -----------------------------------------

;; Cell -> Boolean
;; true iff c = S0
(define (s0? c)
  (string=? S0 c))

;; -----------------------------------------

;; Cell -> Boolean
;; true iff c = S1
(define (s1? c)
  (string=? S1 c))

