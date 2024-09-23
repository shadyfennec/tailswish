main:
    push #38            ; Divide 38...
    push #5             ; by 5.
    call divide         ; Call the function, top of the stack is remainder, below
                        ; is the result.
main_loop:
    jmp main_loop

;; Divides the second-to-top number on the stack by the top number on the stack.
;;
;; State of the stack at call time:
;;     a b
;; Where 'a' is the divided and 'b' is the divisor.
;;
;; State of the stack at return:
;;     x y
;; Where 'x' is the result of the division and 'y' is the remainder.
;; 
;; In the comments for the function, 'c' denotes the counter for the result of
;; the division, initialized at 0. 
divide:
    push #0             ;   a b c=0
divide_loop:
    rot                 ;   c a b
    dup                 ;   c a b b
    rot                 ;   c b a b
    sub                 ;   c b a-b
    jv divide_end       ; If a-b has underflowed, the division is done.
    rot                 ;   a-b c b
    rot                 ;   b a-b c
    inc                 ;   b a-b c+1
    swp                 ;   b c+1 a-b
    rot                 ;   a-b b c+1
    jmp divide_loop     ; And repeat.
divide_end:             ; At this point, we have the following:
                        ;   x b y-b
    add                 ;   x b+y-b=y
    ret                 ;   x y