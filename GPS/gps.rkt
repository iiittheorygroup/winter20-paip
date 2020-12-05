; General Problem Solver - GPS

; IDEA -> A single computer program to solve any problem
; Exaggerated by important because separated it's problem solving strategy from it's knowledge of particular problems.

; 5 stages

; 1. Describe the problem
; 2. Specify in algorithmic terms
; 3. Implement the problem
; 4. Test

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Describing the problem :-

;       Embodies the heuristic of means-end analysis. (What problem we solve by what means)
;       Basic system of GPS
;       A problem is solved either by taking appropriate action directly, or by first
;       solving for the preconditions of an appropriate action and then taking the action. A definition of `appropriateness` is required.
;       We arbitrarily decide that the problem description is complete and move on to the specification

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Specification :-

; Representing goal state and current state as a set of conditions
; List of allowable operators
;     Operator: Structure composed of an action, a list of preconditions and a list of effects. Original GPS had this general idea, but flexibility leads to inefficiency. So we restrict the list of effects into `add-list` and `delete-list`.

; Complete Problem:- Starting state, Goal state and set of known operators (Can you build a TM out of it? Yes ofc, but not really sure about an `automata`)
; Ex- (GPS '("unknown" "poor") '("rich" "famous") <list-of-operations>)

; Trivial solving for goal state (if present done, otherwise continue)
; Operator:- appropriate if goal state is reachable.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Implementation :-

; (define state)            |-> Current state and list of conditions
; (define ops)              |-> A list of available operators
;                           |-> (action nil) (precond nil) (add-list nil) (del-list nil)
; (struct operation)        |-> Action, precondition and add/remove op
; (define GPS)              |-> Given state, goals and op return for each state if achieve_goal == #t
; (define achieve)          |-> If achieved, return #t else find appropriate-p
; (define appropriate-p)    |-> op is appropriate for a goal if it's in its add list
; (define apply-op)         |-> Applies the op and updates the state

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TEST

; Note that using elementary datums helps in keeping track of the total search space

; Pose problems to the GPS and examine the solutions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Lack of Intermediate Information
;       |-> Solved by the p-dbg 
