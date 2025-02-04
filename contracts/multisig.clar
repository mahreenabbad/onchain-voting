;; Owner
(define-constant contract-owner tx-sender)

;; Errors
(define-constant err-owner-only (err u100))
(define-constant err-already-locked (err u101))
(define-constant err-more-votes-than-members-required (err u102))
(define-constant err-not-a-member (err u103))
(define-constant err-votes-required-not-met (err u104))

;; Variables
(define-data-var members (list 100 principal) (list))
(define-data-var votes-required uint u1) ;;Defines the minimum number of votes required for approval.
(define-map votes {member: principal, recipient: principal} {decision: bool})


(define-public (start (new-members (list 100 principal)) (new-votes-required uint))
    (begin
        (asserts! (is-eq contract-caller contract-owner) err-owner-only)
        (asserts! (is-eq (len (var-get members)) u0) err-already-locked)
        (asserts! (>= (len new-members) new-votes-required) err-more-votes-than-members-required)
        (var-set members new-members)
        (var-set votes-required new-votes-required)
        (ok true)
    )
)

(define-public (vote (recipient principal) (decision bool))
    (begin
        (asserts! (is-some (index-of (var-get members) contract-caller)) err-not-a-member)
        (ok (map-set votes {member: tx-sender, recipient: recipient} {decision: decision}))
    )
)
(define-read-only (get-vote (member principal) (recipient principal))
    (default-to false (get decision (map-get? votes {member: member, recipient: recipient})))
)

(fold accumulator-function input-list initial-value)

;;A helper function to count the total votes.
(define-private (tally (member principal) (accumulator uint))
    (if (get-vote member tx-sender) (+ accumulator u1) accumulator)
)
;;Aggregates all votes to get the total count.

(define-read-only (tally-votes)
;;fold to loop through all members and apply tally function
    (fold tally (var-get members) u0)
)


(if
    (get-vote member tx-sender) ;; The condition (boolean expression).
    (+ accumulator u1)          ;; Value to return if the condition is true.
    accumulator                 ;; Value to return if the condition is false.
)

;;;;;;;;;;;;;;;;;;;;;;
(define-public (withdraw)
    (let
        (
            (recipient tx-sender)
            (total-votes (tally-votes))
        )
        ;;Allows withdrawal only if enough votes have been cast.
        (asserts! (>= total-votes (var-get votes-required)) err-votes-required-not-met)
        ;;Transfers all STX from contract balance to the caller
        (try! (as-contract (stx-transfer? (stx-get-balance tx-sender) tx-sender recipient)))
        (ok total-votes)
    )
)
;;;;;;;;;;;;;;;;;;;;;;
(define-public (deposit (amount uint))
    (stx-transfer? amount tx-sender (as-contract tx-sender))
)
