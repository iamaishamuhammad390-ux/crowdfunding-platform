;; ============================================================
;; Contract: crowdfunding-platform
;; Purpose : Goal-based crowdfunding with refunds
;; ============================================================

;; ------------------------------------------------------------
;; Error Codes
;; ------------------------------------------------------------
(define-constant ERR-INVALID-AMOUNT        (err u600))
(define-constant ERR-CAMPAIGN-NOT-FOUND   (err u601))
(define-constant ERR-CAMPAIGN-ACTIVE      (err u602))
(define-constant ERR-CAMPAIGN-NOT-SUCCESS (err u603))
(define-constant ERR-CAMPAIGN-NOT-FAILED  (err u604))
(define-constant ERR-NOT-CREATOR          (err u605))
(define-constant ERR-NO-CONTRIBUTION      (err u606))


;; replaced fancy em-dash with regular hyphens in comment
;; Public Functions - Campaign Creation
;; ------------------------------------------------------------

(define-data-var campaign-counter uint u0)

(define-map campaigns
  { id: uint }
  {
    creator: principal,
    goal: uint,
    pledged: uint,
    deadline: uint,
    withdrawn: bool
  }
)

(define-map contributions
  { id: uint, contributor: principal }
  { amount: uint }
)

(define-public (create-campaign (goal uint) (duration uint))
  (begin
    (asserts! (> goal u0) ERR-INVALID-AMOUNT)

    ;; Increment campaign ID
    (var-set campaign-counter (+ (var-get campaign-counter) u1))

    ;; Store campaign
    (map-set campaigns
      { id: (var-get campaign-counter) }
      {
        creator: tx-sender,
        goal: goal,
        pledged: u0,
        deadline: (+ stacks-block-height duration),
        withdrawn: false
      }
    )

    (ok (var-get campaign-counter))
  )
)

;; replaced fancy em-dash with regular hyphens in comment
;; Public Functions - Contributions
;; ------------------------------------------------------------

(define-read-only (get-contribution (id uint) (contributor principal))
  (default-to u0 (get amount (map-get? contributions { id: id, contributor: contributor })))
)

(define-public (contribute (id uint) (amount uint))
  (match (map-get? campaigns { id: id })
    campaign
    (begin
      (asserts! (> amount u0) ERR-INVALID-AMOUNT)
      (asserts! (< stacks-block-height (get deadline campaign)) ERR-CAMPAIGN-ACTIVE)

      ;; Transfer STX to contract
      (try!
        (stx-transfer? amount tx-sender (as-contract tx-sender))
      )

      ;; Update campaign pledged amount
      (map-set campaigns
        { id: id }
        {
          creator: (get creator campaign),
          goal: (get goal campaign),
          pledged: (+ (get pledged campaign) amount),
          deadline: (get deadline campaign),
          withdrawn: (get withdrawn campaign)
        }
      )

      ;; Update contributor record
      (let ((current (get-contribution id tx-sender)))
        (map-set contributions
          { id: id, contributor: tx-sender }
          { amount: (+ current amount) }
        )
      )

      (ok amount)
    )
    ERR-CAMPAIGN-NOT-FOUND
  )
)

;; replaced fancy em-dash with regular hyphens in comment
;; Public Functions - Creator Withdrawal
;; ------------------------------------------------------------

(define-read-only (is-successful (id uint))
  (match (map-get? campaigns { id: id })
    campaign
    (>= (get pledged campaign) (get goal campaign))
    false
  )
)

(define-public (withdraw (id uint))
  (match (map-get? campaigns { id: id })
    campaign
    (begin
      (asserts! (is-eq tx-sender (get creator campaign)) ERR-NOT-CREATOR)
      (asserts! (>= stacks-block-height (get deadline campaign)) ERR-CAMPAIGN-ACTIVE)
      (asserts! (is-successful id) ERR-CAMPAIGN-NOT-SUCCESS)
      (asserts! (not (get withdrawn campaign)) ERR-CAMPAIGN-ACTIVE)

      ;; Transfer pledged funds to creator
      (try!
        (stx-transfer?
          (get pledged campaign)
          (as-contract tx-sender)
          tx-sender
        )
      )

      ;; Mark withdrawn
      (map-set campaigns
        { id: id }
        {
          creator: (get creator campaign),
          goal: (get goal campaign),
          pledged: (get pledged campaign),
          deadline: (get deadline campaign),
          withdrawn: true
        }
      )

      (ok true)
    )
    ERR-CAMPAIGN-NOT-FOUND
  )
)

;; replaced fancy em-dash with regular hyphens in comment
;; Public Functions - Refunds
;; ------------------------------------------------------------

(define-public (refund (id uint))
  (let ((contributed (get-contribution id tx-sender)))
    (match (map-get? campaigns { id: id })
      campaign
      (begin
        (asserts! (>= stacks-block-height (get deadline campaign)) ERR-CAMPAIGN-ACTIVE)
        (asserts! (not (is-successful id)) ERR-CAMPAIGN-NOT-FAILED)
        (asserts! (> contributed u0) ERR-NO-CONTRIBUTION)

        ;; Refund contributor
        (try!
          (stx-transfer?
            contributed
            (as-contract tx-sender)
            tx-sender
          )
        )

        ;; Reset contribution
        (map-set contributions
          { id: id, contributor: tx-sender }
          { amount: u0 }
        )

        (ok contributed)
      )
      ERR-CAMPAIGN-NOT-FOUND
    )
  )
)
