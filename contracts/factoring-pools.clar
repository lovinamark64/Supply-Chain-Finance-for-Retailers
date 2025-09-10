;; Invoice Factoring Pools Contract
;; Enables multiple investors to pool resources for collective invoice purchases

;; Error constants
(define-constant err-owner-only (err u200))
(define-constant err-not-found (err u201))
(define-constant err-unauthorized (err u202))
(define-constant err-pool-full (err u203))
(define-constant err-insufficient-funds (err u204))
(define-constant err-pool-closed (err u205))
(define-constant err-already-member (err u206))
(define-constant err-invalid-amount (err u207))
(define-constant err-pool-active (err u208))

;; Constants
(define-constant contract-owner tx-sender)
(define-constant max-pool-members u10)

;; Data variables
(define-data-var next-pool-id uint u1)

;; Investment pool data structure
(define-map factoring-pools
  { pool-id: uint }
  {
    creator: principal,
    target-amount: uint,
    current-amount: uint,
    member-count: uint,
    status: (string-ascii 20),
    created-at: uint,
    invoice-id: (optional uint)
  }
)

;; Pool member participation tracking
(define-map pool-memberships
  { pool-id: uint, member: principal }
  {
    contribution: uint,
    profit-share: uint,
    withdrawn: bool
  }
)

;; Pool member list for iteration
(define-map pool-members-list
  { pool-id: uint, index: uint }
  { member: principal }
)

;; User pool tracking
(define-map user-pools
  { user: principal, pool-id: uint }
  { is-member: bool }
)

;; Read-only functions
(define-read-only (get-pool (pool-id uint))
  (map-get? factoring-pools { pool-id: pool-id })
)

(define-read-only (get-pool-membership (pool-id uint) (member principal))
  (map-get? pool-memberships { pool-id: pool-id, member: member })
)

(define-read-only (get-pool-member (pool-id uint) (index uint))
  (map-get? pool-members-list { pool-id: pool-id, index: index })
)

(define-read-only (get-total-pools)
  (- (var-get next-pool-id) u1)
)

(define-read-only (is-pool-member (pool-id uint) (user principal))
  (default-to 
    { is-member: false } 
    (map-get? user-pools { user: user, pool-id: pool-id })
  )
)

;; Calculate profit share for a pool member
(define-read-only (calculate-member-profit-share (pool-id uint) (member principal) (total-profit uint))
  (let
    (
      (pool (unwrap! (map-get? factoring-pools { pool-id: pool-id }) err-not-found))
      (membership (unwrap! (map-get? pool-memberships { pool-id: pool-id, member: member }) err-not-found))
      (total-contribution (get current-amount pool))
      (contribution (get contribution membership))
    )
    (if (> total-contribution u0)
      (ok (/ (* total-profit contribution) total-contribution))
      err-invalid-amount
    )
  )
)

;; Create a new investment pool
(define-public (create-pool (target-amount uint))
  (let
    (
      (pool-id (var-get next-pool-id))
      (new-pool {
        creator: tx-sender,
        target-amount: target-amount,
        current-amount: u0,
        member-count: u0,
        status: "open",
        created-at: stacks-block-height,
        invoice-id: none
      })
    )
    (asserts! (> target-amount u0) err-invalid-amount)
    (map-set factoring-pools { pool-id: pool-id } new-pool)
    (var-set next-pool-id (+ pool-id u1))
    (ok pool-id)
  )
)

;; Join an investment pool with contribution
(define-public (join-pool (pool-id uint) (contribution uint))
  (let
    (
      (pool (unwrap! (map-get? factoring-pools { pool-id: pool-id }) err-not-found))
      (current-membership (map-get? pool-memberships { pool-id: pool-id, member: tx-sender }))
      (member-count (get member-count pool))
      (current-amount (get current-amount pool))
      (new-amount (+ current-amount contribution))
      (new-member-count (if (is-none current-membership) (+ member-count u1) member-count))
    )
    (asserts! (is-eq (get status pool) "open") err-pool-closed)
    (asserts! (> contribution u0) err-invalid-amount)
    (asserts! (< member-count max-pool-members) err-pool-full)
    (asserts! (is-none current-membership) err-already-member)
    (asserts! (<= new-amount (get target-amount pool)) err-invalid-amount)
    
    ;; Add member to pool
    (map-set pool-memberships
      { pool-id: pool-id, member: tx-sender }
      {
        contribution: contribution,
        profit-share: u0,
        withdrawn: false
      }
    )
    
    ;; Add to members list for iteration
    (map-set pool-members-list
      { pool-id: pool-id, index: member-count }
      { member: tx-sender }
    )
    
    ;; Track user pool membership
    (map-set user-pools
      { user: tx-sender, pool-id: pool-id }
      { is-member: true }
    )
    
    ;; Update pool status
    (let
      (
        (updated-pool (merge pool {
          current-amount: new-amount,
          member-count: new-member-count,
          status: (if (>= new-amount (get target-amount pool)) "ready" "open")
        }))
      )
      (map-set factoring-pools { pool-id: pool-id } updated-pool)
    )
    
    (ok true)
  )
)

;; Purchase invoice using pool funds (only when pool is ready)
(define-public (pool-buy-invoice (pool-id uint) (invoice-id uint))
  (let
    (
      (pool (unwrap! (map-get? factoring-pools { pool-id: pool-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get creator pool)) err-unauthorized)
    (asserts! (is-eq (get status pool) "ready") err-pool-closed)
    (asserts! (is-none (get invoice-id pool)) err-pool-active)
    
    ;; Update pool with purchased invoice
    (map-set factoring-pools
      { pool-id: pool-id }
      (merge pool {
        status: "active",
        invoice-id: (some invoice-id)
      })
    )
    
    ;; Here we would integrate with main contract's buy-invoice function
    ;; For now, we just mark the pool as having purchased the invoice
    (ok true)
  )
)

;; Distribute profits from invoice collection to pool members
(define-public (distribute-pool-profits (pool-id uint) (total-profit uint))
  (let
    (
      (pool (unwrap! (map-get? factoring-pools { pool-id: pool-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get creator pool)) err-unauthorized)
    (asserts! (is-eq (get status pool) "active") err-unauthorized)
    (asserts! (> total-profit u0) err-invalid-amount)
    
    ;; Update pool status to completed
    (map-set factoring-pools
      { pool-id: pool-id }
      (merge pool { status: "completed" })
    )
    
    (ok true)
  )
)

;; Set profit share for a specific pool member
(define-public (set-member-profit-share (pool-id uint) (member principal) (profit-amount uint))
  (let
    (
      (pool (unwrap! (map-get? factoring-pools { pool-id: pool-id }) err-not-found))
      (membership (unwrap! (map-get? pool-memberships { pool-id: pool-id, member: member }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get creator pool)) err-unauthorized)
    (asserts! (is-eq (get status pool) "completed") err-unauthorized)
    
    ;; Set the profit share for this member
    (map-set pool-memberships
      { pool-id: pool-id, member: member }
      (merge membership { profit-share: profit-amount })
    )
    
    (ok true)
  )
)

;; Withdraw profit share from completed pool
(define-public (withdraw-pool-profits (pool-id uint))
  (let
    (
      (pool (unwrap! (map-get? factoring-pools { pool-id: pool-id }) err-not-found))
      (membership (unwrap! (map-get? pool-memberships { pool-id: pool-id, member: tx-sender }) err-not-found))
    )
    (asserts! (is-eq (get status pool) "completed") err-unauthorized)
    (asserts! (not (get withdrawn membership)) err-unauthorized)
    (asserts! (> (get profit-share membership) u0) err-insufficient-funds)
    
    ;; Mark as withdrawn
    (map-set pool-memberships
      { pool-id: pool-id, member: tx-sender }
      (merge membership { withdrawn: true })
    )
    
    ;; Return original contribution plus profit share
    (ok (+ (get contribution membership) (get profit-share membership)))
  )
)

;; Close pool and refund contributions if target not reached
(define-public (close-pool (pool-id uint))
  (let
    (
      (pool (unwrap! (map-get? factoring-pools { pool-id: pool-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get creator pool)) err-unauthorized)
    (asserts! (is-eq (get status pool) "open") err-pool-closed)
    
    ;; Update pool status to closed
    (map-set factoring-pools
      { pool-id: pool-id }
      (merge pool { status: "closed" })
    )
    
    (ok true)
  )
)

;; Withdraw contribution from closed pool
(define-public (withdraw-from-closed-pool (pool-id uint))
  (let
    (
      (pool (unwrap! (map-get? factoring-pools { pool-id: pool-id }) err-not-found))
      (membership (unwrap! (map-get? pool-memberships { pool-id: pool-id, member: tx-sender }) err-not-found))
    )
    (asserts! (is-eq (get status pool) "closed") err-unauthorized)
    (asserts! (not (get withdrawn membership)) err-unauthorized)
    
    ;; Mark as withdrawn
    (map-set pool-memberships
      { pool-id: pool-id, member: tx-sender }
      (merge membership { withdrawn: true })
    )
    
    ;; Return original contribution
    (ok (get contribution membership))
  )
)
