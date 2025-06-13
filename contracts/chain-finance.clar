(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-already-exists (err u103))
(define-constant err-invalid-amount (err u104))
(define-constant err-insufficient-funds (err u105))
(define-constant err-expired (err u106))
(define-constant err-not-for-sale (err u107))
(define-constant err-already-claimed (err u108))

(define-data-var platform-fee uint u5)
(define-data-var next-invoice-id uint u1)

(define-map invoices
  { invoice-id: uint }
  {
    retailer: principal,
    supplier: principal,
    amount: uint,
    due-date: uint,
    status: (string-ascii 20),
    tokenized: bool,
    for-sale: bool,
    sale-discount: uint,
    buyer: (optional principal)
  }
)

(define-map retailer-balances
  { retailer: principal }
  { balance: uint }
)

(define-map investor-balances
  { investor: principal }
  { balance: uint }
)

(define-map claimed-invoices
  { invoice-id: uint }
  { claimed: bool }
)

(define-read-only (get-invoice (invoice-id uint))
  (match (map-get? invoices { invoice-id: invoice-id })
    invoice (ok invoice)
    err-not-found
  )
)

(define-read-only (get-retailer-balance (retailer principal))
  (default-to
    { balance: u0 }
    (map-get? retailer-balances { retailer: retailer })
  )
)

(define-read-only (get-investor-balance (investor principal))
  (default-to
    { balance: u0 }
    (map-get? investor-balances { investor: investor })
  )
)

(define-read-only (get-platform-fee)
  (var-get platform-fee)
)

(define-public (set-platform-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-fee u100) err-invalid-amount)
    (ok (var-set platform-fee new-fee))
  )
)

(define-public (register-invoice (supplier principal) (amount uint) (due-date uint))
  (let
    (
      (invoice-id (var-get next-invoice-id))
      (new-invoice {
        retailer: tx-sender,
        supplier: supplier,
        amount: amount,
        due-date: due-date,
        status: "registered",
        tokenized: false,
        for-sale: false,
        sale-discount: u0,
        buyer: none
      })
    )
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (> due-date stacks-block-height) err-invalid-amount)
    (map-set invoices { invoice-id: invoice-id } new-invoice)
    (var-set next-invoice-id (+ invoice-id u1))
    (ok invoice-id)
  )
)

(define-public (tokenize-invoice (invoice-id uint))
  (let
    (
      (invoice (unwrap! (map-get? invoices { invoice-id: invoice-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get retailer invoice)) err-unauthorized)
    (asserts! (not (get tokenized invoice)) err-already-exists)
    (map-set invoices
      { invoice-id: invoice-id }
      (merge invoice { tokenized: true, status: "tokenized" })
    )
    (ok true)
  )
)

(define-public (offer-invoice-for-sale (invoice-id uint) (discount uint))
  (let
    (
      (invoice (unwrap! (map-get? invoices { invoice-id: invoice-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get retailer invoice)) err-unauthorized)
    (asserts! (get tokenized invoice) err-unauthorized)
    (asserts! (< discount u100) err-invalid-amount)
    (map-set invoices
      { invoice-id: invoice-id }
      (merge invoice { 
        for-sale: true, 
        sale-discount: discount,
        status: "for-sale"
      })
    )
    (ok true)
  )
)

(define-public (cancel-invoice-sale (invoice-id uint))
  (let
    (
      (invoice (unwrap! (map-get? invoices { invoice-id: invoice-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get retailer invoice)) err-unauthorized)
    (asserts! (get for-sale invoice) err-not-for-sale)
    (asserts! (is-none (get buyer invoice)) err-unauthorized)
    (map-set invoices
      { invoice-id: invoice-id }
      (merge invoice { 
        for-sale: false, 
        sale-discount: u0,
        status: "tokenized"
      })
    )
    (ok true)
  )
)

(define-public (buy-invoice (invoice-id uint))
  (let
    (
      (invoice (unwrap! (map-get? invoices { invoice-id: invoice-id }) err-not-found))
      (discount (get sale-discount invoice))
      (amount (get amount invoice))
      (discounted-amount (/ (* amount (- u100 discount)) u100))
      (fee (/ (* discounted-amount (var-get platform-fee)) u100))
      (retailer-amount (- discounted-amount fee))
    )
    (asserts! (get for-sale invoice) err-not-for-sale)
    (asserts! (is-none (get buyer invoice)) err-already-exists)
    (asserts! (> stacks-block-height (get due-date invoice)) err-expired)
    
    ;; Update invoice
    (map-set invoices
      { invoice-id: invoice-id }
      (merge invoice { 
        buyer: (some tx-sender),
        for-sale: false,
        status: "purchased"
      })
    )
    
    ;; Update retailer balance
    (map-set retailer-balances
      { retailer: (get retailer invoice) }
      { balance: (+ (get balance (get-retailer-balance (get retailer invoice))) retailer-amount) }
    )
    
    ;; Update investor balance (claim rights)
    (map-set investor-balances
      { investor: tx-sender }
      { balance: (+ (get balance (get-investor-balance tx-sender)) amount) }
    )
    
    (ok true)
  )
)

(define-public (claim-invoice-payment (invoice-id uint))
  (let
    (
      (invoice (unwrap! (map-get? invoices { invoice-id: invoice-id }) err-not-found))
      (buyer (unwrap! (get buyer invoice) err-unauthorized))
    )
    (asserts! (is-eq tx-sender buyer) err-unauthorized)
    (asserts! (>= stacks-block-height (get due-date invoice)) err-unauthorized)
    (asserts! (not (default-to false (get claimed (map-get? claimed-invoices { invoice-id: invoice-id })))) err-already-claimed)
    
    ;; Mark as claimed
    (map-set claimed-invoices { invoice-id: invoice-id } { claimed: true })
    
    ;; Update invoice status
    (map-set invoices
      { invoice-id: invoice-id }
      (merge invoice { status: "claimed" })
    )
    
    ;; Reduce investor balance
    (map-set investor-balances
      { investor: tx-sender }
      { balance: (- (get balance (get-investor-balance tx-sender)) (get amount invoice)) }
    )
    
    (ok true)
  )
)

(define-public (withdraw-retailer-funds)
  (let
    (
      (balance (get balance (get-retailer-balance tx-sender)))
    )
    (asserts! (> balance u0) err-insufficient-funds)
    (map-set retailer-balances
      { retailer: tx-sender }
      { balance: u0 }
    )
    (ok balance)
  )
)

(define-public (withdraw-investor-funds)
  (let
    (
      (balance (get balance (get-investor-balance tx-sender)))
    )
    (asserts! (> balance u0) err-insufficient-funds)
    (map-set investor-balances
      { investor: tx-sender }
      { balance: u0 }
    )
    (ok balance)
  )
)


(define-map retailer-ratings
  { retailer: principal }
  { 
    total-score: uint,
    rating-count: uint
  }
)

(define-read-only (get-retailer-rating (retailer principal))
  (default-to
    { total-score: u0, rating-count: u0 }
    (map-get? retailer-ratings { retailer: retailer })
  )
)

(define-public (rate-retailer (retailer principal) (score uint))
  (let
    (
      (current-rating (get-retailer-rating retailer))
      (total-score (get total-score current-rating))
      (rating-count (get rating-count current-rating))
    )
    (asserts! (<= score u5) err-invalid-amount)
    (asserts! (> score u0) err-invalid-amount)
    (map-set retailer-ratings
      { retailer: retailer }
      {
        total-score: (+ total-score score),
        rating-count: (+ rating-count u1)
      }
    )
    (ok true)
  )
)



(define-constant err-no-dispute (err u109))

(define-map invoice-disputes
  { invoice-id: uint }
  {
    supplier: principal,
    reason: (string-ascii 50),
    status: (string-ascii 20),
    resolution: (optional (string-ascii 50))
  }
)

(define-public (raise-dispute (invoice-id uint) (reason (string-ascii 50)))
  (let
    (
      (invoice (unwrap! (map-get? invoices { invoice-id: invoice-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get supplier invoice)) err-unauthorized)
    (map-set invoice-disputes
      { invoice-id: invoice-id }
      {
        supplier: tx-sender,
        reason: reason,
        status: "open",
        resolution: none
      }
    )
    (ok true)
  )
)

(define-public (resolve-dispute (invoice-id uint) (resolution (string-ascii 50)))
  (let
    (
      (dispute (unwrap! (map-get? invoice-disputes { invoice-id: invoice-id }) err-no-dispute))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set invoice-disputes
      { invoice-id: invoice-id }
      (merge dispute {
        status: "resolved",
        resolution: (some resolution)
      })
    )
    (ok true)
  )
)


(define-map retailer-payment-accounts
  { retailer: principal }
  { balance: uint }
)

(define-map auto-payment-settings
  { retailer: principal }
  { enabled: bool }
)

(define-map scheduled-payments
  { invoice-id: uint }
  { 
    amount: uint,
    due-block: uint,
    processed: bool
  }
)

(define-read-only (get-payment-account-balance (retailer principal))
  (default-to
    { balance: u0 }
    (map-get? retailer-payment-accounts { retailer: retailer })
  )
)

(define-read-only (get-auto-payment-status (retailer principal))
  (default-to
    { enabled: false }
    (map-get? auto-payment-settings { retailer: retailer })
  )
)

(define-read-only (get-scheduled-payment (invoice-id uint))
  (map-get? scheduled-payments { invoice-id: invoice-id })
)

(define-public (deposit-to-payment-account (amount uint))
  (begin
    (asserts! (> amount u0) err-invalid-amount)
    (map-set retailer-payment-accounts
      { retailer: tx-sender }
      { balance: (+ (get balance (get-payment-account-balance tx-sender)) amount) }
    )
    (ok amount)
  )
)

(define-public (withdraw-from-payment-account (amount uint))
  (let
    (
      (current-balance (get balance (get-payment-account-balance tx-sender)))
    )
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (>= current-balance amount) err-insufficient-funds)
    (map-set retailer-payment-accounts
      { retailer: tx-sender }
      { balance: (- current-balance amount) }
    )
    (ok amount)
  )
)

(define-public (enable-auto-payments)
  (begin
    (map-set auto-payment-settings
      { retailer: tx-sender }
      { enabled: true }
    )
    (ok true)
  )
)

(define-public (disable-auto-payments)
  (begin
    (map-set auto-payment-settings
      { retailer: tx-sender }
      { enabled: false }
    )
    (ok true)
  )
)

(define-public (schedule-auto-payment (invoice-id uint))
  (let
    (
      (invoice (unwrap! (map-get? invoices { invoice-id: invoice-id }) err-not-found))
      (retailer (get retailer invoice))
      (auto-payment-enabled (get enabled (get-auto-payment-status retailer)))
    )
    (asserts! (is-eq tx-sender retailer) err-unauthorized)
    (asserts! auto-payment-enabled err-unauthorized)
    (asserts! (is-eq (get status invoice) "registered") err-unauthorized)
    (map-set scheduled-payments
      { invoice-id: invoice-id }
      {
        amount: (get amount invoice),
        due-block: (get due-date invoice),
        processed: false
      }
    )
    (map-set invoices
      { invoice-id: invoice-id }
      (merge invoice { status: "scheduled" })
    )
    (ok true)
  )
)

(define-public (process-auto-payment (invoice-id uint))
  (let
    (
      (invoice (unwrap! (map-get? invoices { invoice-id: invoice-id }) err-not-found))
      (scheduled-payment (unwrap! (map-get? scheduled-payments { invoice-id: invoice-id }) err-not-found))
      (retailer (get retailer invoice))
      (supplier (get supplier invoice))
      (amount (get amount scheduled-payment))
      (retailer-balance (get balance (get-payment-account-balance retailer)))
    )
    (asserts! (>= stacks-block-height (get due-block scheduled-payment)) err-unauthorized)
    (asserts! (not (get processed scheduled-payment)) err-already-claimed)
    (asserts! (>= retailer-balance amount) err-insufficient-funds)
    
    (map-set retailer-payment-accounts
      { retailer: retailer }
      { balance: (- retailer-balance amount) }
    )
    
    (map-set scheduled-payments
      { invoice-id: invoice-id }
      (merge scheduled-payment { processed: true })
    )
    
    (map-set invoices
      { invoice-id: invoice-id }
      (merge invoice { status: "paid" })
    )
    
    (ok true)
  )
)

(define-public (cancel-scheduled-payment (invoice-id uint))
  (let
    (
      (invoice (unwrap! (map-get? invoices { invoice-id: invoice-id }) err-not-found))
      (scheduled-payment (unwrap! (map-get? scheduled-payments { invoice-id: invoice-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get retailer invoice)) err-unauthorized)
    (asserts! (not (get processed scheduled-payment)) err-already-claimed)
    (asserts! (< stacks-block-height (get due-block scheduled-payment)) err-expired)
    
    (map-delete scheduled-payments { invoice-id: invoice-id })
    
    (map-set invoices
      { invoice-id: invoice-id }
      (merge invoice { status: "registered" })
    )
    
    (ok true)
  )
)