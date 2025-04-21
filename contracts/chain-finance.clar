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