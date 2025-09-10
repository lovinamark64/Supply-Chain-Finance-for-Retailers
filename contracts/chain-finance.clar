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
(define-constant err-no-insurance (err u110))
(define-constant err-insurance-expired (err u111))
(define-constant err-already-insured (err u112))
(define-constant err-interest-not-enabled (err u113))
(define-constant err-credit-limit-exceeded (err u114))
(define-constant err-insufficient-credit-score (err u115))

(define-data-var platform-fee uint u5)
(define-data-var next-invoice-id uint u1)
(define-data-var insurance-pool uint u0)
(define-data-var default-interest-rate uint u5)
(define-data-var minimum-credit-score uint u300)

(define-map invoices
  { invoice-id: uint }
  {
    retailer: principal,
    supplier: principal,
    amount: uint,
    due-date: uint,
    status: (string-ascii 25),
    tokenized: bool,
    for-sale: bool,
    sale-discount: uint,
    buyer: (optional principal),
    interest-enabled: bool,
    interest-rate: uint,
    accrued-interest: uint
  }
)

;; Credit scoring and payment history tracking
(define-map retailer-credit-profiles
  { retailer: principal }
  {
    total-invoices: uint,
    paid-on-time: uint,
    late-payments: uint,
    defaults: uint,
    total-volume: uint,
    credit-score: uint,
    last-updated: uint
  }
)

;; Credit tier privileges based on scores
(define-map credit-tiers
  { tier: uint }
  {
    min-score: uint,
    max-discount: uint,
    max-invoice-amount: uint,
    insurance-discount: uint
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

(define-map invoice-insurance
  { invoice-id: uint }
  {
    insured: bool,
    premium-paid: uint,
    coverage-amount: uint,
    expires-at: uint,
    policyowner: principal
  }
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

(define-read-only (get-insurance-details (invoice-id uint))
  (map-get? invoice-insurance { invoice-id: invoice-id })
)

(define-read-only (get-insurance-pool-balance)
  (var-get insurance-pool)
)

(define-read-only (get-default-interest-rate)
  (var-get default-interest-rate)
)

(define-read-only (calculate-insurance-premium (amount uint) (risk-factor uint))
  (/ (* amount risk-factor) u1000)
)

(define-read-only (calculate-interest (principal-amount uint) (rate uint) (blocks-overdue uint))
  (/ (* (* principal-amount rate) blocks-overdue) u36500)
)

;; Credit scoring system read-only functions
(define-read-only (get-credit-profile (retailer principal))
  (default-to
    { 
      total-invoices: u0,
      paid-on-time: u0,
      late-payments: u0,
      defaults: u0,
      total-volume: u0,
      credit-score: u500,
      last-updated: u0
    }
    (map-get? retailer-credit-profiles { retailer: retailer })
  )
)

(define-read-only (get-credit-tier (tier uint))
  (map-get? credit-tiers { tier: tier })
)

(define-read-only (calculate-credit-score (profile (tuple (total-invoices uint) (paid-on-time uint) (late-payments uint) (defaults uint) (total-volume uint) (credit-score uint) (last-updated uint))))
  (let
    (
      (total-invoices (get total-invoices profile))
      (paid-on-time (get paid-on-time profile))
      (late-payments (get late-payments profile))
      (defaults (get defaults profile))
      (total-volume (get total-volume profile))
    )
    (if (is-eq total-invoices u0)
      u500  ;; Default score for new retailers
      (let
        (
          (on-time-rate (/ (* paid-on-time u100) total-invoices))
          (default-penalty (if (> defaults u0) (* defaults u50) u0))
          (late-penalty (* late-payments u10))
          (volume-bonus (if (> total-volume u10000) u50 u0))
          (base-score (+ u300 (* on-time-rate u4)))
        )
        (if (>= (- (+ base-score volume-bonus) (+ default-penalty late-penalty)) u850)
          u850
          (if (<= (- (+ base-score volume-bonus) (+ default-penalty late-penalty)) u300)
            u300
            (- (+ base-score volume-bonus) (+ default-penalty late-penalty))
          )
        )
      )
    )
  )
)

(define-read-only (get-retailer-credit-tier (retailer principal))
  (let
    (
      (credit-score (get credit-score (get-credit-profile retailer)))
    )
    (if (>= credit-score u750) u4
      (if (>= credit-score u650) u3
        (if (>= credit-score u550) u2
          (if (>= credit-score u450) u1 u0)
        )
      )
    )
  )
)

(define-read-only (get-max-discount-for-retailer (retailer principal))
  (let
    (
      (tier (get-retailer-credit-tier retailer))
      (tier-info (get-credit-tier tier))
    )
    (match tier-info
      info (get max-discount info)
      u10  ;; Default 10% for unrated retailers
    )
  )
)

;; Initialize credit tier system - only owner can set these
(define-public (initialize-credit-tiers)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    ;; Tier 0: Poor credit (300-449)
    (map-set credit-tiers { tier: u0 } { min-score: u300, max-discount: u5, max-invoice-amount: u1000, insurance-discount: u0 })
    ;; Tier 1: Fair credit (450-549)
    (map-set credit-tiers { tier: u1 } { min-score: u450, max-discount: u15, max-invoice-amount: u5000, insurance-discount: u5 })
    ;; Tier 2: Good credit (550-649)
    (map-set credit-tiers { tier: u2 } { min-score: u550, max-discount: u25, max-invoice-amount: u15000, insurance-discount: u10 })
    ;; Tier 3: Very good credit (650-749)
    (map-set credit-tiers { tier: u3 } { min-score: u650, max-discount: u40, max-invoice-amount: u50000, insurance-discount: u15 })
    ;; Tier 4: Excellent credit (750+)
    (map-set credit-tiers { tier: u4 } { min-score: u750, max-discount: u60, max-invoice-amount: u100000, insurance-discount: u20 })
    (ok true)
  )
)

;; Update credit profile when payment events occur
(define-public (record-payment-event (retailer principal) (invoice-amount uint) (payment-type (string-ascii 10)))
  (let
    (
      (current-profile (get-credit-profile retailer))
      (total-invoices (get total-invoices current-profile))
      (paid-on-time (get paid-on-time current-profile))
      (late-payments (get late-payments current-profile))
      (defaults (get defaults current-profile))
      (total-volume (get total-volume current-profile))
      (new-total-invoices (+ total-invoices u1))
      (new-total-volume (+ total-volume invoice-amount))
      (new-paid-on-time (if (is-eq payment-type "on-time") (+ paid-on-time u1) paid-on-time))
      (new-late-payments (if (is-eq payment-type "late") (+ late-payments u1) late-payments))
      (new-defaults (if (is-eq payment-type "default") (+ defaults u1) defaults))
      (updated-profile {
        total-invoices: new-total-invoices,
        paid-on-time: new-paid-on-time,
        late-payments: new-late-payments,
        defaults: new-defaults,
        total-volume: new-total-volume,
        credit-score: u0,
        last-updated: stacks-block-height
      })
      (new-score (calculate-credit-score updated-profile))
    )
    (map-set retailer-credit-profiles
      { retailer: retailer }
      (merge updated-profile { credit-score: new-score })
    )
    (ok new-score)
  )
)

(define-public (set-platform-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-fee u100) err-invalid-amount)
    (ok (var-set platform-fee new-fee))
  )
)

(define-public (set-default-interest-rate (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-rate u50) err-invalid-amount)
    (ok (var-set default-interest-rate new-rate))
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
        buyer: none,
        interest-enabled: false,
        interest-rate: u0,
        accrued-interest: u0
      })
    )
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (> due-date stacks-block-height) err-invalid-amount)
    ;; Enforce credit-based invoice amount limits
    (unwrap-panic (validate-invoice-credit-limit tx-sender amount))
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
      (retailer (get retailer invoice))
      (max-allowed-discount (get-max-discount-for-retailer retailer))
    )
    (asserts! (is-eq tx-sender retailer) err-unauthorized)
    (asserts! (get tokenized invoice) err-unauthorized)
    (asserts! (< discount u100) err-invalid-amount)
    (asserts! (<= discount max-allowed-discount) err-credit-limit-exceeded)
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
    
    ;; Record on-time payment for credit score
    (unwrap-panic (record-payment-event retailer amount "on-time"))
    
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

(define-public (purchase-invoice-insurance (invoice-id uint) (coverage-percent uint))
  (let
    (
      (invoice (unwrap! (map-get? invoices { invoice-id: invoice-id }) err-not-found))
      (invoice-amount (get amount invoice))
      (coverage-amount (/ (* invoice-amount coverage-percent) u100))
      (retailer-rating (get-retailer-rating (get retailer invoice)))
      (risk-factor (if (> (get rating-count retailer-rating) u0)
                      (+ u10 (- u50 (* (/ (get total-score retailer-rating) (get rating-count retailer-rating)) u10)))
                      u30))
      (premium (calculate-insurance-premium coverage-amount risk-factor))
      (policy-duration (+ stacks-block-height u144))
    )
    (asserts! (is-some (get buyer invoice)) err-unauthorized)
    (asserts! (is-eq tx-sender (unwrap-panic (get buyer invoice))) err-unauthorized)
    (asserts! (is-none (map-get? invoice-insurance { invoice-id: invoice-id })) err-already-insured)
    (asserts! (<= coverage-percent u100) err-invalid-amount)
    (asserts! (> coverage-percent u0) err-invalid-amount)
    
    (map-set invoice-insurance
      { invoice-id: invoice-id }
      {
        insured: true,
        premium-paid: premium,
        coverage-amount: coverage-amount,
        expires-at: policy-duration,
        policyowner: tx-sender
      }
    )
    
    (var-set insurance-pool (+ (var-get insurance-pool) premium))
    (ok premium)
  )
)

(define-public (claim-insurance-payout (invoice-id uint))
  (let
    (
      (invoice (unwrap! (map-get? invoices { invoice-id: invoice-id }) err-not-found))
      (insurance (unwrap! (map-get? invoice-insurance { invoice-id: invoice-id }) err-no-insurance))
      (coverage-amount (get coverage-amount insurance))
      (pool-balance (var-get insurance-pool))
    )
    (asserts! (is-eq tx-sender (get policyowner insurance)) err-unauthorized)
    (asserts! (>= stacks-block-height (get due-date invoice)) err-unauthorized)
    (asserts! (< stacks-block-height (get expires-at insurance)) err-insurance-expired)
    (asserts! (not (default-to false (get claimed (map-get? claimed-invoices { invoice-id: invoice-id })))) err-already-claimed)
    (asserts! (>= pool-balance coverage-amount) err-insufficient-funds)
    
    (map-set claimed-invoices { invoice-id: invoice-id } { claimed: true })
    (var-set insurance-pool (- pool-balance coverage-amount))
    
    (map-set investor-balances
      { investor: tx-sender }
      { balance: (+ (get balance (get-investor-balance tx-sender)) coverage-amount) }
    )
    
    (ok coverage-amount)
  )
)

(define-public (enable-interest (invoice-id uint) (custom-rate uint))
  (let
    (
      (invoice (unwrap! (map-get? invoices { invoice-id: invoice-id }) err-not-found))
      (interest-rate (if (> custom-rate u0) custom-rate (var-get default-interest-rate)))
    )
    (asserts! (is-eq tx-sender (get retailer invoice)) err-unauthorized)
    (asserts! (not (get interest-enabled invoice)) err-already-exists)
    (asserts! (<= interest-rate u50) err-invalid-amount)
    (map-set invoices
      { invoice-id: invoice-id }
      (merge invoice { 
        interest-enabled: true,
        interest-rate: interest-rate 
      })
    )
    (ok true)
  )
)

(define-public (calculate-and-update-interest (invoice-id uint))
  (let
    (
      (invoice (unwrap! (map-get? invoices { invoice-id: invoice-id }) err-not-found))
      (current-block stacks-block-height)
      (due-date (get due-date invoice))
      (blocks-overdue (if (> current-block due-date) (- current-block due-date) u0))
      (principal-amount (get amount invoice))
      (interest-rate (get interest-rate invoice))
      (new-interest (calculate-interest principal-amount interest-rate blocks-overdue))
    )
    (asserts! (get interest-enabled invoice) err-interest-not-enabled)
    (asserts! (> current-block due-date) err-unauthorized)
    (map-set invoices
      { invoice-id: invoice-id }
      (merge invoice { accrued-interest: new-interest })
    )
    (ok new-interest)
  )
)

(define-public (claim-invoice-with-interest (invoice-id uint))
  (let
    (
      (invoice (unwrap! (map-get? invoices { invoice-id: invoice-id }) err-not-found))
      (buyer (unwrap! (get buyer invoice) err-unauthorized))
      (principal-amount (get amount invoice))
      (accrued-interest (get accrued-interest invoice))
      (total-amount (+ principal-amount accrued-interest))
    )
    (asserts! (is-eq tx-sender buyer) err-unauthorized)
    (asserts! (>= stacks-block-height (get due-date invoice)) err-unauthorized)
    (asserts! (not (default-to false (get claimed (map-get? claimed-invoices { invoice-id: invoice-id })))) err-already-claimed)
    
    (map-set claimed-invoices { invoice-id: invoice-id } { claimed: true })
    
    (map-set invoices
      { invoice-id: invoice-id }
      (merge invoice { status: "claimed-with-interest" })
    )
    
    (map-set investor-balances
      { investor: tx-sender }
      { balance: (+ (get balance (get-investor-balance tx-sender)) total-amount) }
    )
    
    (ok total-amount)
  )
)

;; Enhanced insurance pricing based on credit score
(define-public (purchase-credit-based-insurance (invoice-id uint) (coverage-percent uint))
  (let
    (
      (invoice (unwrap! (map-get? invoices { invoice-id: invoice-id }) err-not-found))
      (retailer (get retailer invoice))
      (invoice-amount (get amount invoice))
      (coverage-amount (/ (* invoice-amount coverage-percent) u100))
      (credit-profile (get-credit-profile retailer))
      (credit-score (get credit-score credit-profile))
      (tier (get-retailer-credit-tier retailer))
      (tier-info (get-credit-tier tier))
      (insurance-discount (match tier-info info (get insurance-discount info) u0))
      (base-risk-factor (if (>= credit-score u700) u15
                         (if (>= credit-score u600) u25
                           (if (>= credit-score u500) u35 u50))))
      (adjusted-risk-factor (if (> insurance-discount u0) 
                              (- base-risk-factor (/ (* base-risk-factor insurance-discount) u100))
                              base-risk-factor))
      (premium (calculate-insurance-premium coverage-amount adjusted-risk-factor))
      (policy-duration (+ stacks-block-height u144))
    )
    (asserts! (is-some (get buyer invoice)) err-unauthorized)
    (asserts! (is-eq tx-sender (unwrap-panic (get buyer invoice))) err-unauthorized)
    (asserts! (is-none (map-get? invoice-insurance { invoice-id: invoice-id })) err-already-insured)
    (asserts! (<= coverage-percent u100) err-invalid-amount)
    (asserts! (> coverage-percent u0) err-invalid-amount)
    
    (map-set invoice-insurance
      { invoice-id: invoice-id }
      {
        insured: true,
        premium-paid: premium,
        coverage-amount: coverage-amount,
        expires-at: policy-duration,
        policyowner: tx-sender
      }
    )
    
    (var-set insurance-pool (+ (var-get insurance-pool) premium))
    (ok premium)
  )
)

;; Manually record late payment or default (for admin/oracle use)
(define-public (record-late-payment-or-default (retailer principal) (invoice-amount uint) (is-default bool))
  (let
    (
      (payment-type (if is-default "default" "late"))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (unwrap-panic (record-payment-event retailer invoice-amount payment-type))
    (ok true)
  )
)

;; Credit score based invoice amount validation
(define-public (validate-invoice-credit-limit (retailer principal) (amount uint))
  (let
    (
      (tier (get-retailer-credit-tier retailer))
      (tier-info (get-credit-tier tier))
      (max-amount (match tier-info info (get max-invoice-amount info) u1000))
    )
    (asserts! (<= amount max-amount) err-credit-limit-exceeded)
    (ok true)
  )
)



