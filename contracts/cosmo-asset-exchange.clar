;; Cosmo Asset Exchange - Stellar Asset Distribution and Tokenized Exchange Framework
;; This contract provides a decentralized platform for the registration, allocation,
;; and peer-to-peer exchange of digital assets represented as "cosmic credits" on the blockchain.
;; The system ensures transparency, auditability, and fair distribution among participants.

;; ===============================================
;; DATA STORAGE STRUCTURES
;; ===============================================

;; Track individual asset holdings
(define-map individual-asset-holdings principal uint)

;; Track individual token balances
(define-map individual-token-balances principal uint)

;; Registry of assets available for exchange operations
(define-map asset-exchange-listings {owner: principal} {amount: uint, price: uint})

;; ===============================================
;; ERROR DEFINITIONS AND CONSTANTS
;; ===============================================

;; System administrator account for privileged operations
(define-constant manager-principal tx-sender)

;; Error codes for system operation validation
(define-constant err-system-limit-reached (err u209))
(define-constant err-access-denied (err u200))
(define-constant err-conversion-failure (err u206))
(define-constant err-circular-operation (err u207))
(define-constant err-insufficient-balance (err u201))
(define-constant err-operation-failed (err u202))
(define-constant err-invalid-price (err u203))
(define-constant err-invalid-amount (err u204))
(define-constant err-invalid-fee (err u205))
(define-constant err-limit-exceeded (err u208))

;; ===============================================
;; GLOBAL SYSTEM PARAMETERS
;; ===============================================

;; Conversion and fee parameters
(define-data-var conversion-percentage uint u90)
(define-data-var standard-value uint u100)
(define-data-var individual-asset-limit uint u10000)
(define-data-var global-asset-count uint u0)
(define-data-var operation-fee-percentage uint u5)
(define-data-var global-capacity-ceiling uint u1000000)



;; ===============================================
;; UTILITY FUNCTIONS
;; ===============================================

;; Calculate operation fees for transactions
(define-private (compute-fee (value uint))
  (/ (* value (var-get operation-fee-percentage)) u100))

;; Calculate conversion value for asset-to-token operations
(define-private (compute-conversion-value (amount uint))
  (/ (* amount (var-get standard-value) (var-get conversion-percentage)) u100))

;; Update global asset count with validation
(define-private (update-global-asset-count (adjustment int))
  (let (
    (current-count (var-get global-asset-count))
    (adjusted-count (if (< adjustment 0)
                     (if (>= current-count (to-uint (- 0 adjustment)))
                         (- current-count (to-uint (- 0 adjustment)))
                         u0)
                     (+ current-count (to-uint adjustment))))
  )
    (asserts! (<= adjusted-count (var-get global-capacity-ceiling)) err-system-limit-reached)
    (var-set global-asset-count adjusted-count)
    (ok true)))

;; ===============================================
;; ADMINISTRATIVE FUNCTIONS
;; ===============================================

;; Manager function to distribute assets to participants
(define-public (distribute-assets-to-participant (recipient principal) (amount uint))
  (let (
    (existing-balance (default-to u0 (map-get? individual-asset-holdings recipient)))
    (updated-balance (+ existing-balance amount))
    (current-global-count (var-get global-asset-count))
    (new-global-count (+ current-global-count amount))
  )
    ;; Only manager can perform this operation
    (asserts! (is-eq tx-sender manager-principal) err-access-denied)
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (<= updated-balance (var-get individual-asset-limit)) err-limit-exceeded)
    (asserts! (<= new-global-count (var-get global-capacity-ceiling)) err-system-limit-reached)

    ;; Update global count
    (var-set global-asset-count new-global-count)

    ;; Record the operation for audit purposes
    (print {event: "asset-distribution", recipient: recipient, amount: amount, updated-balance: updated-balance})

    (ok updated-balance)))

;; ===============================================
;; PARTICIPANT ASSET OPERATIONS
;; ===============================================

;; Register new participant assets
(define-public (register-new-assets (amount uint))
  (let (
    (current-holdings (default-to u0 (map-get? individual-asset-holdings tx-sender)))
    (updated-holdings (+ current-holdings amount))
    (global-total (var-get global-asset-count))
    (updated-total (+ global-total amount))
  )
    ;; Validate operation parameters
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (<= updated-holdings (var-get individual-asset-limit)) err-limit-exceeded)
    (asserts! (<= updated-total (var-get global-capacity-ceiling)) err-system-limit-reached)

    ;; Update participant's balance
    (map-set individual-asset-holdings tx-sender updated-holdings)

    ;; Update global asset count
    (var-set global-asset-count updated-total)

    ;; Return success
    (ok true)))

;; Offer assets for exchange on the marketplace
(define-public (offer-assets-for-exchange (amount uint) (price uint))
  (let (
    (current-assets (default-to u0 (map-get? individual-asset-holdings tx-sender)))
    (currently-offered (get amount (default-to {amount: u0, price: u0} 
                         (map-get? asset-exchange-listings {owner: tx-sender}))))
    (total-offered (+ amount currently-offered))
  )
    ;; Validate operation parameters
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (> price u0) err-invalid-price)
    (asserts! (>= current-assets total-offered) err-insufficient-balance)

    ;; Update global asset tracking
    (try! (update-global-asset-count (to-int amount)))

    ;; Update exchange listings
    (map-set asset-exchange-listings {owner: tx-sender} 
             {amount: total-offered, price: price})

    (ok true)))

;; Remove assets from exchange offerings
(define-public (remove-exchange-offering (amount uint))
  (let (
    (offering-data (default-to {amount: u0, price: u0} 
                   (map-get? asset-exchange-listings {owner: tx-sender})))
    (offered-amount (get amount offering-data))
    (offered-price (get price offering-data))
  )
    ;; Validate operation parameters
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (>= offered-amount amount) err-insufficient-balance)

    ;; Update the exchange registry
    (map-set asset-exchange-listings 
             {owner: tx-sender} 
             {amount: (- offered-amount amount), price: offered-price})

    (ok true)))

;; Cancel all exchange offerings
(define-public (withdraw-all-offerings)
  (let (
    (offering-data (default-to {amount: u0, price: u0} 
                   (map-get? asset-exchange-listings {owner: tx-sender})))
    (offered-amount (get amount offering-data))
    (global-total (var-get global-asset-count))
  )
    ;; Verify offerings exist to cancel
    (asserts! (> offered-amount u0) err-insufficient-balance)

    ;; Update global asset tracking
    (var-set global-asset-count (- global-total offered-amount))

    ;; Remove the offering completely
    (map-set asset-exchange-listings {owner: tx-sender} {amount: u0, price: u0})

    ;; Log operation for audit purposes
    (print {event: "offerings-withdrawn", participant: tx-sender, amount: offered-amount})

    (ok true)))

;; Cancel specific offering amount
(define-public (withdraw-specific-offering (amount uint))
  (let (
    (current-offering (default-to {amount: u0, price: u0} 
                      (map-get? asset-exchange-listings {owner: tx-sender})))
    (offered-amount (get amount current-offering))
    (offered-price (get price current-offering))
  )
    ;; Validate operation parameters
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (>= offered-amount amount) err-insufficient-balance)

    ;; Remove or update offering as appropriate
    (if (is-eq offered-amount amount)
        (map-delete asset-exchange-listings {owner: tx-sender})
        (map-set asset-exchange-listings {owner: tx-sender} 
                {amount: (- offered-amount amount), price: offered-price}))

    (ok true)))

;; ===============================================
;; EXCHANGE AND TRANSACTION OPERATIONS
;; ===============================================

;; Purchase assets from another participant
(define-public (purchase-assets (provider principal) (amount uint))
  (let (
    (offering-data (default-to {amount: u0, price: u0} 
                    (map-get? asset-exchange-listings {owner: provider})))
    (asset-cost (* amount (get price offering-data)))
    (fee-amount (compute-fee asset-cost))
    (total-cost (+ asset-cost fee-amount))
    (provider-assets (default-to u0 (map-get? individual-asset-holdings provider)))
    (buyer-tokens (default-to u0 (map-get? individual-token-balances tx-sender)))
    (provider-tokens (default-to u0 (map-get? individual-token-balances provider)))
    (manager-tokens (default-to u0 (map-get? individual-token-balances manager-principal)))
  )
    ;; Transaction validations
    (asserts! (not (is-eq tx-sender provider)) err-circular-operation)
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (>= (get amount offering-data) amount) err-insufficient-balance)
    (asserts! (>= provider-assets amount) err-insufficient-balance)
    (asserts! (>= buyer-tokens total-cost) err-insufficient-balance)

    ;; Update provider's asset balance and offering
    (map-set individual-asset-holdings provider (- provider-assets amount))
    (map-set asset-exchange-listings {owner: provider} 
             {amount: (- (get amount offering-data) amount), 
              price: (get price offering-data)})

    ;; Update token balances
    (map-set individual-token-balances tx-sender (- buyer-tokens total-cost))
    (map-set individual-token-balances provider (+ provider-tokens asset-cost))
    (map-set individual-token-balances manager-principal (+ manager-tokens fee-amount))

    ;; Update buyer's asset balance
    (map-set individual-asset-holdings tx-sender 
             (+ (default-to u0 (map-get? individual-asset-holdings tx-sender)) amount))

    (ok true)))

;; Convert assets to tokens based on standard value
(define-public (convert-assets-to-tokens (amount uint))
  (let (
    (participant-assets (default-to u0 (map-get? individual-asset-holdings tx-sender)))
    (token-value (compute-conversion-value amount))
    (manager-token-balance (default-to u0 (map-get? individual-token-balances manager-principal)))
  )
    ;; Operation validations
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (>= participant-assets amount) err-insufficient-balance)
    (asserts! (>= manager-token-balance token-value) err-conversion-failure)

    ;; Update participant's asset balance
    (map-set individual-asset-holdings tx-sender (- participant-assets amount))

    ;; Update token balances
    (map-set individual-token-balances tx-sender 
             (+ (default-to u0 (map-get? individual-token-balances tx-sender)) token-value))
    (map-set individual-token-balances manager-principal (- manager-token-balance token-value))

    (ok true)))

;; Enhanced asset conversion with additional validation checks
(define-public (secured-asset-conversion (amount uint))
  (let (
        (participant-assets (default-to u0 (map-get? individual-asset-holdings tx-sender)))
        (token-value (compute-conversion-value amount))
  )
    ;; Additional validation checks
    (asserts! (>= participant-assets amount) err-insufficient-balance)
    (asserts! (> token-value u0) err-conversion-failure)

    ;; Process the conversion
    (map-set individual-asset-holdings tx-sender (- participant-assets amount))
    (map-set individual-token-balances tx-sender 
             (+ (default-to u0 (map-get? individual-token-balances tx-sender)) token-value))
    (map-set individual-token-balances manager-principal 
             (- (default-to u0 (map-get? individual-token-balances manager-principal)) token-value))

    (ok true)))

;; Optimized direct asset acquisition function
(define-public (streamlined-asset-purchase (provider principal) (amount uint))
  (let (
        (offering-data (default-to {amount: u0, price: u0} 
                       (map-get? asset-exchange-listings {owner: provider})))
        (asset-cost (* amount (get price offering-data)))
        (buyer-tokens (default-to u0 (map-get? individual-token-balances tx-sender)))
        (provider-assets (default-to u0 (map-get? individual-asset-holdings provider)))
  )
    ;; Efficient validation sequence
    (asserts! (>= buyer-tokens asset-cost) err-insufficient-balance)
    (asserts! (>= provider-assets amount) err-insufficient-balance)

    ;; Direct balance updates
    (map-set individual-token-balances tx-sender (- buyer-tokens asset-cost))
    (map-set individual-asset-holdings tx-sender 
             (+ (default-to u0 (map-get? individual-asset-holdings tx-sender)) amount))
    (map-set individual-asset-holdings provider (- provider-assets amount))
    (map-set individual-token-balances provider 
             (+ (default-to u0 (map-get? individual-token-balances provider)) asset-cost))

    (ok true)))

;; Transfer assets between participants
(define-public (transfer-assets-to-participant (recipient principal) (amount uint))
  (let (
    (sender-assets (default-to u0 (map-get? individual-asset-holdings tx-sender)))
    (recipient-assets (default-to u0 (map-get? individual-asset-holdings recipient)))
    (transfer-fee (compute-fee (var-get standard-value)))
    (sender-token-balance (default-to u0 (map-get? individual-token-balances tx-sender)))
  )
    ;; Transaction validations
    (asserts! (not (is-eq tx-sender recipient)) err-circular-operation)
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (>= sender-assets amount) err-insufficient-balance)
    (asserts! (>= sender-token-balance transfer-fee) err-insufficient-balance)
    (asserts! (<= (+ recipient-assets amount) (var-get individual-asset-limit)) 
              err-limit-exceeded)

    ;; Update asset balances
    (map-set individual-asset-holdings tx-sender (- sender-assets amount))
    (map-set individual-asset-holdings recipient (+ recipient-assets amount))

    ;; Process fee payment
    (map-set individual-token-balances tx-sender (- sender-token-balance transfer-fee))
    (map-set individual-token-balances manager-principal 
             (+ (default-to u0 (map-get? individual-token-balances manager-principal)) transfer-fee))

    (ok true)
  )
)

;; ===============================================
;; TOKEN MANAGEMENT OPERATIONS
;; ===============================================

;; Extract tokens from the system
(define-public (extract-tokens (amount uint))
  (let (
    (current-balance (default-to u0 (map-get? individual-token-balances tx-sender)))
    (new-balance (if (>= current-balance amount)
                    (- current-balance amount)
                    u0))
  )
    ;; Validations
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (>= current-balance amount) err-insufficient-balance)

    ;; Update participant's token balance
    (map-set individual-token-balances tx-sender new-balance)

    ;; Process token transfer through contract
    (try! (as-contract (stx-transfer? amount (as-contract tx-sender) tx-sender)))

    (ok new-balance)))

