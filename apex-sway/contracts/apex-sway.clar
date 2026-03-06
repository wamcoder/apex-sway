;; EthicalTrace - Supply Chain Transparency Platform

;; This contract implements:
;;   - Provenance DNA (unique cryptographic fingerprint per product)
;;   - Proof of Impact validator rewards
;;   - Supplier reputation token system
;;   - Multi-tier compliance verification
;;   - Consumer transparency portal support

;; ============================================================
;; CONSTANTS
;; ============================================================

(define-constant CONTRACT-OWNER tx-sender)

(define-constant ERR-NOT-AUTHORIZED        (err u100))
(define-constant ERR-PRODUCT-NOT-FOUND     (err u101))
(define-constant ERR-ALREADY-REGISTERED    (err u102))
(define-constant ERR-INVALID-SCORE         (err u103))
(define-constant ERR-SUPPLIER-NOT-FOUND    (err u104))
(define-constant ERR-INVALID-STAGE         (err u105))
(define-constant ERR-VALIDATOR-NOT-FOUND   (err u106))
(define-constant ERR-ALREADY-VERIFIED      (err u107))

;; Compliance score range: 0-100
(define-constant MAX-SCORE u100)

;; Reputation token reward amounts
(define-constant REWARD-VERIFICATION    u10)
(define-constant REWARD-HIGH-COMPLIANCE u25)
(define-constant REWARD-VALIDATOR       u5)

;; High-compliance threshold
(define-constant HIGH-COMPLIANCE-THRESHOLD u80)

;; ============================================================
;; DATA MAPS AND VARS
;; ============================================================

;; Total products registered
(define-data-var total-products uint u0)

;; Total validators registered
(define-data-var total-validators uint u0)

;; Supplier registry
;; Maps principal -> supplier info
(define-map suppliers
  principal
  {
    name:               (string-ascii 64),
    registered-at:      uint,
    reputation-tokens:  uint,
    compliance-score:   uint,    ;; 0-100
    active:             bool
  }
)

;; Product registry
;; Maps product-id (uint) -> product info
(define-map products
  uint
  {
    provenance-dna:     (buff 32),   ;; unique cryptographic fingerprint
    owner:              principal,
    supplier:           principal,
    name:               (string-ascii 64),
    created-at:         uint,
    current-stage:      uint,        ;; 0=raw, 1=manufacturing, 2=logistics, 3=retail, 4=sold
    environmental-score: uint,       ;; 0-100
    labor-score:         uint,       ;; 0-100
    quality-score:       uint,       ;; 0-100
    verified:            bool,
    active:              bool
  }
)

;; Supply chain stage events
;; Maps {product-id, stage} -> stage data
(define-map stage-events
  { product-id: uint, stage: uint }
  {
    recorded-by:     principal,
    recorded-at:     uint,
    location-hash:   (buff 32),   ;; hashed GPS / location data
    notes:           (string-ascii 128),
    sensor-data-hash:(buff 32),   ;; hash of IoT / satellite data
    verified:        bool
  }
)

;; Validator registry
;; Maps principal -> validator info
(define-map validators
  principal
  {
    registered-at:       uint,
    verifications-done:  uint,
    impact-score:        uint,   ;; accumulated Proof of Impact score
    active:              bool
  }
)

;; Verification records
;; Maps {product-id, validator} -> verification result
(define-map verifications
  { product-id: uint, validator: principal }
  {
    verified-at:         uint,
    compliance-score:    uint,
    environmental-score: uint,
    labor-score:         uint,
    quality-score:       uint,
    notes:               (string-ascii 128)
  }
)

;; Consumer QR scan log
;; Maps {product-id, consumer} -> scan count
(define-map consumer-scans
  { product-id: uint, consumer: principal }
  { scan-count: uint, last-scanned: uint }
)

;; ============================================================
;; PRIVATE HELPERS
;; ============================================================

(define-private (is-contract-owner)
  (is-eq tx-sender CONTRACT-OWNER)
)

(define-private (is-registered-supplier (addr principal))
  (match (map-get? suppliers addr)
    supplier (get active supplier)
    false
  )
)

(define-private (is-registered-validator (addr principal))
  (match (map-get? validators addr)
    v (get active v)
    false
  )
)

(define-private (valid-score (score uint))
  (<= score MAX-SCORE)
)

;; Compute aggregate compliance score as average of three sub-scores
(define-private (compute-compliance
    (env-score uint)
    (labor-score uint)
    (quality-score uint))
  (/ (+ env-score (+ labor-score quality-score)) u3)
)

;; Award reputation tokens to a supplier
(define-private (award-tokens (supplier-addr principal) (amount uint))
  (match (map-get? suppliers supplier-addr)
    s (begin
        (map-set suppliers supplier-addr
          (merge s { reputation-tokens: (+ (get reputation-tokens s) amount) })
        )
        true
      )
    false
  )
)

;; ============================================================
;; SUPPLIER FUNCTIONS
;; ============================================================

;; Register a new supplier
(define-public (register-supplier (name (string-ascii 64)))
  (begin
    (asserts! (not (is-registered-supplier tx-sender)) ERR-ALREADY-REGISTERED)
    (map-set suppliers tx-sender
      {
        name:              name,
        registered-at:     block-height,
        reputation-tokens: u0,
        compliance-score:  u0,
        active:            true
      }
    )
    (ok true)
  )
)

;; Deactivate own supplier account
(define-public (deactivate-supplier)
  (match (map-get? suppliers tx-sender)
    s (begin
        (map-set suppliers tx-sender (merge s { active: false }))
        (ok true)
      )
    ERR-SUPPLIER-NOT-FOUND
  )
)

;; Read-only: get supplier info
(define-read-only (get-supplier (addr principal))
  (map-get? suppliers addr)
)

;; Read-only: get reputation tokens for a supplier
(define-read-only (get-reputation-tokens (addr principal))
  (match (map-get? suppliers addr)
    s (ok (get reputation-tokens s))
    ERR-SUPPLIER-NOT-FOUND
  )
)

;; ============================================================
;; PRODUCT / PROVENANCE DNA FUNCTIONS
;; ============================================================

;; Register a new product with its Provenance DNA fingerprint
;; provenance-dna: 32-byte hash representing the product's unique fingerprint
(define-public (register-product
    (name             (string-ascii 64))
    (provenance-dna   (buff 32))
    (env-score        uint)
    (labor-score      uint)
    (quality-score    uint))
  (let
    (
      (product-id (+ (var-get total-products) u1))
    )
    (asserts! (is-registered-supplier tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (valid-score env-score)     ERR-INVALID-SCORE)
    (asserts! (valid-score labor-score)   ERR-INVALID-SCORE)
    (asserts! (valid-score quality-score) ERR-INVALID-SCORE)
    (map-set products product-id
      {
        provenance-dna:      provenance-dna,
        owner:               tx-sender,
        supplier:            tx-sender,
        name:                name,
        created-at:          block-height,
        current-stage:       u0,
        environmental-score: env-score,
        labor-score:         labor-score,
        quality-score:       quality-score,
        verified:            false,
        active:              true
      }
    )
    (var-set total-products product-id)
    (ok product-id)
  )
)

;; Advance a product to the next supply chain stage and record stage event
;; Stages: 0=raw material, 1=manufacturing, 2=logistics, 3=retail, 4=sold
(define-public (record-stage-event
    (product-id       uint)
    (location-hash    (buff 32))
    (sensor-data-hash (buff 32))
    (notes            (string-ascii 128)))
  (match (map-get? products product-id)
    product
    (let ((next-stage (+ (get current-stage product) u1)))
      (asserts! (is-eq tx-sender (get supplier product)) ERR-NOT-AUTHORIZED)
      (asserts! (get active product)                     ERR-PRODUCT-NOT-FOUND)
      (asserts! (<= next-stage u4)                       ERR-INVALID-STAGE)
      (map-set products product-id
        (merge product { current-stage: next-stage })
      )
      (map-set stage-events
        { product-id: product-id, stage: next-stage }
        {
          recorded-by:      tx-sender,
          recorded-at:      block-height,
          location-hash:    location-hash,
          notes:            notes,
          sensor-data-hash: sensor-data-hash,
          verified:         false
        }
      )
      (ok next-stage)
    )
    ERR-PRODUCT-NOT-FOUND
  )
)

;; Transfer product ownership (e.g., supplier to retailer)
(define-public (transfer-product (product-id uint) (new-owner principal))
  (match (map-get? products product-id)
    product
    (begin
      (asserts! (is-eq tx-sender (get owner product)) ERR-NOT-AUTHORIZED)
      (asserts! (get active product)                  ERR-PRODUCT-NOT-FOUND)
      (map-set products product-id
        (merge product { owner: new-owner })
      )
      (ok true)
    )
    ERR-PRODUCT-NOT-FOUND
  )
)

;; Read-only: get product details (consumer transparency portal)
(define-read-only (get-product (product-id uint))
  (map-get? products product-id)
)

;; Read-only: get stage event details
(define-read-only (get-stage-event (product-id uint) (stage uint))
  (map-get? stage-events { product-id: product-id, stage: stage })
)

;; Read-only: total products registered on platform
(define-read-only (get-total-products)
  (var-get total-products)
)

;; ============================================================
;; VALIDATOR / PROOF OF IMPACT FUNCTIONS
;; ============================================================

;; Register as a validator (must be approved by contract owner)
(define-public (register-validator (validator-addr principal))
  (begin
    (asserts! (is-contract-owner)                             ERR-NOT-AUTHORIZED)
    (asserts! (not (is-registered-validator validator-addr))  ERR-ALREADY-REGISTERED)
    (map-set validators validator-addr
      {
        registered-at:      block-height,
        verifications-done: u0,
        impact-score:       u0,
        active:             true
      }
    )
    (var-set total-validators (+ (var-get total-validators) u1))
    (ok true)
  )
)

;; Validator submits a compliance verification for a product
;; This implements the multi-tier verification protocol
(define-public (submit-verification
    (product-id       uint)
    (env-score        uint)
    (labor-score      uint)
    (quality-score    uint)
    (notes            (string-ascii 128)))
  (match (map-get? products product-id)
    product
    (begin
      (asserts! (is-registered-validator tx-sender)   ERR-NOT-AUTHORIZED)
      (asserts! (get active product)                  ERR-PRODUCT-NOT-FOUND)
      (asserts! (is-none
                  (map-get? verifications
                    { product-id: product-id, validator: tx-sender }))
                ERR-ALREADY-VERIFIED)
      (asserts! (valid-score env-score)     ERR-INVALID-SCORE)
      (asserts! (valid-score labor-score)   ERR-INVALID-SCORE)
      (asserts! (valid-score quality-score) ERR-INVALID-SCORE)
      (let
        (
          (compliance (compute-compliance env-score labor-score quality-score))
          (validator  (unwrap-panic (map-get? validators tx-sender)))
        )
        ;; Store verification record
        (map-set verifications
          { product-id: product-id, validator: tx-sender }
          {
            verified-at:         block-height,
            compliance-score:    compliance,
            environmental-score: env-score,
            labor-score:         labor-score,
            quality-score:       quality-score,
            notes:               notes
          }
        )
        ;; Update product scores and mark verified
        (map-set products product-id
          (merge product
            {
              environmental-score: env-score,
              labor-score:         labor-score,
              quality-score:       quality-score,
              verified:            true
            }
          )
        )
        ;; Update validator Proof of Impact score and verification count
        (map-set validators tx-sender
          (merge validator
            {
              verifications-done: (+ (get verifications-done validator) u1),
              impact-score:       (+ (get impact-score validator) compliance)
            }
          )
        )
        ;; Reward validator with reputation tokens (validators earn supplier tokens)
        (award-tokens (get supplier product) REWARD-VERIFICATION)
        ;; Bonus reward for high compliance
        (if (>= compliance HIGH-COMPLIANCE-THRESHOLD)
          (award-tokens (get supplier product) REWARD-HIGH-COMPLIANCE)
          false
        )
        ;; Update supplier compliance score
        (match (map-get? suppliers (get supplier product))
          s (map-set suppliers (get supplier product)
              (merge s { compliance-score: compliance })
            )
          false
        )
        (ok compliance)
      )
    )
    ERR-PRODUCT-NOT-FOUND
  )
)

;; Read-only: get validator info
(define-read-only (get-validator (addr principal))
  (map-get? validators addr)
)

;; Read-only: get verification record for a product by a specific validator
(define-read-only (get-verification (product-id uint) (validator principal))
  (map-get? verifications { product-id: product-id, validator: validator })
)

;; Read-only: total validators registered
(define-read-only (get-total-validators)
  (var-get total-validators)
)

;; ============================================================
;; CONSUMER TRANSPARENCY PORTAL
;; ============================================================

;; Consumer scans QR code to view product provenance
;; Records the scan event on-chain for analytics
(define-public (scan-product (product-id uint))
  (match (map-get? products product-id)
    product
    (let
      (
        (scan-key { product-id: product-id, consumer: tx-sender })
        (existing  (default-to
                     { scan-count: u0, last-scanned: u0 }
                     (map-get? consumer-scans scan-key)))
      )
      (asserts! (get active product) ERR-PRODUCT-NOT-FOUND)
      (map-set consumer-scans scan-key
        {
          scan-count:   (+ (get scan-count existing) u1),
          last-scanned: block-height
        }
      )
      ;; Return product transparency data
      (ok {
        name:                (get name product),
        provenance-dna:      (get provenance-dna product),
        supplier:            (get supplier product),
        current-stage:       (get current-stage product),
        environmental-score: (get environmental-score product),
        labor-score:         (get labor-score product),
        quality-score:       (get quality-score product),
        verified:            (get verified product)
      })
    )
    ERR-PRODUCT-NOT-FOUND
  )
)

;; Read-only: get consumer scan data for a product
(define-read-only (get-consumer-scans (product-id uint) (consumer principal))
  (map-get? consumer-scans { product-id: product-id, consumer: consumer })
)

;; ============================================================
;; ADMIN FUNCTIONS
;; ============================================================

;; Deactivate a product (e.g., counterfeit detected)
(define-public (deactivate-product (product-id uint))
  (match (map-get? products product-id)
    product
    (begin
      (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
      (map-set products product-id (merge product { active: false }))
      (ok true)
    )
    ERR-PRODUCT-NOT-FOUND
  )
)

;; Deactivate a validator
(define-public (deactivate-validator (validator-addr principal))
  (match (map-get? validators validator-addr)
    v
    (begin
      (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
      (map-set validators validator-addr (merge v { active: false }))
      (ok true)
    )
    ERR-VALIDATOR-NOT-FOUND
  )
)
