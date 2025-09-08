;; International Trade Customs Management System
;; A comprehensive blockchain-based system for managing international trade customs declarations,
;; duty calculations, payment processing, and goods clearance with automated compliance tracking
;; and multi-stakeholder authorization controls for enhanced transparency and efficiency.

;; ERROR CONSTANTS
(define-constant ERR-UNAUTHORIZED-ACCESS (err u100))
(define-constant ERR-INVALID-PAYMENT-AMOUNT (err u101))
(define-constant ERR-DUTY-ALREADY-SETTLED (err u102))
(define-constant ERR-DECLARATION-NOT-FOUND (err u103))
(define-constant ERR-INVALID-DUTY-RATE (err u104))
(define-constant ERR-GOODS-ALREADY-CLEARED (err u105))
(define-constant ERR-PAYMENT-NOT-COMPLETED (err u106))
(define-constant ERR-INVALID-GOODS-CATEGORY (err u107))
(define-constant ERR-SYSTEM-TEMPORARILY-DISABLED (err u108))
(define-constant ERR-INSUFFICIENT-GOODS-VALUE (err u109))
(define-constant ERR-CATEGORY-NOT-ACTIVE (err u110))
(define-constant ERR-INVALID-PRINCIPAL (err u111))
(define-constant ERR-INVALID-STRING-LENGTH (err u112))
(define-constant ERR-INVALID-DECLARATION-ID (err u113))

;; SYSTEM CONSTANTS
(define-constant contract-deployer tx-sender)
(define-constant max-duty-rate-basis-points u10000) ;; 100% maximum duty rate
(define-constant min-goods-value-threshold u1) ;; Minimum goods value for processing
(define-constant duty-rate-precision-factor u10000) ;; Basis points conversion factor
(define-constant max-category-name-length u50)
(define-constant max-description-length u200)
;; STATE VARIABLES
(define-data-var authorized-customs-officer principal contract-deployer)
(define-data-var current-declaration-counter uint u1)
(define-data-var system-operational-status bool true)
(define-data-var total-processed-declarations uint u0)
(define-data-var total-collected-duties uint u0)

;; DATA STRUCTURES
;; Comprehensive goods category with duty configuration
(define-map goods-category-duty-rates
  { goods-category-name: (string-ascii 50) }
  { 
    duty-rate-in-basis-points: uint, 
    category-active-status: bool,
    last-updated-block-height: uint,
    category-description: (string-ascii 100)
  }
)

;; Detailed customs declaration record
(define-map international-trade-declarations
  { unique-declaration-identifier: uint }
  {
    importing-business-principal: principal,
    declared-goods-total-value: uint,
    assigned-goods-category: (string-ascii 50),
    calculated-duty-obligation: uint,
    payment-completion-status: bool,
    customs-clearance-status: bool,
    declaration-submission-block-height: uint,
    duty-payment-block-height: (optional uint),
    goods-clearance-block-height: (optional uint),
    additional-processing-notes: (string-ascii 200)
  }
)

;; Business import activity tracking
(define-map importing-business-activity-records
  { business-principal-address: principal }
  {
    total-submitted-declarations: uint,
    cumulative-duties-paid-amount: uint,
    pending-clearance-declarations-count: uint,
    business-registration-block-height: uint,
    last-activity-block-height: uint
  }
)

;; System audit trail for transparency
(define-map customs-system-audit-log
  { audit-entry-identifier: uint }
  {
    action-performed: (string-ascii 100),
    authorized-user-principal: principal,
    affected-declaration-id: (optional uint),
    action-block-height: uint,
    additional-audit-details: (string-ascii 200)
  }
)

(define-data-var next-audit-entry-id uint u1)

;; INPUT VALIDATION UTILITIES
(define-private (validate-principal-input (input-principal principal))
  (and 
    (not (is-eq input-principal 'SP000000000000000000002Q6VF78))
    (not (is-eq input-principal 'ST000000000000000000002AMW42H))
  )
)

(define-private (validate-string-length (input-string (string-ascii 200)) (max-length uint))
  (<= (len input-string) max-length)
)

(define-private (validate-declaration-id (id uint))
  (and (> id u0) (< id (var-get current-declaration-counter)))
)

(define-private (validate-category-name (name (string-ascii 50)))
  (and 
    (> (len name) u0)
    (<= (len name) max-category-name-length)
  )
)

;; AUTHORIZATION & VALIDATION UTILITIES
(define-private (verify-contract-deployer-access)
  (is-eq tx-sender contract-deployer)
)

(define-private (verify-customs-officer-access)
  (is-eq tx-sender (var-get authorized-customs-officer))
)

(define-private (verify-system-administrator-privileges)
  (or (verify-contract-deployer-access) (verify-customs-officer-access))
)

(define-private (validate-system-operational-state)
  (var-get system-operational-status)
)

(define-private (validate-goods-category-exists (category-name (string-ascii 50)))
  (is-some (map-get? goods-category-duty-rates { goods-category-name: category-name }))
)

(define-private (validate-goods-category-active-status (category-name (string-ascii 50)))
  (match (map-get? goods-category-duty-rates { goods-category-name: category-name })
    category-configuration (get category-active-status category-configuration)
    false
  )
)

;; BUSINESS LOGIC UTILITIES
(define-private (compute-duty-amount-from-value-and-rate (goods-value uint) (rate-basis-points uint))
  (/ (* goods-value rate-basis-points) duty-rate-precision-factor)
)

(define-private (get-current-block-height)
  stacks-block-height
)

(define-private (record-audit-entry (action-description (string-ascii 100)) (declaration-id (optional uint)) (details (string-ascii 200)))
  (let (
    (audit-id (var-get next-audit-entry-id))
    (current-block (get-current-block-height))
  )
    (begin
      (map-set customs-system-audit-log
        { audit-entry-identifier: audit-id }
        {
          action-performed: action-description,
          authorized-user-principal: tx-sender,
          affected-declaration-id: declaration-id,
          action-block-height: current-block,
          additional-audit-details: details
        }
      )
      (var-set next-audit-entry-id (+ audit-id u1))
      audit-id
    )
  )
)

(define-private (update-business-activity-statistics (business-principal principal) (action-type (string-ascii 20)) (amount uint))
  (match (map-get? importing-business-activity-records { business-principal-address: business-principal })
    existing-record (let (
      (current-block (get-current-block-height))
      (updated-record (merge existing-record { last-activity-block-height: current-block }))
    )
      (begin
        (if (is-eq action-type "new-declaration")
          (map-set importing-business-activity-records
            { business-principal-address: business-principal }
            (merge updated-record {
              total-submitted-declarations: (+ (get total-submitted-declarations updated-record) u1),
              pending-clearance-declarations-count: (+ (get pending-clearance-declarations-count updated-record) u1)
            })
          )
          (if (is-eq action-type "payment-completed")
            (map-set importing-business-activity-records
              { business-principal-address: business-principal }
              (merge updated-record {
                cumulative-duties-paid-amount: (+ (get cumulative-duties-paid-amount updated-record) amount)
              })
            )
            (if (is-eq action-type "goods-cleared")
              (map-set importing-business-activity-records
                { business-principal-address: business-principal }
                (merge updated-record {
                  pending-clearance-declarations-count: (if (> (get pending-clearance-declarations-count updated-record) u0)
                    (- (get pending-clearance-declarations-count updated-record) u1)
                    u0)
                })
              )
              true
            )
          )
        )
        true
      )
    )
    ;; Create new business record
    (let (
      (current-block (get-current-block-height))
    )
      (begin
        (map-set importing-business-activity-records
          { business-principal-address: business-principal }
          {
            total-submitted-declarations: u1,
            cumulative-duties-paid-amount: u0,
            pending-clearance-declarations-count: u1,
            business-registration-block-height: current-block,
            last-activity-block-height: current-block
          }
        )
        true
      )
    )
  )
)

;; ADMINISTRATIVE FUNCTIONS
(define-public (designate-new-customs-officer (new-officer-principal principal))
  (let ((validated-principal new-officer-principal))
    (begin
      (asserts! (verify-contract-deployer-access) ERR-UNAUTHORIZED-ACCESS)
      (asserts! (validate-system-operational-state) ERR-SYSTEM-TEMPORARILY-DISABLED)
      (asserts! (validate-principal-input validated-principal) ERR-INVALID-PRINCIPAL)
      (var-set authorized-customs-officer validated-principal)
      (record-audit-entry "customs-officer-designation" none "New customs officer designated")
      (ok validated-principal)
    )
  )
)

(define-public (temporarily-disable-system)
  (begin
    (asserts! (verify-system-administrator-privileges) ERR-UNAUTHORIZED-ACCESS)
    (var-set system-operational-status false)
    (record-audit-entry "system-disabled" none "System temporarily disabled by administrator")
    (ok true)
  )
)

(define-public (reactivate-system-operations)
  (begin
    (asserts! (verify-system-administrator-privileges) ERR-UNAUTHORIZED-ACCESS)
    (var-set system-operational-status true)
    (record-audit-entry "system-reactivated" none "System operations restored by administrator")
    (ok true)
  )
)

(define-public (configure-goods-category-duty-rate 
  (category-name (string-ascii 50)) 
  (rate-basis-points uint) 
  (category-description (string-ascii 100)))
  (let (
    (validated-category-name category-name)
    (validated-description category-description)
  )
    (begin
      (asserts! (verify-system-administrator-privileges) ERR-UNAUTHORIZED-ACCESS)
      (asserts! (validate-system-operational-state) ERR-SYSTEM-TEMPORARILY-DISABLED)
      (asserts! (<= rate-basis-points max-duty-rate-basis-points) ERR-INVALID-DUTY-RATE)
      (asserts! (validate-category-name validated-category-name) ERR-INVALID-STRING-LENGTH)
      (asserts! (validate-string-length validated-description u100) ERR-INVALID-STRING-LENGTH)
      
      (map-set goods-category-duty-rates
        { goods-category-name: validated-category-name }
        {
          duty-rate-in-basis-points: rate-basis-points,
          category-active-status: true,
          last-updated-block-height: stacks-block-height,
          category-description: validated-description
        }
      )
      
      (record-audit-entry 
        "duty-rate-configured" 
        none 
        "Goods category duty rate configured successfully"
      )
      (ok validated-category-name)
    )
  )
)

(define-public (deactivate-goods-category (category-name (string-ascii 50)))
  (let ((validated-category-name category-name))
    (begin
      (asserts! (verify-system-administrator-privileges) ERR-UNAUTHORIZED-ACCESS)
      (asserts! (validate-system-operational-state) ERR-SYSTEM-TEMPORARILY-DISABLED)
      (asserts! (validate-category-name validated-category-name) ERR-INVALID-STRING-LENGTH)
      
      (match (map-get? goods-category-duty-rates { goods-category-name: validated-category-name })
        existing-configuration (begin
          (map-set goods-category-duty-rates
            { goods-category-name: validated-category-name }
            (merge existing-configuration { 
              category-active-status: false,
              last-updated-block-height: stacks-block-height 
            })
          )
          (record-audit-entry "category-deactivated" none "Goods category deactivated successfully")
          (ok validated-category-name)
        )
        ERR-DECLARATION-NOT-FOUND
      )
    )
  )
)

;; CORE BUSINESS FUNCTIONS
(define-public (submit-customs-declaration 
  (goods-total-value uint) 
  (goods-category (string-ascii 50))
  (processing-notes (string-ascii 200)))
  (let (
    (declaration-id (var-get current-declaration-counter))
    (submission-block (get-current-block-height))
    (validated-notes processing-notes)
  )
    (begin
      (asserts! (validate-system-operational-state) ERR-SYSTEM-TEMPORARILY-DISABLED)
      (asserts! (>= goods-total-value min-goods-value-threshold) ERR-INSUFFICIENT-GOODS-VALUE)
      (asserts! (validate-goods-category-exists goods-category) ERR-INVALID-GOODS-CATEGORY)
      (asserts! (validate-goods-category-active-status goods-category) ERR-CATEGORY-NOT-ACTIVE)
      (asserts! (validate-string-length validated-notes u200) ERR-INVALID-STRING-LENGTH)
      
      (let (
        (category-config (unwrap-panic (map-get? goods-category-duty-rates { goods-category-name: goods-category })))
        (calculated-duty (compute-duty-amount-from-value-and-rate 
          goods-total-value 
          (get duty-rate-in-basis-points category-config)))
      )
        (begin
          ;; Create comprehensive declaration record
          (map-set international-trade-declarations
            { unique-declaration-identifier: declaration-id }
            {
              importing-business-principal: tx-sender,
              declared-goods-total-value: goods-total-value,
              assigned-goods-category: goods-category,
              calculated-duty-obligation: calculated-duty,
              payment-completion-status: false,
              customs-clearance-status: false,
              declaration-submission-block-height: submission-block,
              duty-payment-block-height: none,
              goods-clearance-block-height: none,
              additional-processing-notes: validated-notes
            }
          )
          
          ;; Update business activity tracking
          (update-business-activity-statistics tx-sender "new-declaration" u0)
          
          ;; Update system counters
          (var-set current-declaration-counter (+ declaration-id u1))
          (var-set total-processed-declarations (+ (var-get total-processed-declarations) u1))
          
          ;; Create audit trail
          (record-audit-entry 
            "declaration-submitted" 
            (some declaration-id)
            "New customs declaration submitted successfully"
          )
          
          (ok declaration-id)
        )
      )
    )
  )
)

(define-public (process-duty-payment (declaration-id uint))
  (let ((validated-declaration-id declaration-id))
    (begin
      (asserts! (validate-declaration-id validated-declaration-id) ERR-INVALID-DECLARATION-ID)
      
      (match (map-get? international-trade-declarations { unique-declaration-identifier: validated-declaration-id })
        declaration-record (let (
          (payment-block (get-current-block-height))
        )
          (begin
            (asserts! (validate-system-operational-state) ERR-SYSTEM-TEMPORARILY-DISABLED)
            (asserts! (is-eq tx-sender (get importing-business-principal declaration-record)) ERR-UNAUTHORIZED-ACCESS)
            (asserts! (not (get payment-completion-status declaration-record)) ERR-DUTY-ALREADY-SETTLED)
            
            ;; Note: In production, integrate with actual STX transfer mechanism
            ;; (try! (stx-transfer? (get calculated-duty-obligation declaration-record) 
            ;;                     tx-sender 
            ;;                     (var-get authorized-customs-officer)))
            
            ;; Update declaration with payment information
            (map-set international-trade-declarations
              { unique-declaration-identifier: validated-declaration-id }
              (merge declaration-record {
                payment-completion-status: true,
                duty-payment-block-height: (some payment-block)
              })
            )
            
            ;; Update business activity statistics
            (update-business-activity-statistics 
              tx-sender 
              "payment-completed" 
              (get calculated-duty-obligation declaration-record))
            
            ;; Update system totals
            (var-set total-collected-duties 
              (+ (var-get total-collected-duties) (get calculated-duty-obligation declaration-record)))
            
            ;; Create audit entry
            (record-audit-entry 
              "duty-payment-processed" 
              (some validated-declaration-id)
              "Duty payment processed successfully"
            )
            
            (ok true)
          )
        )
        ERR-DECLARATION-NOT-FOUND
      )
    )
  )
)

(define-public (authorize-goods-clearance (declaration-id uint) (clearance-notes (string-ascii 200)))
  (let (
    (validated-declaration-id declaration-id)
    (validated-clearance-notes clearance-notes)
  )
    (begin
      (asserts! (validate-declaration-id validated-declaration-id) ERR-INVALID-DECLARATION-ID)
      (asserts! (validate-string-length validated-clearance-notes u200) ERR-INVALID-STRING-LENGTH)
      
      (match (map-get? international-trade-declarations { unique-declaration-identifier: validated-declaration-id })
        declaration-record (let (
          (clearance-block (get-current-block-height))
        )
          (begin
            (asserts! (verify-system-administrator-privileges) ERR-UNAUTHORIZED-ACCESS)
            (asserts! (validate-system-operational-state) ERR-SYSTEM-TEMPORARILY-DISABLED)
            (asserts! (get payment-completion-status declaration-record) ERR-PAYMENT-NOT-COMPLETED)
            (asserts! (not (get customs-clearance-status declaration-record)) ERR-GOODS-ALREADY-CLEARED)
            
            ;; Update declaration with clearance authorization
            (map-set international-trade-declarations
              { unique-declaration-identifier: validated-declaration-id }
              (merge declaration-record {
                customs-clearance-status: true,
                goods-clearance-block-height: (some clearance-block),
                additional-processing-notes: validated-clearance-notes
              })
            )
            
            ;; Update business activity statistics
            (update-business-activity-statistics 
              (get importing-business-principal declaration-record) 
              "goods-cleared" 
              u0)
            
            ;; Create comprehensive audit entry
            (record-audit-entry 
              "goods-clearance-authorized" 
              (some validated-declaration-id)
              "Goods clearance authorized successfully"
            )
            
            (ok true)
          )
        )
        ERR-DECLARATION-NOT-FOUND
      )
    )
  )
)

;; READ-ONLY QUERY FUNCTIONS
(define-read-only (get-declaration-details (declaration-id uint))
  (map-get? international-trade-declarations { unique-declaration-identifier: declaration-id })
)

(define-read-only (get-goods-category-information (category-name (string-ascii 50)))
  (map-get? goods-category-duty-rates { goods-category-name: category-name })
)

(define-read-only (get-business-import-activity (business-principal principal))
  (default-to 
    { 
      total-submitted-declarations: u0, 
      cumulative-duties-paid-amount: u0, 
      pending-clearance-declarations-count: u0,
      business-registration-block-height: u0,
      last-activity-block-height: u0
    }
    (map-get? importing-business-activity-records { business-principal-address: business-principal })
  )
)

(define-read-only (calculate-estimated-duty-cost (goods-value uint) (category-name (string-ascii 50)))
  (match (map-get? goods-category-duty-rates { goods-category-name: category-name })
    category-config (if (get category-active-status category-config)
      (ok (compute-duty-amount-from-value-and-rate goods-value (get duty-rate-in-basis-points category-config)))
      ERR-CATEGORY-NOT-ACTIVE
    )
    ERR-INVALID-GOODS-CATEGORY
  )
)

(define-read-only (get-comprehensive-system-status)
  {
    contract-deployer: contract-deployer,
    authorized-customs-officer: (var-get authorized-customs-officer),
    current-declaration-counter: (var-get current-declaration-counter),
    system-operational-status: (var-get system-operational-status),
    total-processed-declarations: (var-get total-processed-declarations),
    total-collected-duties: (var-get total-collected-duties),
    current-block-height: stacks-block-height
  }
)

(define-read-only (verify-declaration-clearance-status (declaration-id uint))
  (match (map-get? international-trade-declarations { unique-declaration-identifier: declaration-id })
    declaration-record (ok (get customs-clearance-status declaration-record))
    ERR-DECLARATION-NOT-FOUND
  )
)

(define-read-only (get-audit-trail-entry (audit-id uint))
  (map-get? customs-system-audit-log { audit-entry-identifier: audit-id })
)

;; Get current block height information
(define-read-only (get-current-block-info)
  {
    current-block-height: stacks-block-height,
    contract-caller: contract-caller,
    tx-sender: tx-sender
  }
)

;; SYSTEM INITIALIZATION
;; Initialize comprehensive goods categories with detailed configurations
(map-set goods-category-duty-rates 
  { goods-category-name: "consumer-electronics" } 
  { duty-rate-in-basis-points: u500, category-active-status: true, last-updated-block-height: stacks-block-height, category-description: "Consumer electronic devices and accessories" })

(map-set goods-category-duty-rates 
  { goods-category-name: "textile-apparel" } 
  { duty-rate-in-basis-points: u1000, category-active-status: true, last-updated-block-height: stacks-block-height, category-description: "Clothing, fabrics, and textile products" })

(map-set goods-category-duty-rates 
  { goods-category-name: "industrial-machinery" } 
  { duty-rate-in-basis-points: u750, category-active-status: true, last-updated-block-height: stacks-block-height, category-description: "Heavy machinery and industrial equipment" })

(map-set goods-category-duty-rates 
  { goods-category-name: "food-beverages" } 
  { duty-rate-in-basis-points: u200, category-active-status: true, last-updated-block-height: stacks-block-height, category-description: "Food products and beverages" })

(map-set goods-category-duty-rates 
  { goods-category-name: "chemical-pharmaceuticals" } 
  { duty-rate-in-basis-points: u300, category-active-status: true, last-updated-block-height: stacks-block-height, category-description: "Chemical compounds and pharmaceutical products" })

(map-set goods-category-duty-rates 
  { goods-category-name: "automotive-parts" } 
  { duty-rate-in-basis-points: u600, category-active-status: true, last-updated-block-height: stacks-block-height, category-description: "Vehicle parts and automotive accessories" })