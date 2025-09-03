;; Subscription Box Management Smart Contract
;; Manages recurring delivery services with product curation, inventory tracking, and billing

;; Constants
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_SUBSCRIPTION_NOT_FOUND (err u101))
(define-constant ERR_INVALID_PLAN (err u102))
(define-constant ERR_INSUFFICIENT_BALANCE (err u103))
(define-constant ERR_PRODUCT_NOT_FOUND (err u104))
(define-constant ERR_INSUFFICIENT_INVENTORY (err u105))
(define-constant ERR_SUBSCRIPTION_INACTIVE (err u106))

(define-constant CONTRACT_OWNER tx-sender)

;; Data Variables
(define-data-var next-subscription-id uint u1)
(define-data-var next-product-id uint u1)

;; Data Maps

;; Subscription Plans
(define-map subscription-plans
  { plan-id: uint }
  {
    name: (string-ascii 50),
    price: uint,
    duration-blocks: uint,
    max-products: uint,
    active: bool
  }
)

;; Customer Subscriptions
(define-map subscriptions
  { subscription-id: uint }
  {
    customer: principal,
    plan-id: uint,
    start-block: uint,
    next-billing-block: uint,
    active: bool,
    auto-renew: bool
  }
)

;; Customer Preferences
(define-map customer-preferences
  { customer: principal }
  {
    categories: (list 5 (string-ascii 20)),
    dietary-restrictions: (list 3 (string-ascii 30)),
    preferred-brands: (list 5 (string-ascii 30))
  }
)

;; Product Inventory
(define-map products
  { product-id: uint }
  {
    name: (string-ascii 50),
    category: (string-ascii 20),
    brand: (string-ascii 30),
    stock-quantity: uint,
    price: uint,
    active: bool
  }
)

;; Subscription Box Contents
(define-map box-contents
  { subscription-id: uint, cycle: uint }
  {
    products: (list 10 uint),
    shipped: bool,
    tracking-id: (optional (string-ascii 50))
  }
)

;; Billing History
(define-map billing-history
  { subscription-id: uint, cycle: uint }
  {
    amount: uint,
    payment-block: uint,
    status: (string-ascii 10)
  }
)

;; Customer-to-Subscription mapping
(define-map customer-subscriptions
  { customer: principal }
  { subscription-id: uint }
)

;; Admin Functions

;; Create subscription plan
(define-public (create-subscription-plan (name (string-ascii 50)) (price uint) (duration-blocks uint) (max-products uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (let ((plan-id (var-get next-subscription-id)))
      (map-set subscription-plans
        { plan-id: plan-id }
        {
          name: name,
          price: price,
          duration-blocks: duration-blocks,
          max-products: max-products,
          active: true
        }
      )
      (var-set next-subscription-id (+ plan-id u1))
      (ok plan-id)
    )
  )
)

;; Add product to inventory
(define-public (add-product (name (string-ascii 50)) (category (string-ascii 20)) (brand (string-ascii 30)) (stock-quantity uint) (price uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (let ((product-id (var-get next-product-id)))
      (map-set products
        { product-id: product-id }
        {
          name: name,
          category: category,
          brand: brand,
          stock-quantity: stock-quantity,
          price: price,
          active: true
        }
      )
      (var-set next-product-id (+ product-id u1))
      (ok product-id)
    )
  )
)

;; Update product inventory
(define-public (update-product-stock (product-id uint) (new-quantity uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (match (map-get? products { product-id: product-id })
      product-data
        (begin
          (map-set products
            { product-id: product-id }
            (merge product-data { stock-quantity: new-quantity })
          )
          (ok true)
        )
      ERR_PRODUCT_NOT_FOUND
    )
  )
)

;; Customer Functions

;; Set customer preferences
(define-public (set-preferences (categories (list 5 (string-ascii 20))) (dietary-restrictions (list 3 (string-ascii 30))) (preferred-brands (list 5 (string-ascii 30))))
  (begin
    (map-set customer-preferences
      { customer: tx-sender }
      {
        categories: categories,
        dietary-restrictions: dietary-restrictions,
        preferred-brands: preferred-brands
      }
    )
    (ok true)
  )
)

;; Subscribe to a plan
(define-public (subscribe (plan-id uint))
  (begin
    (match (map-get? subscription-plans { plan-id: plan-id })
      plan-data
        (if (get active plan-data)
          (let (
            (subscription-id (var-get next-subscription-id))
            (current-block stacks-block-height)
            (next-billing (+ current-block (get duration-blocks plan-data)))
          )
            (map-set subscriptions
              { subscription-id: subscription-id }
              {
                customer: tx-sender,
                plan-id: plan-id,
                start-block: current-block,
                next-billing-block: next-billing,
                active: true,
                auto-renew: true
              }
            )
            (map-set customer-subscriptions
              { customer: tx-sender }
              { subscription-id: subscription-id }
            )
            (var-set next-subscription-id (+ subscription-id u1))
            (ok subscription-id)
          )
          ERR_INVALID_PLAN
        )
      ERR_INVALID_PLAN
    )
  )
)

;; Cancel subscription
(define-public (cancel-subscription (subscription-id uint))
  (begin
    (match (map-get? subscriptions { subscription-id: subscription-id })
      subscription-data
        (if (is-eq (get customer subscription-data) tx-sender)
          (begin
            (map-set subscriptions
              { subscription-id: subscription-id }
              (merge subscription-data { active: false, auto-renew: false })
            )
            (ok true)
          )
          ERR_NOT_AUTHORIZED
        )
      ERR_SUBSCRIPTION_NOT_FOUND
    )
  )
)

;; Toggle auto-renewal
(define-public (toggle-auto-renew (subscription-id uint))
  (begin
    (match (map-get? subscriptions { subscription-id: subscription-id })
      subscription-data
        (if (is-eq (get customer subscription-data) tx-sender)
          (begin
            (map-set subscriptions
              { subscription-id: subscription-id }
              (merge subscription-data { auto-renew: (not (get auto-renew subscription-data)) })
            )
            (ok (not (get auto-renew subscription-data)))
          )
          ERR_NOT_AUTHORIZED
        )
      ERR_SUBSCRIPTION_NOT_FOUND
    )
  )
)

;; Admin Billing Functions

;; Process billing for a subscription
(define-public (process-billing (subscription-id uint) (cycle uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (match (map-get? subscriptions { subscription-id: subscription-id })
      subscription-data
        (if (and (get active subscription-data) (<= (get next-billing-block subscription-data) stacks-block-height))
          (match (map-get? subscription-plans { plan-id: (get plan-id subscription-data) })
            plan-data
              (begin
                ;; Record billing
                (map-set billing-history
                  { subscription-id: subscription-id, cycle: cycle }
                  {
                    amount: (get price plan-data),
                    payment-block: stacks-block-height,
                    status: "paid"
                  }
                )
                ;; Update next billing block
                (map-set subscriptions
                  { subscription-id: subscription-id }
                  (merge subscription-data {
                    next-billing-block: (+ stacks-block-height (get duration-blocks plan-data))
                  })
                )
                (ok true)
              )
            ERR_INVALID_PLAN
          )
          ERR_SUBSCRIPTION_INACTIVE
        )
      ERR_SUBSCRIPTION_NOT_FOUND
    )
  )
)

;; Curate box contents based on preferences
(define-public (curate-box (subscription-id uint) (cycle uint) (product-ids (list 10 uint)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (match (map-get? subscriptions { subscription-id: subscription-id })
      subscription-data
        (if (get active subscription-data)
          (begin
            ;; Validate all products exist and have sufficient stock
            (asserts! (fold validate-product-availability product-ids true) ERR_INSUFFICIENT_INVENTORY)
            ;; Deduct inventory for each product
            (fold deduct-product-stock product-ids true)
            ;; Set box contents
            (map-set box-contents
              { subscription-id: subscription-id, cycle: cycle }
              {
                products: product-ids,
                shipped: false,
                tracking-id: none
              }
            )
            (ok true)
          )
          ERR_SUBSCRIPTION_INACTIVE
        )
      ERR_SUBSCRIPTION_NOT_FOUND
    )
  )
)

;; Mark box as shipped
(define-public (mark-shipped (subscription-id uint) (cycle uint) (tracking-id (string-ascii 50)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (match (map-get? box-contents { subscription-id: subscription-id, cycle: cycle })
      box-data
        (begin
          (map-set box-contents
            { subscription-id: subscription-id, cycle: cycle }
            (merge box-data {
              shipped: true,
              tracking-id: (some tracking-id)
            })
          )
          (ok true)
        )
      ERR_SUBSCRIPTION_NOT_FOUND
    )
  )
)

;; Helper Functions

;; Validate product availability
(define-private (validate-product-availability (product-id uint) (acc bool))
  (if acc
    (match (map-get? products { product-id: product-id })
      product-data
        (and (get active product-data) (> (get stock-quantity product-data) u0))
      false
    )
    false
  )
)

;; Deduct product stock
(define-private (deduct-product-stock (product-id uint) (acc bool))
  (if acc
    (match (map-get? products { product-id: product-id })
      product-data
        (begin
          (map-set products
            { product-id: product-id }
            (merge product-data {
              stock-quantity: (- (get stock-quantity product-data) u1)
            })
          )
          true
        )
      false
    )
    acc
  )
)

;; Read-only Functions

;; Get subscription details
(define-read-only (get-subscription (subscription-id uint))
  (map-get? subscriptions { subscription-id: subscription-id })
)

;; Get customer preferences
(define-read-only (get-customer-preferences (customer principal))
  (map-get? customer-preferences { customer: customer })
)

;; Get product details
(define-read-only (get-product (product-id uint))
  (map-get? products { product-id: product-id })
)

;; Get subscription plan
(define-read-only (get-subscription-plan (plan-id uint))
  (map-get? subscription-plans { plan-id: plan-id })
)

;; Get box contents
(define-read-only (get-box-contents (subscription-id uint) (cycle uint))
  (map-get? box-contents { subscription-id: subscription-id, cycle: cycle })
)

;; Get billing history
(define-read-only (get-billing-history (subscription-id uint) (cycle uint))
  (map-get? billing-history { subscription-id: subscription-id, cycle: cycle })
)

;; Get customer's subscription ID
(define-read-only (get-customer-subscription (customer principal))
  (map-get? customer-subscriptions { customer: customer })
)

;; Check if subscription needs billing
(define-read-only (needs-billing (subscription-id uint))
  (match (map-get? subscriptions { subscription-id: subscription-id })
    subscription-data
      (and
        (get active subscription-data)
        (<= (get next-billing-block subscription-data) stacks-block-height)
      )
    false
  )
)
