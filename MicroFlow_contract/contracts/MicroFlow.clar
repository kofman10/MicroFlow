;; title: MicroFlow
;; version: 1.0.0
;; summary: High-priority AMM pool for synthetic Bitcoin micropayments
;; description: Optimized for low latency and high transaction throughput on Stacks

;; traits
;;

;; token definitions
;; Using SIP-010 trait for fungible tokens
(define-trait sip-010-trait
  (
    (transfer (uint principal principal (optional (buff 34))) (response bool uint))
    (get-name () (response (string-ascii 32) uint))
    (get-symbol () (response (string-ascii 32) uint))
    (get-decimals () (response uint uint))
    (get-balance (principal) (response uint uint))
    (get-total-supply () (response uint uint))
    (get-token-uri () (response (optional (string-utf8 256)) uint))
  )
)

;; constants
;;
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-authorized (err u101))
(define-constant err-invalid-amount (err u102))
(define-constant err-insufficient-liquidity (err u103))
(define-constant err-slippage-exceeded (err u104))
(define-constant err-pool-exists (err u105))
(define-constant err-pool-not-found (err u106))
(define-constant err-zero-amount (err u107))
(define-constant err-invalid-ratio (err u108))
(define-constant err-paused (err u109))

;; Pool configuration constants
(define-constant fee-denominator u10000) ;; 0.01% precision
(define-constant default-fee-rate u30) ;; 0.3% fee
(define-constant min-liquidity u1000) ;; Minimum liquidity to prevent division by zero

;; data vars
;;
(define-data-var pool-nonce uint u0)
(define-data-var contract-paused bool false)
(define-data-var total-volume uint u0)
(define-data-var protocol-fee-accumulated uint u0)

;; data maps
;;
;; Pool data structure for AMM
(define-map pools
  { pool-id: uint }
  {
    token-x: principal,
    token-y: principal,
    reserve-x: uint,
    reserve-y: uint,
    lp-token-supply: uint,
    fee-rate: uint,
    active: bool
  }
)

;; LP token balances for each pool
(define-map lp-balances
  { pool-id: uint, owner: principal }
  { balance: uint }
)

;; User position tracking for micropayments optimization
(define-map user-positions
  { user: principal, pool-id: uint }
  {
    total-deposits-x: uint,
    total-deposits-y: uint,
    total-withdrawals: uint,
    last-interaction-block: uint
  }
)

;; Fast-path cache for recent swaps (optimization for high throughput)
(define-map recent-swaps
  { pool-id: uint, block-height: uint }
  {
    volume: uint,
    swap-count: uint
  }
)

;; public functions
;;

;; Create a new liquidity pool
(define-public (create-pool
    (token-x principal)
    (token-y principal)
    (initial-x uint)
    (initial-y uint))
  (let
    (
      (pool-id (var-get pool-nonce))
    )
    ;; Validations
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (not (var-get contract-paused)) err-paused)
    (asserts! (> initial-x u0) err-zero-amount)
    (asserts! (> initial-y u0) err-zero-amount)
    (asserts! (not (is-eq token-x token-y)) err-invalid-ratio)

    ;; Create pool
    (map-set pools
      { pool-id: pool-id }
      {
        token-x: token-x,
        token-y: token-y,
        reserve-x: initial-x,
        reserve-y: initial-y,
        lp-token-supply: (* initial-x initial-y),
        fee-rate: default-fee-rate,
        active: true
      }
    )

    ;; Mint initial LP tokens to creator
    (map-set lp-balances
      { pool-id: pool-id, owner: tx-sender }
      { balance: (* initial-x initial-y) }
    )

    ;; Increment pool nonce
    (var-set pool-nonce (+ pool-id u1))

    (ok pool-id)
  )
)

;; Add liquidity to existing pool
(define-public (add-liquidity
    (pool-id uint)
    (amount-x uint)
    (amount-y uint)
    (min-lp-tokens uint))
  (let
    (
      (pool (unwrap! (map-get? pools { pool-id: pool-id }) err-pool-not-found))
      (reserve-x (get reserve-x pool))
      (reserve-y (get reserve-y pool))
      (lp-supply (get lp-token-supply pool))
      (lp-tokens-from-x (/ (* amount-x lp-supply) reserve-x))
      (lp-tokens-from-y (/ (* amount-y lp-supply) reserve-y))
      (lp-tokens-to-mint (if (<= lp-tokens-from-x lp-tokens-from-y) lp-tokens-from-x lp-tokens-from-y))
      (current-balance (default-to u0 (get balance (map-get? lp-balances { pool-id: pool-id, owner: tx-sender }))))
    )
    ;; Validations
    (asserts! (not (var-get contract-paused)) err-paused)
    (asserts! (get active pool) err-pool-not-found)
    (asserts! (> amount-x u0) err-zero-amount)
    (asserts! (> amount-y u0) err-zero-amount)
    (asserts! (>= lp-tokens-to-mint min-lp-tokens) err-slippage-exceeded)

    ;; Update pool reserves
    (map-set pools
      { pool-id: pool-id }
      (merge pool {
        reserve-x: (+ reserve-x amount-x),
        reserve-y: (+ reserve-y amount-y),
        lp-token-supply: (+ lp-supply lp-tokens-to-mint)
      })
    )

    ;; Update LP token balance
    (map-set lp-balances
      { pool-id: pool-id, owner: tx-sender }
      { balance: (+ current-balance lp-tokens-to-mint) }
    )

    ;; Update user position tracking
    (update-user-position pool-id tx-sender amount-x amount-y u0)

    (ok lp-tokens-to-mint)
  )
)

;; Remove liquidity from pool
(define-public (remove-liquidity
    (pool-id uint)
    (lp-tokens uint)
    (min-amount-x uint)
    (min-amount-y uint))
  (let
    (
      (pool (unwrap! (map-get? pools { pool-id: pool-id }) err-pool-not-found))
      (reserve-x (get reserve-x pool))
      (reserve-y (get reserve-y pool))
      (lp-supply (get lp-token-supply pool))
      (current-balance (default-to u0 (get balance (map-get? lp-balances { pool-id: pool-id, owner: tx-sender }))))
      (amount-x (/ (* lp-tokens reserve-x) lp-supply))
      (amount-y (/ (* lp-tokens reserve-y) lp-supply))
    )
    ;; Validations
    (asserts! (not (var-get contract-paused)) err-paused)
    (asserts! (get active pool) err-pool-not-found)
    (asserts! (> lp-tokens u0) err-zero-amount)
    (asserts! (>= current-balance lp-tokens) err-insufficient-liquidity)
    (asserts! (>= amount-x min-amount-x) err-slippage-exceeded)
    (asserts! (>= amount-y min-amount-y) err-slippage-exceeded)

    ;; Update pool reserves
    (map-set pools
      { pool-id: pool-id }
      (merge pool {
        reserve-x: (- reserve-x amount-x),
        reserve-y: (- reserve-y amount-y),
        lp-token-supply: (- lp-supply lp-tokens)
      })
    )

    ;; Update LP token balance
    (map-set lp-balances
      { pool-id: pool-id, owner: tx-sender }
      { balance: (- current-balance lp-tokens) }
    )

    ;; Update user position tracking
    (update-user-position pool-id tx-sender u0 u0 (+ amount-x amount-y))

    (ok { amount-x: amount-x, amount-y: amount-y })
  )
)

;; Swap token X for token Y (optimized for micropayments)
(define-public (swap-x-for-y
    (pool-id uint)
    (amount-x uint)
    (min-amount-y uint))
  (let
    (
      (pool (unwrap! (map-get? pools { pool-id: pool-id }) err-pool-not-found))
      (reserve-x (get reserve-x pool))
      (reserve-y (get reserve-y pool))
      (fee-rate (get fee-rate pool))
      (amount-x-with-fee (- amount-x (/ (* amount-x fee-rate) fee-denominator)))
      (amount-y (/ (* amount-x-with-fee reserve-y) (+ reserve-x amount-x-with-fee)))
    )
    ;; Validations
    (asserts! (not (var-get contract-paused)) err-paused)
    (asserts! (get active pool) err-pool-not-found)
    (asserts! (> amount-x u0) err-zero-amount)
    (asserts! (>= amount-y min-amount-y) err-slippage-exceeded)
    (asserts! (> reserve-y amount-y) err-insufficient-liquidity)

    ;; Update pool reserves
    (map-set pools
      { pool-id: pool-id }
      (merge pool {
        reserve-x: (+ reserve-x amount-x),
        reserve-y: (- reserve-y amount-y)
      })
    )

    ;; Track volume and update cache
    (var-set total-volume (+ (var-get total-volume) amount-x))
    (update-swap-cache pool-id amount-x)

    (ok amount-y)
  )
)

;; Swap token Y for token X (optimized for micropayments)
(define-public (swap-y-for-x
    (pool-id uint)
    (amount-y uint)
    (min-amount-x uint))
  (let
    (
      (pool (unwrap! (map-get? pools { pool-id: pool-id }) err-pool-not-found))
      (reserve-x (get reserve-x pool))
      (reserve-y (get reserve-y pool))
      (fee-rate (get fee-rate pool))
      (amount-y-with-fee (- amount-y (/ (* amount-y fee-rate) fee-denominator)))
      (amount-x (/ (* amount-y-with-fee reserve-x) (+ reserve-y amount-y-with-fee)))
    )
    ;; Validations
    (asserts! (not (var-get contract-paused)) err-paused)
    (asserts! (get active pool) err-pool-not-found)
    (asserts! (> amount-y u0) err-zero-amount)
    (asserts! (>= amount-x min-amount-x) err-slippage-exceeded)
    (asserts! (> reserve-x amount-x) err-insufficient-liquidity)

    ;; Update pool reserves
    (map-set pools
      { pool-id: pool-id }
      (merge pool {
        reserve-x: (- reserve-x amount-x),
        reserve-y: (+ reserve-y amount-y)
      })
    )

    ;; Track volume and update cache
    (var-set total-volume (+ (var-get total-volume) amount-y))
    (update-swap-cache pool-id amount-y)

    (ok amount-x)
  )
)

;; Emergency pause function (owner only)
(define-public (pause-contract)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set contract-paused true)
    (ok true)
  )
)

;; Unpause contract (owner only)
(define-public (unpause-contract)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set contract-paused false)
    (ok true)
  )
)

;; Update pool fee rate (owner only)
(define-public (update-fee-rate (pool-id uint) (new-fee-rate uint))
  (let
    (
      (pool (unwrap! (map-get? pools { pool-id: pool-id }) err-pool-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-fee-rate u500) err-invalid-amount) ;; Max 5% fee

    (map-set pools
      { pool-id: pool-id }
      (merge pool { fee-rate: new-fee-rate })
    )
    (ok true)
  )
)

;; read only functions
;;

;; Get pool information
(define-read-only (get-pool (pool-id uint))
  (map-get? pools { pool-id: pool-id })
)

;; Get LP token balance
(define-read-only (get-lp-balance (pool-id uint) (owner principal))
  (default-to u0 (get balance (map-get? lp-balances { pool-id: pool-id, owner: owner })))
)

;; Get current pool price (token Y per token X)
(define-read-only (get-pool-price (pool-id uint))
  (match (map-get? pools { pool-id: pool-id })
    pool (ok (/ (get reserve-y pool) (get reserve-x pool)))
    err-pool-not-found
  )
)

;; Calculate swap output (X to Y) without executing
(define-read-only (get-swap-x-to-y-output (pool-id uint) (amount-x uint))
  (match (map-get? pools { pool-id: pool-id })
    pool
      (let
        (
          (reserve-x (get reserve-x pool))
          (reserve-y (get reserve-y pool))
          (fee-rate (get fee-rate pool))
          (amount-x-with-fee (- amount-x (/ (* amount-x fee-rate) fee-denominator)))
          (amount-y (/ (* amount-x-with-fee reserve-y) (+ reserve-x amount-x-with-fee)))
        )
        (ok amount-y)
      )
    err-pool-not-found
  )
)

;; Calculate swap output (Y to X) without executing
(define-read-only (get-swap-y-to-x-output (pool-id uint) (amount-y uint))
  (match (map-get? pools { pool-id: pool-id })
    pool
      (let
        (
          (reserve-x (get reserve-x pool))
          (reserve-y (get reserve-y pool))
          (fee-rate (get fee-rate pool))
          (amount-y-with-fee (- amount-y (/ (* amount-y fee-rate) fee-denominator)))
          (amount-x (/ (* amount-y-with-fee reserve-x) (+ reserve-y amount-y-with-fee)))
        )
        (ok amount-x)
      )
    err-pool-not-found
  )
)

;; Get user position information
(define-read-only (get-user-position (user principal) (pool-id uint))
  (map-get? user-positions { user: user, pool-id: pool-id })
)

;; Get total protocol volume
(define-read-only (get-total-volume)
  (ok (var-get total-volume))
)

;; Get contract status
(define-read-only (is-paused)
  (ok (var-get contract-paused))
)

;; Get total number of pools
(define-read-only (get-pool-count)
  (ok (var-get pool-nonce))
)

;; Get swap statistics for a pool at current block
(define-read-only (get-swap-stats (pool-id uint))
  (map-get? recent-swaps { pool-id: pool-id, block-height: stacks-block-height })
)

;; private functions
;;

;; Update user position tracking (internal helper)
(define-private (update-user-position
    (pool-id uint)
    (user principal)
    (deposit-x uint)
    (deposit-y uint)
    (withdrawal uint))
  (let
    (
      (current-position (default-to
        { total-deposits-x: u0, total-deposits-y: u0, total-withdrawals: u0, last-interaction-block: u0 }
        (map-get? user-positions { user: user, pool-id: pool-id })))
    )
    (map-set user-positions
      { user: user, pool-id: pool-id }
      {
        total-deposits-x: (+ (get total-deposits-x current-position) deposit-x),
        total-deposits-y: (+ (get total-deposits-y current-position) deposit-y),
        total-withdrawals: (+ (get total-withdrawals current-position) withdrawal),
        last-interaction-block: stacks-block-height
      }
    )
  )
)

;; Update swap cache for throughput optimization (internal helper)
(define-private (update-swap-cache (pool-id uint) (volume uint))
  (let
    (
      (current-stats (default-to
        { volume: u0, swap-count: u0 }
        (map-get? recent-swaps { pool-id: pool-id, block-height: stacks-block-height })))
    )
    (map-set recent-swaps
      { pool-id: pool-id, block-height: stacks-block-height }
      {
        volume: (+ (get volume current-stats) volume),
        swap-count: (+ (get swap-count current-stats) u1)
      }
    )
  )
)