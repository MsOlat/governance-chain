;; governance_chain_contract
;; A DAO governance contract that allows token holders to create and vote on proposals

;; constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-proposal-exists (err u103))
(define-constant err-proposal-expired (err u104))
(define-constant err-already-voted (err u105))
(define-constant err-insufficient-tokens (err u106))
(define-constant err-proposal-active (err u107))
(define-constant err-proposal-not-active (err u108))
(define-constant err-invalid-vote (err u109))

;; Proposal status values
(define-constant status-active u1)
(define-constant status-approved u2)
(define-constant status-rejected u3)
(define-constant status-executed u4)

;; Voting options
(define-constant vote-for u1)
(define-constant vote-against u2)
(define-constant vote-abstain u3)

;; Governance parameters
(define-constant min-proposal-duration u144) ;; ~1 day in blocks
(define-constant quorum-percentage u30) ;; 30% of total tokens must vote
(define-constant approval-threshold u51) ;; 51% of votes must be "for"

;; data maps and vars
(define-data-var total-proposals uint u0)
(define-data-var governance-token principal 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.token-contract)
(define-data-var total-token-supply uint u0)

;; Proposal data structure
(define-map proposals
  uint
  {
    title: (string-utf8 100),
    description: (string-utf8 500),
    proposer: principal,
    start-block-height: uint,
    end-block-height: uint,
    status: uint,
    for-votes: uint,
    against-votes: uint,
    abstain-votes: uint,
    action-contract: principal,
    action-function: (string-ascii 128),
    action-args: (list 10 (string-utf8 100))
  }
)

;; Track votes by proposal ID and voter
(define-map votes
  {proposal-id: uint, voter: principal}
  {vote-type: uint, amount: uint}
)

;; Track token balances at proposal creation (for voting power)
(define-map voting-power-snapshots
  {proposal-id: uint, voter: principal}
  {amount: uint}
)

;; private functions
(define-private (create-proposal-internal
                  (title (string-utf8 100))
                  (description (string-utf8 500))
                  (duration uint)
                  (action-contract principal)
                  (action-function (string-ascii 128))
                  (action-args (list 10 (string-utf8 100))))
  (let ((proposal-id (var-get total-proposals))
        (start-block (unwrap-panic (get-block-info? time u0)))
        (end-block (+ start-block duration)))
    
    (map-set proposals proposal-id
      {
        title: title,
        description: description,
        proposer: tx-sender,
        start-block-height: start-block,
        end-block-height: end-block,
        status: status-active,
        for-votes: u0,
        against-votes: u0,
        abstain-votes: u0,
        action-contract: action-contract,
        action-function: action-function,
        action-args: action-args
      })
    
    (var-set total-proposals (+ proposal-id u1))
    (ok proposal-id)))

(define-private (is-proposal-active (proposal-id uint))
  (let ((proposal (unwrap! (map-get? proposals proposal-id) false))
        (current-height (unwrap-panic (get-block-info? time u0))))
    (and
      (is-eq (get status proposal) status-active)
      (<= (get start-block-height proposal) current-height)
      (>= (get end-block-height proposal) current-height))))

(define-private (get-voting-power (voter principal))
  ;; In a real implementation, this would query the token contract
  ;; For simplicity, we'll return a fixed value
  u100)

;; public functions
(define-public (create-proposal
                (title (string-utf8 100))
                (description (string-utf8 500))
                (duration uint)
                (action-contract principal)
                (action-function (string-ascii 128))
                (action-args (list 10 (string-utf8 100))))
  (let ((voting-power (get-voting-power tx-sender)))
    (asserts! (>= voting-power u10) err-insufficient-tokens)
    (asserts! (>= duration min-proposal-duration) err-unauthorized)
    (create-proposal-internal title description duration action-contract action-function action-args)))
