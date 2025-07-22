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

;; New error codes
(define-constant err-cannot-cancel (err u110))
(define-constant err-cannot-update (err u111))
(define-constant err-proposal-started (err u112))


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

;; Event log for proposal lifecycle
(define-map proposal-events
  {proposal-id: uint, event-type: (string-ascii 32)}
  {block: uint, sender: principal, info: (string-utf8 200)}
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
    (map-set proposal-events {proposal-id: proposal-id, event-type: "created"} {block: start-block, sender: tx-sender, info: title})
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

(define-public (vote (proposal-id uint) (vote-type uint) (amount uint))
  (let ((proposal (unwrap! (map-get? proposals proposal-id) err-not-found))
        (voting-power (get-voting-power tx-sender)))
    
    ;; Check if proposal is active
    (asserts! (is-proposal-active proposal-id) err-proposal-not-active)
    
    ;; Check if user has enough voting power
    (asserts! (>= voting-power amount) err-insufficient-tokens)
    
    ;; Check if user has already voted
    (asserts! (is-none (map-get? votes {proposal-id: proposal-id, voter: tx-sender})) err-already-voted)
    
    ;; Check if vote type is valid
    (asserts! (or (is-eq vote-type vote-for) (is-eq vote-type vote-against) (is-eq vote-type vote-abstain)) err-invalid-vote)
    
    ;; Record the vote
    (map-set votes {proposal-id: proposal-id, voter: tx-sender} {vote-type: vote-type, amount: amount})
    
    ;; Update vote tallies based on vote type
    (if (is-eq vote-type vote-for)
      (map-set proposals proposal-id (merge proposal {for-votes: (+ (get for-votes proposal) amount)}))
      (if (is-eq vote-type vote-against)
        (map-set proposals proposal-id (merge proposal {against-votes: (+ (get against-votes proposal) amount)}))
        (map-set proposals proposal-id (merge proposal {abstain-votes: (+ (get abstain-votes proposal) amount)}))))
    
    (map-set proposal-events {proposal-id: proposal-id, event-type: "voted"} {block: (unwrap-panic (get-block-info? time u0)), sender: tx-sender, info: u"voted"})
    (ok true)))

(define-public (finalize-proposal (proposal-id uint))
  (let ((proposal (unwrap! (map-get? proposals proposal-id) err-not-found))
        (current-height (unwrap-panic (get-block-info? time u0)))
        (total-votes (+ (get for-votes proposal) (get against-votes proposal) (get abstain-votes proposal))))
    
    ;; Check if proposal has ended
    (asserts! (> current-height (get end-block-height proposal)) err-proposal-active)
    
    ;; Check if proposal is still active (not already finalized)
    (asserts! (is-eq (get status proposal) status-active) err-unauthorized)
    
    ;; Calculate if quorum was reached
    (if (>= (* total-votes u100) (* (var-get total-token-supply) quorum-percentage))
      ;; Quorum reached, check if proposal passed
      (if (>= (* (get for-votes proposal) u100) (* total-votes approval-threshold))
        ;; Proposal approved
        (map-set proposals proposal-id (merge proposal {status: status-approved}))
        ;; Proposal rejected
        (map-set proposals proposal-id (merge proposal {status: status-rejected})))
      ;; Quorum not reached, proposal rejected
      (map-set proposals proposal-id (merge proposal {status: status-rejected})))
    
    (map-set proposal-events {proposal-id: proposal-id, event-type: "finalized"} {block: current-height, sender: tx-sender, info: u""})
    (ok true)))

(define-public (execute-proposal (proposal-id uint))
  (let ((proposal (unwrap! (map-get? proposals proposal-id) err-not-found)))
    
    ;; Check if proposal was approved
    (asserts! (is-eq (get status proposal) status-approved) err-unauthorized)
    
    ;; Mark proposal as executed
    (map-set proposals proposal-id (merge proposal {status: status-executed}))
    
    ;; In a real implementation, this would execute the proposal's action
    ;; For simplicity, we'll just return success
    (map-set proposal-events {proposal-id: proposal-id, event-type: "executed"} {block: (unwrap-panic (get-block-info? time u0)), sender: tx-sender, info: u""})
    (ok true)))

;; New: Cancel a proposal (by proposer or owner, only if voting hasn't started)
(define-public (cancel-proposal (proposal-id uint))
  (let ((proposal (unwrap! (map-get? proposals proposal-id) err-not-found))
        (current-height (unwrap-panic (get-block-info? time u0))))
    (asserts! (or (is-eq tx-sender (get proposer proposal)) (is-eq tx-sender contract-owner)) err-cannot-cancel)
    (asserts! (> (get start-block-height proposal) current-height) err-proposal-started)
    (map-set proposals proposal-id (merge proposal {status: status-rejected}))
    (map-set proposal-events {proposal-id: proposal-id, event-type: "cancelled"} {block: current-height, sender: tx-sender, info: u""})
    (ok true)))

;; New: Update proposal details (by proposer or owner, only before voting starts)
(define-public (update-proposal (proposal-id uint)
    (title (string-utf8 100))
    (description (string-utf8 500))
    (action-contract principal)
    (action-function (string-ascii 128))
    (action-args (list 10 (string-utf8 100))))
  (let ((proposal (unwrap! (map-get? proposals proposal-id) err-not-found))
        (current-height (unwrap-panic (get-block-info? time u0))))
    (asserts! (or (is-eq tx-sender (get proposer proposal)) (is-eq tx-sender contract-owner)) err-cannot-update)
    (asserts! (> (get start-block-height proposal) current-height) err-proposal-started)
    (map-set proposals proposal-id (merge proposal {
      title: title,
      description: description,
      action-contract: action-contract,
      action-function: action-function,
      action-args: action-args
    }))
    (map-set proposal-events {proposal-id: proposal-id, event-type: "updated"} {block: current-height, sender: tx-sender, info: title})
    (ok true)))

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals proposal-id))

(define-read-only (get-vote (proposal-id uint) (voter principal))
  (map-get? votes {proposal-id: proposal-id, voter: voter}))

(define-read-only (get-proposal-count)
  (var-get total-proposals))

;; New: Get proposal event log
(define-read-only (get-proposal-event (proposal-id uint) (event-type (string-ascii 32)))
  (map-get? proposal-events {proposal-id: proposal-id, event-type: event-type}))

(define-public (set-governance-token (token-contract principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set governance-token token-contract)
    (ok true)))

(define-public (set-total-token-supply (new-supply uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set total-token-supply new-supply)
    (ok true)))
