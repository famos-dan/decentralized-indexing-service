
;; title: decentralized-indexing-service
;; Error Codes
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_NODE (err u101))
(define-constant ERR_NODE_EXISTS (err u102))
(define-constant ERR_INSUFFICIENT_STAKE (err u103))
(define-constant ERR_INVALID_STAKE_AMOUNT (err u104))
(define-constant ERR_WITHDRAWAL_FAILED (err u105))
(define-constant ERR_CHALLENGE_EXISTS (err u106))
(define-constant ERR_INVALID_CHALLENGE (err u107))
(define-constant ERR_INVALID_QUERY (err u108))
(define-constant ERR_INSUFFICIENT_REPUTATION (err u109))
(define-constant ERR_DATA_VERIFICATION_FAILED (err u110))

;; Constants
(define-constant MIN_STAKE_AMOUNT u5000)
(define-constant MAX_REPUTATION_SCORE u10000)
(define-constant REPUTATION_DECAY_RATE u10)
(define-constant CHALLENGE_PERIOD u144) ;; Approximately 1 day
(define-constant QUERY_FEE_BASE u100)

;; Node Types
(define-constant NODE_TYPE_FULL u0)
(define-constant NODE_TYPE_LIGHT u1)
(define-constant NODE_TYPE_ARCHIVE u2)

;; Structs for advanced tracking
(define-map IndexingNodes 
  { node-address: principal }
  {
    total-stake: uint,
    reputation-score: uint,
    node-type: uint,
    active: bool,
    last-validation-timestamp: uint,
    total-queries-served: uint,
    successful-queries: uint,
    failed-queries: uint,
    total-data-indexed: uint
  }
)

;; Challenges and Dispute Resolution
(define-map NodeChallenges
  { 
    challenger: principal, 
    challenged-node: principal,
    challenge-block: uint
  }
  {
    challenge-stake: uint,
    resolved: bool,
    challenge-type: uint,
    evidence-hash: (string-ascii 64)
  }
)

;; Query Tracking
(define-map QueryTracking
  { 
    query-id: (string-ascii 64),
    node: principal 
  }
  {
    timestamp: uint,
    query-type: uint,
    data-hash: (string-ascii 64),
    verified: bool
  }
)

;; Governance Parameters
(define-map NetworkParameters
  { param-key: (string-ascii 32) }
  { value: uint }
)

;; Global State Variables
(define-data-var total-nodes uint u0)
(define-data-var total-staked-amount uint u0)
(define-data-var total-queries-processed uint u0)
(define-data-var total-data-indexed uint u0)

;; Advanced Data Commitment
(define-public (submit-data-commitment 
  (node principal)
  (data-hash (string-ascii 64))
)
  (let (
    (node-info (unwrap! (map-get? IndexingNodes { node-address: node }) ERR_INVALID_NODE))
  )
    ;; Verify node is active
    (asserts! (get active node-info) ERR_UNAUTHORIZED)
    
    ;; Update total indexed data
    (map-set IndexingNodes 
      { node-address: node }
      (merge node-info {
        total-data-indexed: (+ (get total-data-indexed node-info) u1)
      })
    )
    
    (ok true)
)
)

;; Challenge Mechanism
(define-public (challenge-node 
  (challenged-node principal)
  (challenge-type uint)
  (evidence-hash (string-ascii 64))
)
  (let (
    (challenger tx-sender)
    (challenge-stake (/ (stx-get-balance challenger) u10)) ;; 10% of balance
    (node-info (unwrap! (map-get? IndexingNodes { node-address: challenged-node }) ERR_INVALID_NODE))
  )
    ;; Prevent duplicate challenges
    (asserts! (is-none (map-get? NodeChallenges 
      { 
        challenger: challenger, 
        challenged-node: challenged-node,
        challenge-block: stacks-block-height 
      }
    )) ERR_CHALLENGE_EXISTS)
    
    ;; Transfer challenge stake
    (try! (stx-transfer? challenge-stake challenger (as-contract tx-sender)))
    
    ;; Record challenge
    (map-set NodeChallenges
      { 
        challenger: challenger, 
        challenged-node: challenged-node,
        challenge-block: stacks-block-height 
      }
      {
        challenge-stake: challenge-stake,
        resolved: false,
        challenge-type: challenge-type,
        evidence-hash: evidence-hash
      }
    )
    
    (ok true)
)
)

;; Resolve Challenge
(define-public (resolve-challenge 
  (challenger principal)
  (challenged-node principal)
  (challenge-block uint)
  (is-valid bool)
)
  (let (
    (challenge-info (unwrap! 
      (map-get? NodeChallenges 
        { 
          challenger: challenger, 
          challenged-node: challenged-node,
          challenge-block: challenge-block 
        }
      ) 
      ERR_INVALID_CHALLENGE
    ))
    (node-info (unwrap! (map-get? IndexingNodes { node-address: challenged-node }) ERR_INVALID_NODE))
    (resolver tx-sender)
  )
    ;; Ensure challenge is not already resolved
    (asserts! (not (get resolved challenge-info)) ERR_INVALID_CHALLENGE)
    
    ;; Ensure challenge is within resolution period
    (asserts! (<= (- stacks-block-height challenge-block) CHALLENGE_PERIOD) ERR_INVALID_CHALLENGE)
    
    ;; Resolve challenge
    (if is-valid
      ;; Challenge proven - slash node
      (begin
        (map-set IndexingNodes 
          { node-address: challenged-node }
          (merge node-info {
            reputation-score: (/ (get reputation-score node-info) u2), ;; Halve reputation
            active: (if (< (get reputation-score node-info) u1000) false true)
          })
        )
        ;; Reward challenger
        (try! (as-contract (stx-transfer? (get challenge-stake challenge-info) tx-sender challenger)))
      )
      ;; Challenge invalid - punish challenger
      (begin
        ;; Slash challenger's stake
        (try! (as-contract (stx-transfer? (get challenge-stake challenge-info) tx-sender challenged-node)))
      )
    )
    
    ;; Mark challenge as resolved
    (map-set NodeChallenges
      { 
        challenger: challenger, 
        challenged-node: challenged-node,
        challenge-block: challenge-block 
      }
      (merge challenge-info { resolved: true })
    )
    
    (ok true)
)
)

;; Read-only Functions
(define-read-only (get-node-info (node principal))
  (map-get? IndexingNodes { node-address: node })
)

(define-read-only (get-network-stats)
  {
    total-nodes: (var-get total-nodes),
    total-staked-amount: (var-get total-staked-amount),
    total-queries-processed: (var-get total-queries-processed),
    total-data-indexed: (var-get total-data-indexed)
  }
)

(define-read-only (get-network-parameter (param-key (string-ascii 32)))
  (map-get? NetworkParameters { param-key: param-key })
)

;; Error Codes for new features
(define-constant ERR_DELEGATION_LIMIT_REACHED (err u111))
(define-constant ERR_INVALID_DELEGATION (err u112))
(define-constant ERR_REWARD_CLAIM_FAILED (err u113))
(define-constant ERR_INVALID_REWARD_PERIOD (err u114))
(define-constant ERR_ALREADY_VOTED (err u115))
(define-constant ERR_PROPOSAL_EXPIRED (err u116))
(define-constant ERR_PROPOSAL_NOT_ACTIVE (err u117))
(define-constant ERR_INVALID_DATA_FEED (err u118))
(define-constant ERR_FEED_EXISTS (err u119))
(define-constant ERR_INSUFFICIENT_PERMISSIONS (err u120))

;; Constants for new features
(define-constant MAX_DELEGATIONS_PER_NODE u10)
(define-constant REWARD_CLAIM_PERIOD u720) ;; Approximately 5 days
(define-constant PROPOSAL_VOTING_PERIOD u4320) ;; Approximately 30 days
(define-constant MIN_VOTES_FOR_PROPOSAL u100)
(define-constant DATA_FEED_EXPIRY_PERIOD u1440) ;; 10 days
(define-constant SUBNET_CREATION_FEE u50000)


(define-map StakeDelegations
  {
    delegator: principal,
    node: principal
  }
  {
    amount: uint,
    start-block: uint,
    last-reward-block: uint,
    commission-rate: uint
  }
)