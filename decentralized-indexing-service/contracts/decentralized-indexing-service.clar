
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

(define-map NodeDelegationInfo
  { node-address: principal }
  {
    total-delegated: uint,
    delegator-count: uint,
    commission-rate: uint,
    accepting-delegations: bool
  }
)

(define-map RewardPeriods
  { period-id: uint }
  {
    start-block: uint,
    end-block: uint,
    total-rewards: uint,
    distributed: bool
  }
)

(define-map NodeRewards
  {
    node: principal,
    period-id: uint
  }
  {
    amount: uint,
    claimed: bool
  }
)

(define-map Proposals
  { proposal-id: uint }
  {
    proposer: principal,
    description-hash: (string-ascii 64),
    parameter-key: (string-ascii 32),
    proposed-value: uint,
    start-block: uint,
    end-block: uint,
    votes-for: uint,
    votes-against: uint,
    executed: bool
  }
)

(define-map ProposalVotes
  {
    proposal-id: uint,
    voter: principal
  }
  {
    vote-amount: uint,
    for: bool
  }
)

(define-map DataFeeds
  { feed-id: (string-ascii 32) }
  {
    creator: principal,
    feed-type: uint,
    access-fee: uint,
    metadata-hash: (string-ascii 64),
    creation-block: uint,
    expiry-block: uint,
    subscribers: uint
  }
)

(define-map FeedSubscriptions
  {
    subscriber: principal,
    feed-id: (string-ascii 32)
  }
  {
    start-block: uint,
    subscription-period: uint,
    total-paid: uint
  }
)

(define-map Subnets
  { subnet-id: (string-ascii 32) }
  {
    creator: principal,
    creation-block: uint,
    node-count: uint,
    min-stake-requirement: uint,
    specialized: bool,
    topic-hash: (string-ascii 64)
  }
)

(define-map SubnetMembership
  {
    subnet-id: (string-ascii 32),
    node: principal
  }
  {
    join-block: uint,
    stake-committed: uint
  }
)

;; Global state variables for new features
(define-data-var current-proposal-id uint u0)
(define-data-var current-reward-period uint u0)
(define-data-var total-delegated-stake uint u0)
(define-data-var total-rewards-distributed uint u0)

;; Update node delegation settings
(define-public (update-delegation-settings
  (commission-rate uint)
  (accepting-delegations bool)
)
  (let (
    (node tx-sender)
    (node-info (unwrap! (map-get? IndexingNodes { node-address: node }) ERR_INVALID_NODE))
    (delegation-info (default-to 
                      {
                        total-delegated: u0,
                        delegator-count: u0,
                        commission-rate: u50,
                        accepting-delegations: true
                      }
                      (map-get? NodeDelegationInfo { node-address: node })))
  )
    ;; Verify commission rate (max 30%)
    (asserts! (<= commission-rate u300) ERR_UNAUTHORIZED)
    
    ;; Update node delegation info
    (map-set NodeDelegationInfo
      { node-address: node }
      {
        total-delegated: (get total-delegated delegation-info),
        delegator-count: (get delegator-count delegation-info),
        commission-rate: commission-rate,
        accepting-delegations: accepting-delegations
      }
    )
    
    (ok true)
  )
)

;; Start a new reward period
(define-public (start-reward-period
  (total-rewards uint)
)
  (let (
    (current-period (var-get current-reward-period))
    (admin tx-sender)
  )
    ;; Only admin can call this (simplified for demonstration)
    ;; Transfer rewards to contract
    (try! (stx-transfer? total-rewards admin (as-contract tx-sender)))
    
    ;; Create new reward period
    (map-set RewardPeriods
      { period-id: (+ current-period u1) }
      {
        start-block: stacks-block-height,
        end-block: (+ stacks-block-height REWARD_CLAIM_PERIOD),
        total-rewards: total-rewards,
        distributed: false
      }
    )
    
    ;; Update current period
    (var-set current-reward-period (+ current-period u1))
    
    (ok (+ current-period u1))
  )
)

;; Claim rewards for a period
(define-public (claim-rewards
  (period-id uint)
)
  (let (
    (node tx-sender)
    (node-info (unwrap! (map-get? IndexingNodes { node-address: node }) ERR_INVALID_NODE))
    (reward-info (unwrap! (map-get? NodeRewards 
                           { 
                             node: node,
                             period-id: period-id
                           }) 
                         ERR_INVALID_REWARD_PERIOD))
    (period-info (unwrap! (map-get? RewardPeriods { period-id: period-id }) ERR_INVALID_REWARD_PERIOD))
  )
    ;; Verify reward not already claimed
    (asserts! (not (get claimed reward-info)) ERR_REWARD_CLAIM_FAILED)
    
    ;; Verify reward period has ended
    (asserts! (>= stacks-block-height (get end-block period-info)) ERR_INVALID_REWARD_PERIOD)
    
    ;; Transfer reward
    (try! (as-contract (stx-transfer? (get amount reward-info) tx-sender node)))
    
    ;; Mark as claimed
    (map-set NodeRewards
      { 
        node: node,
        period-id: period-id
      }
      (merge reward-info { claimed: true })
    )
    
    ;; Update total rewards distributed
    (var-set total-rewards-distributed (+ (var-get total-rewards-distributed) (get amount reward-info)))
    
    (ok (get amount reward-info))
  )
)

;; Create a new governance proposal
(define-public (create-proposal
  (description-hash (string-ascii 64))
  (parameter-key (string-ascii 32))
  (proposed-value uint)
)
  (let (
    (proposer tx-sender)
    (current-id (var-get current-proposal-id))
    (node-info (unwrap! (map-get? IndexingNodes { node-address: proposer }) ERR_INVALID_NODE))
  )
    ;; Verify proposer has sufficient reputation
    (asserts! (>= (get reputation-score node-info) u5000) ERR_INSUFFICIENT_REPUTATION)
    
    ;; Create proposal
    (map-set Proposals
      { proposal-id: (+ current-id u1) }
      {
        proposer: proposer,
        description-hash: description-hash,
        parameter-key: parameter-key,
        proposed-value: proposed-value,
        start-block: stacks-block-height,
        end-block: (+ stacks-block-height PROPOSAL_VOTING_PERIOD),
        votes-for: u0,
        votes-against: u0,
        executed: false
      }
    )
    
    ;; Update proposal counter
    (var-set current-proposal-id (+ current-id u1))
    
    (ok (+ current-id u1))
  )
)

;; Execute a passed proposal
(define-public (execute-proposal
  (proposal-id uint)
)
  (let (
    (proposal (unwrap! (map-get? Proposals { proposal-id: proposal-id }) ERR_INVALID_QUERY))
  )
    ;; Verify proposal voting period has ended
    (asserts! (> stacks-block-height (get end-block proposal)) ERR_PROPOSAL_NOT_ACTIVE)
    
    ;; Verify proposal hasn't been executed
    (asserts! (not (get executed proposal)) ERR_PROPOSAL_NOT_ACTIVE)
    
    ;; Verify proposal passed
    (asserts! (and
              (> (get votes-for proposal) (get votes-against proposal))
              (>= (get votes-for proposal) MIN_VOTES_FOR_PROPOSAL))
             ERR_INSUFFICIENT_REPUTATION)
    
    ;; Update network parameter
    (map-set NetworkParameters
      { param-key: (get parameter-key proposal) }
      { value: (get proposed-value proposal) }
    )
    
    ;; Mark proposal as executed
    (map-set Proposals
      { proposal-id: proposal-id }
      (merge proposal { executed: true })
    )
    
    (ok true)
  )
)

;; Subscribe to a data feed
(define-public (subscribe-to-feed
  (feed-id (string-ascii 32))
  (subscription-period uint)
)
  (let (
    (subscriber tx-sender)
    (feed (unwrap! (map-get? DataFeeds { feed-id: feed-id }) ERR_INVALID_DATA_FEED))
    (total-fee (* (get access-fee feed) subscription-period))
  )
    ;; Verify feed hasn't expired
    (asserts! (< stacks-block-height (get expiry-block feed)) ERR_INVALID_DATA_FEED)
    
    ;; Pay subscription fee to creator
    (try! (stx-transfer? total-fee subscriber (get creator feed)))
    
    ;; Record subscription
    (map-set FeedSubscriptions
      {
        subscriber: subscriber,
        feed-id: feed-id
      }
      {
        start-block: stacks-block-height,
        subscription-period: subscription-period,
        total-paid: total-fee
      }
    )
    
    ;; Update feed subscriber count
    (map-set DataFeeds
      { feed-id: feed-id }
      (merge feed { subscribers: (+ (get subscribers feed) u1) })
    )
    
    (ok true)
  )
)

;; Create a specialized subnet
(define-public (create-subnet
  (subnet-id (string-ascii 32))
  (min-stake-requirement uint)
  (specialized bool)
  (topic-hash (string-ascii 64))
)
  (let (
    (creator tx-sender)
    (node-info (unwrap! (map-get? IndexingNodes { node-address: creator }) ERR_INVALID_NODE))
  )
    ;; Verify node is active and has enough reputation
    (asserts! (and (get active node-info) 
                  (>= (get reputation-score node-info) u5000))
             ERR_INSUFFICIENT_REPUTATION)
    
    ;; Pay subnet creation fee
    (try! (stx-transfer? SUBNET_CREATION_FEE creator (as-contract tx-sender)))
    
    ;; Create subnet
    (map-set Subnets
      { subnet-id: subnet-id }
      {
        creator: creator,
        creation-block: stacks-block-height,
        node-count: u1, ;; Creator is first member
        min-stake-requirement: min-stake-requirement,
        specialized: specialized,
        topic-hash: topic-hash
      }
    )
    
    ;; Add creator to subnet
    (map-set SubnetMembership
      {
        subnet-id: subnet-id,
        node: creator
      }
      {
        join-block: stacks-block-height,
        stake-committed: (get total-stake node-info)
      }
    )
    
    (ok true)
  )
)
