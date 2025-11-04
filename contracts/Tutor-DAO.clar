(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_ALREADY_EXISTS (err u101))
(define-constant ERR_NOT_FOUND (err u102))
(define-constant ERR_INVALID_AMOUNT (err u103))
(define-constant ERR_INSUFFICIENT_BALANCE (err u104))
(define-constant ERR_ALREADY_VOTED (err u105))
(define-constant ERR_PROPOSAL_ENDED (err u106))
(define-constant ERR_PROPOSAL_ACTIVE (err u107))
(define-constant ERR_ALREADY_REVIEWED (err u108))
(define-constant ERR_ACHIEVEMENT_NOT_FOUND (err u109))
(define-constant ERR_ACHIEVEMENT_ALREADY_EARNED (err u110))

(define-constant ERR_DISPUTE_WINDOW_EXPIRED (err u111))
(define-constant ERR_DISPUTE_ALREADY_EXISTS (err u112))
(define-constant ERR_DISPUTE_NOT_FOUND (err u113))
(define-constant ERR_DISPUTE_ALREADY_RESOLVED (err u114))

(define-constant ERR_SESSION_NOT_FOUND (err u115))
(define-constant ERR_SESSION_ALREADY_COMPLETED (err u116))
(define-constant ERR_SESSION_ALREADY_CANCELLED (err u117))
(define-constant ERR_INSUFFICIENT_ESCROW (err u118))
(define-constant ERR_SESSION_NOT_READY (err u119))

(define-constant ERR_ALREADY_SLASHED (err u120))
(define-constant ERR_NOT_RESOLVED (err u121))
(define-constant SLASH_AMOUNT u1000000)

(define-data-var next-session-id uint u1)

(define-data-var next-tutor-id uint u1)
(define-data-var next-proposal-id uint u1)
(define-data-var next-review-id uint u1)
(define-data-var dao-treasury uint u0)
(define-data-var next-achievement-id uint u1)

(define-map tutors
  { tutor-id: uint }
  {
    address: principal,
    name: (string-ascii 50),
    subject: (string-ascii 30),
    total-rating: uint,
    review-count: uint,
    total-rewards: uint,
    active: bool
  }
)

(define-map tutor-addresses
  { address: principal }
  { tutor-id: uint }
)

(define-map reviews
  { review-id: uint }
  {
    tutor-id: uint,
    student: principal,
    rating: uint,
    comment: (string-ascii 200),
    stacks-block-height: uint
  }
)

(define-map student-tutor-reviews
  { student: principal, tutor-id: uint }
  { review-id: uint }
)

(define-map proposals
  { proposal-id: uint }
  {
    tutor-id: uint,
    reward-amount: uint,
    description: (string-ascii 200),
    votes-for: uint,
    votes-against: uint,
    start-block: uint,
    end-block: uint,
    executed: bool,
    proposer: principal
  }
)

(define-map votes
  { proposal-id: uint, voter: principal }
  { vote: bool, amount: uint }
)

(define-map dao-members
  { member: principal }
  { voting-power: uint, joined-block: uint }
)

(define-public (register-tutor (name (string-ascii 50)) (subject (string-ascii 30)))
  (let
    (
      (tutor-id (var-get next-tutor-id))
      (caller tx-sender)
    )
    (asserts! (is-none (map-get? tutor-addresses { address: caller })) ERR_ALREADY_EXISTS)
    (map-set tutors
      { tutor-id: tutor-id }
      {
        address: caller,
        name: name,
        subject: subject,
        total-rating: u0,
        review-count: u0,
        total-rewards: u0,
        active: true
      }
    )
    (map-set tutor-addresses { address: caller } { tutor-id: tutor-id })
    (var-set next-tutor-id (+ tutor-id u1))
    (ok tutor-id)
  )
)

(define-public (join-dao)
  (let
    (
      (caller tx-sender)
      (current-member (map-get? dao-members { member: caller }))
    )
    (asserts! (is-none current-member) ERR_ALREADY_EXISTS)
    (map-set dao-members
      { member: caller }
      { voting-power: u1, joined-block: stacks-block-height }
    )
    (ok true)
  )
)

(define-public (submit-review (tutor-id uint) (rating uint) (comment (string-ascii 200)))
  (let
    (
      (review-id (var-get next-review-id))
      (caller tx-sender)
      (tutor-data (unwrap! (map-get? tutors { tutor-id: tutor-id }) ERR_NOT_FOUND))
      (existing-review (map-get? student-tutor-reviews { student: caller, tutor-id: tutor-id }))
    )
    (asserts! (and (>= rating u1) (<= rating u5)) ERR_INVALID_AMOUNT)
    (asserts! (is-none existing-review) ERR_ALREADY_REVIEWED)
    (asserts! (get active tutor-data) ERR_NOT_FOUND)
    
    (map-set reviews
      { review-id: review-id }
      {
        tutor-id: tutor-id,
        student: caller,
        rating: rating,
        comment: comment,
        stacks-block-height: stacks-block-height
      }
    )
    
    (map-set student-tutor-reviews
      { student: caller, tutor-id: tutor-id }
      { review-id: review-id }
    )
    
    (map-set tutors
      { tutor-id: tutor-id }
      (merge tutor-data {
        total-rating: (+ (get total-rating tutor-data) rating),
        review-count: (+ (get review-count tutor-data) u1)
      })
    )
    
    (var-set next-review-id (+ review-id u1))
    (try! (check-and-award-achievements tutor-id))
    (ok review-id)
  )
)

(define-public (create-reward-proposal (tutor-id uint) (reward-amount uint) (description (string-ascii 200)))
  (let
    (
      (proposal-id (var-get next-proposal-id))
      (caller tx-sender)
      (member-data (unwrap! (map-get? dao-members { member: caller }) ERR_NOT_AUTHORIZED))
      (tutor-data (unwrap! (map-get? tutors { tutor-id: tutor-id }) ERR_NOT_FOUND))
    )
    (asserts! (> reward-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (get active tutor-data) ERR_NOT_FOUND)
    
    (map-set proposals
      { proposal-id: proposal-id }
      {
        tutor-id: tutor-id,
        reward-amount: reward-amount,
        description: description,
        votes-for: u0,
        votes-against: u0,
        start-block: stacks-block-height,
        end-block: (+ stacks-block-height u144),
        executed: false,
        proposer: caller
      }
    )
    
    (var-set next-proposal-id (+ proposal-id u1))
    (ok proposal-id)
  )
)

(define-public (vote-on-proposal (proposal-id uint) (vote-for bool))
  (let
    (
      (caller tx-sender)
      (member-data (unwrap! (map-get? dao-members { member: caller }) ERR_NOT_AUTHORIZED))
      (proposal-data (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR_NOT_FOUND))
      (existing-vote (map-get? votes { proposal-id: proposal-id, voter: caller }))
      (voting-power (get voting-power member-data))
    )
    (asserts! (is-none existing-vote) ERR_ALREADY_VOTED)
    (asserts! (<= stacks-block-height (get end-block proposal-data)) ERR_PROPOSAL_ENDED)
    
    (map-set votes
      { proposal-id: proposal-id, voter: caller }
      { vote: vote-for, amount: voting-power }
    )
    
    (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal-data
        {
          votes-for: (if vote-for (+ (get votes-for proposal-data) voting-power) (get votes-for proposal-data)),
          votes-against: (if vote-for (get votes-against proposal-data) (+ (get votes-against proposal-data) voting-power))
        }
      )
    )
    
    (ok true)
  )
)
(define-public (execute-proposal (proposal-id uint))
  (let
    (
      (proposal-data (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR_NOT_FOUND))
      (tutor-data (unwrap! (map-get? tutors { tutor-id: (get tutor-id proposal-data) }) ERR_NOT_FOUND))
      (reward-amount (get reward-amount proposal-data))
    )
    (asserts! (> stacks-block-height (get end-block proposal-data)) ERR_PROPOSAL_ACTIVE)
    (asserts! (not (get executed proposal-data)) ERR_ALREADY_EXISTS)
    (asserts! (> (get votes-for proposal-data) (get votes-against proposal-data)) ERR_NOT_AUTHORIZED)
    (asserts! (>= (var-get dao-treasury) reward-amount) ERR_INSUFFICIENT_BALANCE)
    
    (try! (stx-transfer? reward-amount (as-contract tx-sender) (get address tutor-data)))
    
    (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal-data { executed: true })
    )
    
    (map-set tutors
      { tutor-id: (get tutor-id proposal-data) }
      (merge tutor-data {
        total-rewards: (+ (get total-rewards tutor-data) reward-amount)
      })
    )
    
    (var-set dao-treasury (- (var-get dao-treasury) reward-amount))
    (try! (check-and-award-achievements (get tutor-id proposal-data)))
    (ok true)
  )
)

(define-public (fund-dao (amount uint))
  (begin
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (var-set dao-treasury (+ (var-get dao-treasury) amount))
    (ok true)
  )
)

(define-public (increase-voting-power (member principal) (additional-power uint))
  (let
    (
      (member-data (unwrap! (map-get? dao-members { member: member }) ERR_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (map-set dao-members
      { member: member }
      (merge member-data {
        voting-power: (+ (get voting-power member-data) additional-power)
      })
    )
    (ok true)
  )
)

(define-read-only (get-tutor (tutor-id uint))
  (map-get? tutors { tutor-id: tutor-id })
)

(define-read-only (get-tutor-by-address (address principal))
  (match (map-get? tutor-addresses { address: address })
    tutor-ref (map-get? tutors { tutor-id: (get tutor-id tutor-ref) })
    none
  )
)

(define-read-only (get-review (review-id uint))
  (map-get? reviews { review-id: review-id })
)

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals { proposal-id: proposal-id })
)

(define-read-only (get-dao-member (member principal))
  (map-get? dao-members { member: member })
)

(define-read-only (get-vote (proposal-id uint) (voter principal))
  (map-get? votes { proposal-id: proposal-id, voter: voter })
)

(define-read-only (get-dao-treasury)
  (var-get dao-treasury)
)

(define-read-only (get-tutor-average-rating (tutor-id uint))
  (match (map-get? tutors { tutor-id: tutor-id })
    tutor-data
      (if (> (get review-count tutor-data) u0)
        (some (/ (get total-rating tutor-data) (get review-count tutor-data)))
        (some u0)
      )
    none
  )
)

(define-read-only (has-student-reviewed-tutor (student principal) (tutor-id uint))
  (is-some (map-get? student-tutor-reviews { student: student, tutor-id: tutor-id }))
)





(define-map achievements
  { achievement-id: uint }
  {
    name: (string-ascii 50),
    description: (string-ascii 100),
    requirement-type: (string-ascii 20),
    requirement-value: uint,
    badge-symbol: (string-ascii 10)
  }
)

(define-map tutor-achievements
  { tutor-id: uint, achievement-id: uint }
  { earned-block: uint, earned-date: uint }
)

(define-map tutor-achievement-count
  { tutor-id: uint }
  { total-achievements: uint }
)

(define-private (initialize-achievements)
  (begin
    (map-set achievements { achievement-id: u1 } 
      { name: "First Review", description: "Received your first student review", 
        requirement-type: "review-count", requirement-value: u1, badge-symbol: "*" })
    (map-set achievements { achievement-id: u2 } 
      { name: "Five Star Teacher", description: "Maintained 5-star average with 10+ reviews", 
        requirement-type: "perfect-rating", requirement-value: u10, badge-symbol: "**" })
    (map-set achievements { achievement-id: u3 } 
      { name: "Veteran Educator", description: "Received 50+ student reviews", 
        requirement-type: "review-count", requirement-value: u50, badge-symbol: "^" })
    (map-set achievements { achievement-id: u4 } 
      { name: "Top Performer", description: "Earned 1000+ STX in rewards", 
        requirement-type: "total-rewards", requirement-value: u1000000000, badge-symbol: "@" })
    (var-set next-achievement-id u5)
    (ok true)
  )
)

(define-private (check-and-award-achievements (tutor-id uint))
  (let
    (
      (tutor-data (unwrap! (map-get? tutors { tutor-id: tutor-id }) ERR_NOT_FOUND))
      (review-count (get review-count tutor-data))
      (total-rating (get total-rating tutor-data))
      (total-rewards (get total-rewards tutor-data))
      (average-rating (if (> review-count u0) (/ total-rating review-count) u0))
    )
    (begin
      (if (and (>= review-count u1) 
               (is-none (map-get? tutor-achievements { tutor-id: tutor-id, achievement-id: u1 })))
          (unwrap-panic (award-achievement tutor-id u1))
          true)
      (if (and (>= review-count u10) (is-eq average-rating u5)
               (is-none (map-get? tutor-achievements { tutor-id: tutor-id, achievement-id: u2 })))
          (unwrap-panic (award-achievement tutor-id u2))
          true)
      (if (and (>= review-count u50)
               (is-none (map-get? tutor-achievements { tutor-id: tutor-id, achievement-id: u3 })))
          (unwrap-panic (award-achievement tutor-id u3))
          true)
      (if (and (>= total-rewards u1000000000)
               (is-none (map-get? tutor-achievements { tutor-id: tutor-id, achievement-id: u4 })))
          (unwrap-panic (award-achievement tutor-id u4))
          true)
      (ok true)
    )
  )
)

(define-private (award-achievement (tutor-id uint) (achievement-id uint))
  (let
    (
      (current-count (default-to u0 (get total-achievements 
        (map-get? tutor-achievement-count { tutor-id: tutor-id }))))
    )
    (map-set tutor-achievements
      { tutor-id: tutor-id, achievement-id: achievement-id }
      { earned-block: stacks-block-height, earned-date: stacks-block-height }
    )
    (map-set tutor-achievement-count
      { tutor-id: tutor-id }
      { total-achievements: (+ current-count u1) }
    )
    (ok true)
  )
)

(define-read-only (get-tutor-achievements (tutor-id uint))
  (default-to u0 (get total-achievements 
    (map-get? tutor-achievement-count { tutor-id: tutor-id })))
)

(define-read-only (get-achievement-details (achievement-id uint))
  (map-get? achievements { achievement-id: achievement-id })
)

(define-read-only (has-tutor-earned-achievement (tutor-id uint) (achievement-id uint))
  (is-some (map-get? tutor-achievements { tutor-id: tutor-id, achievement-id: achievement-id }))
)


(define-data-var next-dispute-id uint u1)

(define-map disputes
  { dispute-id: uint }
  {
    review-id: uint,
    student: principal,
    tutor-id: uint,
    reason: (string-ascii 200),
    evidence: (string-ascii 300),
    filed-block: uint,
    resolved: bool,
    resolution: (string-ascii 100),
    arbitrator-votes-for: uint,
    arbitrator-votes-against: uint,
    resolution-block: (optional uint)
  }
)

(define-map dispute-votes
  { dispute-id: uint, arbitrator: principal }
  { supports-student: bool }
)

(define-public (file-dispute (review-id uint) (reason (string-ascii 200)) (evidence (string-ascii 300)))
  (let
    (
      (dispute-id (var-get next-dispute-id))
      (caller tx-sender)
      (review-data (unwrap! (map-get? reviews { review-id: review-id }) ERR_NOT_FOUND))
      (dispute-window u144)
    )
    (asserts! (is-eq caller (get student review-data)) ERR_NOT_AUTHORIZED)
    (asserts! (<= (- stacks-block-height (get stacks-block-height review-data)) dispute-window) ERR_DISPUTE_WINDOW_EXPIRED)
    (asserts! (is-none (map-get? disputes { dispute-id: dispute-id })) ERR_DISPUTE_ALREADY_EXISTS)
    
    (map-set disputes
      { dispute-id: dispute-id }
      {
        review-id: review-id,
        student: caller,
        tutor-id: (get tutor-id review-data),
        reason: reason,
        evidence: evidence,
        filed-block: stacks-block-height,
        resolved: false,
        resolution: "",
        arbitrator-votes-for: u0,
        arbitrator-votes-against: u0,
        resolution-block: none
      }
    )
    
    (var-set next-dispute-id (+ dispute-id u1))
    (ok dispute-id)
  )
)

(define-public (arbitrate-dispute (dispute-id uint) (supports-student bool))
  (let
    (
      (caller tx-sender)
      (member-data (unwrap! (map-get? dao-members { member: caller }) ERR_NOT_AUTHORIZED))
      (dispute-data (unwrap! (map-get? disputes { dispute-id: dispute-id }) ERR_DISPUTE_NOT_FOUND))
      (existing-vote (map-get? dispute-votes { dispute-id: dispute-id, arbitrator: caller }))
    )
    (asserts! (not (get resolved dispute-data)) ERR_DISPUTE_ALREADY_RESOLVED)
    (asserts! (is-none existing-vote) ERR_ALREADY_VOTED)
    
    (map-set dispute-votes
      { dispute-id: dispute-id, arbitrator: caller }
      { supports-student: supports-student }
    )
    
    (map-set disputes
      { dispute-id: dispute-id }
      (merge dispute-data
        {
          arbitrator-votes-for: (if supports-student (+ (get arbitrator-votes-for dispute-data) u1) (get arbitrator-votes-for dispute-data)),
          arbitrator-votes-against: (if supports-student (get arbitrator-votes-against dispute-data) (+ (get arbitrator-votes-against dispute-data) u1))
        }
      )
    )
    
    (ok true)
  )
)

(define-public (resolve-dispute (dispute-id uint))
  (let
    (
      (dispute-data (unwrap! (map-get? disputes { dispute-id: dispute-id }) ERR_DISPUTE_NOT_FOUND))
      (votes-for (get arbitrator-votes-for dispute-data))
      (votes-against (get arbitrator-votes-against dispute-data))
      (resolution-text (if (> votes-for votes-against) "Student dispute upheld" "Tutor maintained good standing"))
    )
    (asserts! (not (get resolved dispute-data)) ERR_DISPUTE_ALREADY_RESOLVED)
    (asserts! (>= (+ votes-for votes-against) u3) ERR_INSUFFICIENT_BALANCE)
    
    (map-set disputes
      { dispute-id: dispute-id }
      (merge dispute-data
        {
          resolved: true,
          resolution: resolution-text,
          resolution-block: (some stacks-block-height)
        }
      )
    )
    
    (ok (> votes-for votes-against))
  )
)

(define-read-only (get-dispute (dispute-id uint))
  (map-get? disputes { dispute-id: dispute-id })
)

(define-read-only (get-dispute-vote (dispute-id uint) (arbitrator principal))
  (map-get? dispute-votes { dispute-id: dispute-id, arbitrator: arbitrator })
)


(define-map learning-sessions
  { session-id: uint }
  {
    student: principal,
    tutor-id: uint,
    amount: uint,
    description: (string-ascii 100),
    created-block: uint,
    completion-deadline: uint,
    student-confirmed: bool,
    tutor-confirmed: bool,
    funds-released: bool,
    cancelled: bool
  }
)

(define-map student-session-count
  { student: principal }
  { total-sessions: uint, active-sessions: uint }
)

(define-public (create-learning-session (tutor-id uint) (amount uint) (description (string-ascii 100)) (session-duration-blocks uint))
  (let
    (
      (session-id (var-get next-session-id))
      (caller tx-sender)
      (tutor-data (unwrap! (map-get? tutors { tutor-id: tutor-id }) ERR_NOT_FOUND))
      (current-student-data (default-to { total-sessions: u0, active-sessions: u0 }
        (map-get? student-session-count { student: caller })))
    )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (get active tutor-data) ERR_NOT_FOUND)
    (try! (stx-transfer? amount caller (as-contract tx-sender)))
    
    (map-set learning-sessions
      { session-id: session-id }
      {
        student: caller,
        tutor-id: tutor-id,
        amount: amount,
        description: description,
        created-block: stacks-block-height,
        completion-deadline: (+ stacks-block-height session-duration-blocks),
        student-confirmed: false,
        tutor-confirmed: false,
        funds-released: false,
        cancelled: false
      }
    )
    
    (map-set student-session-count
      { student: caller }
      {
        total-sessions: (+ (get total-sessions current-student-data) u1),
        active-sessions: (+ (get active-sessions current-student-data) u1)
      }
    )
    
    (var-set next-session-id (+ session-id u1))
    (ok session-id)
  )
)

(define-public (confirm-session-completion (session-id uint))
  (let
    (
      (caller tx-sender)
      (session-data (unwrap! (map-get? learning-sessions { session-id: session-id }) ERR_SESSION_NOT_FOUND))
      (tutor-data (unwrap! (map-get? tutors { tutor-id: (get tutor-id session-data) }) ERR_NOT_FOUND))
    )
    (asserts! (not (get cancelled session-data)) ERR_SESSION_ALREADY_CANCELLED)
    (asserts! (not (get funds-released session-data)) ERR_SESSION_ALREADY_COMPLETED)
    (asserts! (or (is-eq caller (get student session-data)) 
                  (is-eq caller (get address tutor-data))) ERR_NOT_AUTHORIZED)
    
    (map-set learning-sessions
      { session-id: session-id }
      (merge session-data
        {
          student-confirmed: (or (get student-confirmed session-data) (is-eq caller (get student session-data))),
          tutor-confirmed: (or (get tutor-confirmed session-data) (is-eq caller (get address tutor-data)))
        }
      )
    )
    
    (let
      (
        (updated-session (unwrap! (map-get? learning-sessions { session-id: session-id }) ERR_SESSION_NOT_FOUND))
      )
      (if (and (get student-confirmed updated-session) (get tutor-confirmed updated-session))
        (begin
          (try! (release-session-funds session-id))
          (ok true)
        )
        (ok false)
      )
    )
  )
)

(define-private (release-session-funds (session-id uint))
  (let
    (
      (session-data (unwrap! (map-get? learning-sessions { session-id: session-id }) ERR_SESSION_NOT_FOUND))
      (tutor-data (unwrap! (map-get? tutors { tutor-id: (get tutor-id session-data) }) ERR_NOT_FOUND))
      (student-data (default-to { total-sessions: u0, active-sessions: u0 }
        (map-get? student-session-count { student: (get student session-data) })))
    )
    (try! (stx-transfer? (get amount session-data) (as-contract tx-sender) (get address tutor-data)))
    
    (map-set learning-sessions
      { session-id: session-id }
      (merge session-data { funds-released: true })
    )
    
    (map-set student-session-count
      { student: (get student session-data) }
      (merge student-data {
        active-sessions: (if (> (get active-sessions student-data) u0) 
                           (- (get active-sessions student-data) u1) u0)
      })
    )
    
    (ok true)
  )
)

(define-public (cancel-session (session-id uint))
  (let
    (
      (caller tx-sender)
      (session-data (unwrap! (map-get? learning-sessions { session-id: session-id }) ERR_SESSION_NOT_FOUND))
      (student-data (default-to { total-sessions: u0, active-sessions: u0 }
        (map-get? student-session-count { student: (get student session-data) })))
    )
    (asserts! (is-eq caller (get student session-data)) ERR_NOT_AUTHORIZED)
    (asserts! (not (get funds-released session-data)) ERR_SESSION_ALREADY_COMPLETED)
    (asserts! (not (get cancelled session-data)) ERR_SESSION_ALREADY_CANCELLED)
    (asserts! (< stacks-block-height (get completion-deadline session-data)) ERR_PROPOSAL_ENDED)
    
    (try! (stx-transfer? (get amount session-data) (as-contract tx-sender) caller))
    
    (map-set learning-sessions
      { session-id: session-id }
      (merge session-data { cancelled: true })
    )
    
    (map-set student-session-count
      { student: caller }
      (merge student-data {
        active-sessions: (if (> (get active-sessions student-data) u0) 
                           (- (get active-sessions student-data) u1) u0)
      })
    )
    
    (ok true)
  )
)

(define-read-only (get-learning-session (session-id uint))
  (map-get? learning-sessions { session-id: session-id })
)

(define-read-only (get-student-session-stats (student principal))
  (map-get? student-session-count { student: student })
)

(define-map tutor-bonds
  { tutor-id: uint }
  { amount: uint }
)

(define-map dispute-penalties
  { dispute-id: uint }
  { slashed: bool }
)

(define-public (bond-tutor (tutor-id uint) (amount uint))
  (let
    (
      (caller tx-sender)
      (tutor (unwrap! (map-get? tutors { tutor-id: tutor-id }) ERR_NOT_FOUND))
      (bond (default-to { amount: u0 } (map-get? tutor-bonds { tutor-id: tutor-id })))
    )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (is-eq caller (get address tutor)) ERR_NOT_AUTHORIZED)
    (try! (stx-transfer? amount caller (as-contract tx-sender)))
    (map-set tutor-bonds { tutor-id: tutor-id } { amount: (+ (get amount bond) amount) })
    (ok (get amount (unwrap! (map-get? tutor-bonds { tutor-id: tutor-id }) ERR_NOT_FOUND)))
  )
)

(define-public (withdraw-bond (tutor-id uint) (amount uint))
  (let
    (
      (caller tx-sender)
      (tutor (unwrap! (map-get? tutors { tutor-id: tutor-id }) ERR_NOT_FOUND))
      (bond (default-to { amount: u0 } (map-get? tutor-bonds { tutor-id: tutor-id })))
    )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (is-eq caller (get address tutor)) ERR_NOT_AUTHORIZED)
    (asserts! (>= (get amount bond) amount) ERR_INSUFFICIENT_BALANCE)
    (try! (stx-transfer? amount (as-contract tx-sender) caller))
    (map-set tutor-bonds { tutor-id: tutor-id } { amount: (- (get amount bond) amount) })
    (ok true)
  )
)

(define-public (slash-bond-on-dispute (dispute-id uint))
  (let
    (
      (d (unwrap! (map-get? disputes { dispute-id: dispute-id }) ERR_DISPUTE_NOT_FOUND))
      (already (map-get? dispute-penalties { dispute-id: dispute-id }))
      (bond (default-to { amount: u0 } (map-get? tutor-bonds { tutor-id: (get tutor-id d) })))
    )
    (asserts! (get resolved d) ERR_NOT_RESOLVED)
    (asserts! (is-none already) ERR_ALREADY_SLASHED)
    (asserts! (> (get arbitrator-votes-for d) (get arbitrator-votes-against d)) ERR_NOT_AUTHORIZED)
    (let
      (
        (amt (if (> (get amount bond) SLASH_AMOUNT) SLASH_AMOUNT (get amount bond)))
      )
      (try! (stx-transfer? amt (as-contract tx-sender) (get student d)))
      (map-set tutor-bonds { tutor-id: (get tutor-id d) } { amount: (- (get amount bond) amt) })
      (map-set dispute-penalties { dispute-id: dispute-id } { slashed: true })
      (ok amt)
    )
  )
)

(define-read-only (get-tutor-bond (tutor-id uint))
  (map-get? tutor-bonds { tutor-id: tutor-id })
)

(define-data-var next-leaderboard-id uint u1)

(define-map leaderboard-entries
  { leaderboard-id: uint }
  {
    tutor-id: uint,
    performance-score: uint,
    rank-position: uint,
    last-updated: uint
  }
)

(define-map tutor-leaderboard-position
  { tutor-id: uint }
  { leaderboard-id: uint }
)

(define-map subject-leaders
  { subject: (string-ascii 30) }
  { top-tutor-id: uint, top-score: uint }
)

(define-private (calculate-performance-score (tutor-id uint))
  (let
    (
      (tutor (unwrap! (map-get? tutors { tutor-id: tutor-id }) u0))
      (review-count (get review-count tutor))
      (avg-rating (if (> review-count u0) 
                     (/ (get total-rating tutor) review-count) u0))
      (rewards-score (/ (get total-rewards tutor) u1000000))
      (achievement-count (get-tutor-achievements tutor-id))
    )
    (+
      (* avg-rating u100)
      (* review-count u10)
      rewards-score
      (* achievement-count u50)
    )
  )
)

(define-public (update-leaderboard-position (tutor-id uint))
  (let
    (
      (tutor (unwrap! (map-get? tutors { tutor-id: tutor-id }) ERR_NOT_FOUND))
      (score (calculate-performance-score tutor-id))
      (existing-entry (map-get? tutor-leaderboard-position { tutor-id: tutor-id }))
      (leaderboard-id (match existing-entry
                        entry (get leaderboard-id entry)
                        (var-get next-leaderboard-id)))
    )
    (asserts! (get active tutor) ERR_NOT_FOUND)
    
    (map-set leaderboard-entries
      { leaderboard-id: leaderboard-id }
      {
        tutor-id: tutor-id,
        performance-score: score,
        rank-position: u0,
        last-updated: stacks-block-height
      }
    )
    
    (if (is-none existing-entry)
      (begin
        (map-set tutor-leaderboard-position
          { tutor-id: tutor-id }
          { leaderboard-id: leaderboard-id }
        )
        (var-set next-leaderboard-id (+ leaderboard-id u1))
      )
      true
    )
    (begin
      (update-subject-leader tutor-id score (get subject tutor))
      (ok score)
    )
  )
)


(define-private (update-subject-leader (tutor-id uint) (score uint) (subject (string-ascii 30)))
  (let
    (
      (current-leader (map-get? subject-leaders { subject: subject }))
    )
    (match current-leader
      leader (if (> score (get top-score leader))
               (map-set subject-leaders { subject: subject }
                 { top-tutor-id: tutor-id, top-score: score })
               true)
      (map-set subject-leaders { subject: subject }
        { top-tutor-id: tutor-id, top-score: score })
    )
  )
)

(define-read-only (get-tutor-leaderboard-entry (tutor-id uint))
  (match (map-get? tutor-leaderboard-position { tutor-id: tutor-id })
    position (map-get? leaderboard-entries { leaderboard-id: (get leaderboard-id position) })
    none
  )
)

(define-read-only (get-subject-top-performer (subject (string-ascii 30)))
  (map-get? subject-leaders { subject: subject })
)