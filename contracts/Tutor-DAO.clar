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

(initialize-achievements)