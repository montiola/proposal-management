;; Proposal Management Smart Contract

(define-map proposals 
    { proposal-id: uint }
    {
        title: (string-utf8 100),
        description: (string-utf8 500),
        proposer: principal,
        creation-time: uint,
        vote-start: uint,
        vote-end: uint,
        total-for-votes: uint,
        total-against-votes: uint,
        status: (string-utf8 20),
        executed: bool
    }
)

;; Track voter participation to prevent double voting
(define-map voter-participation
    { proposal-id: uint, voter: principal }
    { has-voted: bool }
)

;; Tracks the next available proposal ID
(define-data-var next-proposal-id uint u0)

;; Error constants
(define-constant ERR-UNAUTHORIZED (err u1))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u2))
(define-constant ERR-VOTING-NOT-ACTIVE (err u3))
(define-constant ERR-ALREADY-VOTED (err u4))
(define-constant ERR-VOTING-ENDED (err u5))
(define-constant ERR-PROPOSAL-ALREADY-EXECUTED (err u6))
(define-constant ERR-INVALID-VOTING-PERIOD (err u7))
(define-constant ERR-INVALID-TITLE (err u8))
(define-constant ERR-INVALID-DESCRIPTION (err u9))
(define-constant ERR-INVALID-PROPOSAL-ID (err u10))

;; Only contract owner can perform admin actions
(define-constant CONTRACT-OWNER tx-sender)

;; Helper function to validate string input
(define-private (validate-string-input (input (string-utf8 500)))
    (and 
        (not (is-eq input u""))
        (< (len input) u500)
    )
)

;; Helper function to validate proposal ID
(define-private (validate-proposal-id (id uint))
    (< id (var-get next-proposal-id))
)

;; Create a new proposal
(define-public (create-proposal 
    (title (string-utf8 100))
    (description (string-utf8 500))
    (vote-start uint)
    (vote-end uint)
)
    (begin
        ;; Validate inputs
        (asserts! (validate-string-input title) ERR-INVALID-TITLE)
        (asserts! (validate-string-input description) ERR-INVALID-DESCRIPTION)
        
        ;; Validate voting period
        (asserts! (> vote-end vote-start) ERR-INVALID-VOTING-PERIOD)
        
        ;; Get next proposal ID
        (let ((proposal-id (var-get next-proposal-id)))
            
            ;; Store proposal details
            (map-set proposals 
                { proposal-id: proposal-id }
                {
                    title: title,
                    description: description,
                    proposer: tx-sender,
                    creation-time: block-height,
                    vote-start: vote-start,
                    vote-end: vote-end,
                    total-for-votes: u0,
                    total-against-votes: u0,
                    status: u"PENDING",
                    executed: false
                }
            )
            
            ;; Increment proposal ID
            (var-set next-proposal-id (+ proposal-id u1))
            
            ;; Return proposal ID
            (ok proposal-id)
        )
    )
)

;; Vote on a proposal
(define-public (vote 
    (proposal-id uint)
    (vote-for bool)
)
    (begin
        ;; Validate proposal ID
        (asserts! (validate-proposal-id proposal-id) ERR-INVALID-PROPOSAL-ID)
        
        (let 
            (
                (proposal (unwrap! 
                    (map-get? proposals { proposal-id: proposal-id }) 
                    ERR-PROPOSAL-NOT-FOUND
                ))
                (current-block block-height)
            )
            ;; Check voting is active
            (asserts! 
                (and 
                    (<= current-block (get vote-end proposal))
                    (>= current-block (get vote-start proposal))
                ) 
                ERR-VOTING-NOT-ACTIVE
            )
            
            ;; Prevent double voting
            (asserts! 
                (is-none (map-get? voter-participation 
                    { proposal-id: proposal-id, voter: tx-sender }
                )) 
                ERR-ALREADY-VOTED
            )
            
            ;; Record voter participation
            (map-set voter-participation 
                { proposal-id: proposal-id, voter: tx-sender }
                { has-voted: true }
            )
            
            ;; Update vote counts
            (if vote-for
                (map-set proposals 
                    { proposal-id: proposal-id }
                    (merge proposal { 
                        total-for-votes: (+ (get total-for-votes proposal) u1) 
                    })
                )
                (map-set proposals 
                    { proposal-id: proposal-id }
                    (merge proposal { 
                        total-against-votes: (+ (get total-against-votes proposal) u1) 
                    })
                )
            )
            
            (ok true)
        )
    )
)

;; Execute a proposal if voting has ended and it passes
(define-public (execute-proposal (proposal-id uint))
    (begin
        ;; Validate proposal ID
        (asserts! (validate-proposal-id proposal-id) ERR-INVALID-PROPOSAL-ID)
        
        (let 
            (
                (proposal (unwrap! 
                    (map-get? proposals { proposal-id: proposal-id }) 
                    ERR-PROPOSAL-NOT-FOUND
                ))
                (current-block block-height)
            )
            ;; Check proposal hasn't been executed
            (asserts! (not (get executed proposal)) ERR-PROPOSAL-ALREADY-EXECUTED)
            
            ;; Check voting has ended
            (asserts! (> current-block (get vote-end proposal)) ERR-VOTING-NOT-ACTIVE)
            
            ;; Check if proposal passes (more votes for than against)
            (let ((passes (> (get total-for-votes proposal) (get total-against-votes proposal))))
                (if passes
                    (begin
                        ;; Mark proposal as executed
                        (map-set proposals 
                            { proposal-id: proposal-id }
                            (merge proposal { 
                                status: u"PASSED",
                                executed: true 
                            })
                        )
                        (ok true)
                    )
                    (begin
                        ;; Mark proposal as failed
                        (map-set proposals 
                            { proposal-id: proposal-id }
                            (merge proposal { 
                                status: u"FAILED",
                                executed: true 
                            })
                        )
                        (ok false)
                    )
                )
            )
        )
    )
)

;; Read a proposal's details
(define-read-only (get-proposal-details (proposal-id uint))
    (if (validate-proposal-id proposal-id)
        (map-get? proposals { proposal-id: proposal-id })
        none
    )
)

;; Administrative function to cancel a proposal (only by contract owner)
(define-public (cancel-proposal (proposal-id uint))
    (begin
        ;; Validate proposal ID
        (asserts! (validate-proposal-id proposal-id) ERR-INVALID-PROPOSAL-ID)
        
        (let 
            (
                (proposal (unwrap! 
                    (map-get? proposals { proposal-id: proposal-id }) 
                    ERR-PROPOSAL-NOT-FOUND
                ))
            )
            ;; Only contract owner can cancel
            (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
            
            ;; Update proposal status
            (map-set proposals 
                { proposal-id: proposal-id }
                (merge proposal { 
                    status: u"CANCELLED",
                    executed: true 
                })
            )
            
            (ok true)
        )
    )
)

;; Get total number of proposals
(define-read-only (get-total-proposals)
    (var-get next-proposal-id)
)