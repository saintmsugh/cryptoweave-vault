;; CryptoWeave-Substrate

(define-constant NEXUS_OWNER tx-sender)
(define-constant ERR_ASSET_NOT_FOUND (err u301))
(define-constant ERR_DUPLICATE_ENTRY (err u302))
(define-constant ERR_BAD_INPUT (err u303))
(define-constant ERR_LIMIT_EXCEEDED (err u304))
(define-constant ERR_ACCESS_DENIED (err u305))
(define-constant ERR_NO_PERMISSION (err u306))
(define-constant ERR_ADMIN_ONLY (err u307))
(define-constant ERR_FORBIDDEN_ACTION (err u308))

(define-data-var vault-item-counter uint u0)
(define-data-var bundle-sequence-id uint u0)

;; Primary vault item storage structure
(define-map nexus-vault-items
    {item-id: uint}
    {
        title: (string-ascii 64),
        creator: (string-ascii 32),
        owner: principal,
        duration: uint,
        creation-block: uint,
        item-type: (string-ascii 32),
        tags: (list 8 (string-ascii 24))
    }
)

;; Permission management for vault items
(define-map vault-permissions
    {item-id: uint, user: principal}
    {has-access: bool}
)

;; Access grant history tracking
(define-map permission-history
    {item-id: uint, grantor: principal, grantee: principal}
    {
        granted-at: uint,
        revoked-at: uint,
        is-active: bool
    }
)

;; Item rating and review system
(define-map item-reviews
    {item-id: uint, reviewer: principal}
    {
        rating: uint,
        comment: (optional (string-ascii 256)),
        review-time: uint,
        first-review-time: uint
    }
)

;; Statistical data for reviews
(define-map review-stats
    {item-id: uint}
    {
        review-count: uint,
        latest-review: uint
    }
)

;; Bundle creation and management
(define-map vault-bundles
    {bundle-id: uint}
    {
        bundle-name: (string-ascii 64),
        description: (string-ascii 256),
        manager: principal,
        category: (string-ascii 32),
        created-at: uint,
        updated-at: uint,
        item-count: uint,
        public-access: bool
    }
)

;; Bundle membership tracking
(define-map bundle-members
    {bundle-id: uint, member: principal}
    {
        is-member: bool,
        joined-at: uint,
        is-admin: bool
    }
)

;; Item to bundle associations
(define-map bundle-items
    {bundle-id: uint, item-id: uint}
    {
        added-by: principal,
        added-at: uint
    }
)

;; Personal collection management
(define-map personal-collections
    {owner: principal, collection-id: uint}
    {
        name: (string-ascii 64),
        description: (string-ascii 128),
        created-at: uint,
        modified-at: uint,
        item-count: uint,
        is-public: bool
    }
)

;; Collection item relationships
(define-map collection-items
    {owner: principal, collection-id: uint, item-id: uint}
    {
        added-at: uint,
        order-index: uint
    }
)

;; User collection counters
(define-map user-collection-counters
    {owner: principal}
    {next-collection-id: uint}
)

;; Internal helper functions

(define-private (item-exists (item-id uint))
    (is-some (map-get? nexus-vault-items {item-id: item-id}))
)

(define-private (get-item-duration (item-id uint))
    (default-to u0 
        (get duration 
            (map-get? nexus-vault-items {item-id: item-id})
        )
    )
)

(define-private (is-valid-tag (tag (string-ascii 24)))
    (and 
        (> (len tag) u0)
        (< (len tag) u25)
    )
)

(define-private (validate-tag-list (tags (list 8 (string-ascii 24))))
    (and
        (> (len tags) u0)
        (<= (len tags) u8)
        (is-eq (len (filter is-valid-tag tags)) (len tags))
    )
)

(define-private (is-item-owner (item-id uint) (user principal))
    (match (map-get? nexus-vault-items {item-id: item-id})
        item-data (is-eq (get owner item-data) user)
        false
    )
)

(define-private (get-next-collection-id (owner principal))
    (get next-collection-id (default-to {next-collection-id: u0} 
        (map-get? user-collection-counters {owner: owner})))
)

;; Core vault item operations

(define-public (create-vault-item 
        (title (string-ascii 64))
        (creator (string-ascii 32))
        (duration uint)
        (item-type (string-ascii 32))
        (tags (list 8 (string-ascii 24)))
    )
    (let
        ((new-item-id (+ (var-get vault-item-counter) u1)))

        (asserts! (and (> (len title) u0) (< (len title) u65)) ERR_BAD_INPUT)
        (asserts! (and (> (len creator) u0) (< (len creator) u33)) ERR_BAD_INPUT)
        (asserts! (and (> duration u0) (< duration u10000)) ERR_LIMIT_EXCEEDED)
        (asserts! (and (> (len item-type) u0) (< (len item-type) u33)) ERR_BAD_INPUT)
        (asserts! (validate-tag-list tags) ERR_BAD_INPUT)

        (map-insert nexus-vault-items
            {item-id: new-item-id}
            {
                title: title,
                creator: creator,
                owner: tx-sender,
                duration: duration,
                creation-block: block-height,
                item-type: item-type,
                tags: tags
            }
        )

        (map-insert vault-permissions
            {item-id: new-item-id, user: tx-sender}
            {has-access: true}
        )

        (var-set vault-item-counter new-item-id)
        (ok new-item-id)
    )
)

(define-public (remove-vault-item (item-id uint))
    (let
        ((item-data (unwrap! (map-get? nexus-vault-items {item-id: item-id}) ERR_ASSET_NOT_FOUND)))

        (asserts! (item-exists item-id) ERR_ASSET_NOT_FOUND)
        (asserts! (is-eq (get owner item-data) tx-sender) ERR_ACCESS_DENIED)

        (map-delete nexus-vault-items {item-id: item-id})
        (map-delete vault-permissions {item-id: item-id, user: tx-sender})
        (ok true)
    )
)

(define-public (change-item-owner (item-id uint) (new-owner principal))
    (let
        ((item-data (unwrap! (map-get? nexus-vault-items {item-id: item-id}) ERR_ASSET_NOT_FOUND)))

        (asserts! (item-exists item-id) ERR_ASSET_NOT_FOUND)
        (asserts! (is-eq (get owner item-data) tx-sender) ERR_ACCESS_DENIED)

        (map-set nexus-vault-items
            {item-id: item-id}
            (merge item-data {owner: new-owner})
        )
        (ok true)
    )
)

(define-public (update-item-metadata 
        (item-id uint) 
        (new-title (string-ascii 64)) 
        (new-duration uint) 
        (new-type (string-ascii 32)) 
        (new-tags (list 8 (string-ascii 24)))
    )
    (let
        ((item-data (unwrap! (map-get? nexus-vault-items {item-id: item-id}) ERR_ASSET_NOT_FOUND)))

        (asserts! (item-exists item-id) ERR_ASSET_NOT_FOUND)
        (asserts! (is-eq (get owner item-data) tx-sender) ERR_ACCESS_DENIED)
        (asserts! (and (> (len new-title) u0) (< (len new-title) u65)) ERR_BAD_INPUT)
        (asserts! (and (> new-duration u0) (< new-duration u10000)) ERR_LIMIT_EXCEEDED)
        (asserts! (and (> (len new-type) u0) (< (len new-type) u33)) ERR_BAD_INPUT)
        (asserts! (validate-tag-list new-tags) ERR_BAD_INPUT)

        (map-set nexus-vault-items
            {item-id: item-id}
            (merge item-data {
                title: new-title,
                duration: new-duration,
                item-type: new-type,
                tags: new-tags
            })
        )
        (ok true)
    )
)

;; Permission and access control

(define-public (grant-item-access 
        (item-id uint)
        (target-user principal)
    )
    (let
        ((item-data (unwrap! (map-get? nexus-vault-items {item-id: item-id}) ERR_ASSET_NOT_FOUND)))

        (asserts! (item-exists item-id) ERR_ASSET_NOT_FOUND)
        (asserts! (is-eq (get owner item-data) tx-sender) ERR_ACCESS_DENIED)
        (asserts! (not (is-eq tx-sender target-user)) ERR_BAD_INPUT)

        (asserts! (is-none (map-get? vault-permissions {item-id: item-id, user: target-user})) 
                 ERR_DUPLICATE_ENTRY)

        (map-insert vault-permissions
            {item-id: item-id, user: target-user}
            {has-access: true}
        )

        (map-insert permission-history
            {item-id: item-id, grantor: tx-sender, grantee: target-user}
            {
                granted-at: block-height,
                revoked-at: u0,
                is-active: true
            }
        )

        (ok true)
    )
)

(define-public (revoke-item-access 
        (item-id uint)
        (target-user principal)
    )
    (let
        ((item-data (unwrap! (map-get? nexus-vault-items {item-id: item-id}) ERR_ASSET_NOT_FOUND))
         (history-data (unwrap! (map-get? permission-history {item-id: item-id, grantor: tx-sender, grantee: target-user}) ERR_ASSET_NOT_FOUND)))

        (asserts! (item-exists item-id) ERR_ASSET_NOT_FOUND)
        (asserts! (is-eq (get owner item-data) tx-sender) ERR_ACCESS_DENIED)
        (asserts! (get is-active history-data) ERR_NO_PERMISSION)

        (ok true)
    )
)

;; Review and rating system

(define-public (submit-item-review 
        (item-id uint)
        (rating uint)
        (comment (optional (string-ascii 256)))
    )
    (let
        ((item-data (unwrap! (map-get? nexus-vault-items {item-id: item-id}) ERR_ASSET_NOT_FOUND))
         (permission-data (default-to {has-access: false} (map-get? vault-permissions {item-id: item-id, user: tx-sender})))
         (existing-review (map-get? item-reviews {item-id: item-id, reviewer: tx-sender})))

        (asserts! (item-exists item-id) ERR_ASSET_NOT_FOUND)
        (asserts! (or 
                    (is-eq (get owner item-data) tx-sender)
                    (get has-access permission-data)
                  ) 
                ERR_NO_PERMISSION)
        (asserts! (and (>= rating u1) (<= rating u5)) ERR_BAD_INPUT)

        (if (is-some comment)
            (asserts! (and 
                        (> (len (default-to "" comment)) u0) 
                        (< (len (default-to "" comment)) u257)
                      ) 
                    ERR_BAD_INPUT)
            true
        )

        (if (is-some existing-review)
            (map-set item-reviews
                {item-id: item-id, reviewer: tx-sender}
                {
                    rating: rating,
                    comment: comment,
                    review-time: block-height,
                    first-review-time: (get first-review-time (unwrap! existing-review ERR_ASSET_NOT_FOUND))
                }
            )
            (map-insert item-reviews
                {item-id: item-id, reviewer: tx-sender}
                {
                    rating: rating,
                    comment: comment,
                    review-time: block-height,
                    first-review-time: block-height
                }
            )
        )

        (match (map-get? review-stats {item-id: item-id})
            existing-stats (map-set review-stats
                {item-id: item-id}
                (merge existing-stats {
                    review-count: (if (is-some existing-review) 
                                      (get review-count existing-stats) 
                                      (+ (get review-count existing-stats) u1)),
                    latest-review: block-height
                })
            )
            (map-insert review-stats
                {item-id: item-id}
                {
                    review-count: u1,
                    latest-review: block-height
                }
            )
        )

        (ok true)
    )
)

;; Collection management functions

(define-public (add-to-personal-collection 
        (collection-id uint)
        (item-id uint)
    )
    (let
        ((collection-data (unwrap! (map-get? personal-collections {owner: tx-sender, collection-id: collection-id}) ERR_ASSET_NOT_FOUND))
         (item-data (unwrap! (map-get? nexus-vault-items {item-id: item-id}) ERR_ASSET_NOT_FOUND))
         (permission-status (default-to {has-access: false} (map-get? vault-permissions {item-id: item-id, user: tx-sender}))))

        (asserts! (item-exists item-id) ERR_ASSET_NOT_FOUND)
        (asserts! (or 
                    (is-eq (get owner item-data) tx-sender)
                    (get has-access permission-status)
                  ) 
                ERR_NO_PERMISSION)

        (asserts! (is-none (map-get? collection-items {owner: tx-sender, collection-id: collection-id, item-id: item-id})) 
                 ERR_DUPLICATE_ENTRY)

        (ok true)
    )
)

;; Query functions for data retrieval

(define-read-only (get-vault-item-info (item-id uint))
    (map-get? nexus-vault-items {item-id: item-id})
)

(define-read-only (check-user-permission (item-id uint) (user principal))
    (default-to {has-access: false} 
        (map-get? vault-permissions {item-id: item-id, user: user})
    )
)

(define-read-only (get-item-review-stats (item-id uint))
    (map-get? review-stats {item-id: item-id})
)

(define-read-only (get-user-review (item-id uint) (reviewer principal))
    (map-get? item-reviews {item-id: item-id, reviewer: reviewer})
)

(define-read-only (get-nexus-statistics)
    {
        total-vault-items: (var-get vault-item-counter),
        total-bundles: (var-get bundle-sequence-id)
    }
)

