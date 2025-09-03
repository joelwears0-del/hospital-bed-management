;; Hospital Bed Management System
;; A capacity planning system for healthcare facilities with bed availability tracking,
;; patient transfer coordination, and discharge planning.

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-BED-NOT-FOUND (err u101))
(define-constant ERR-BED-OCCUPIED (err u102))
(define-constant ERR-BED-NOT-OCCUPIED (err u103))
(define-constant ERR-INVALID-DEPARTMENT (err u104))
(define-constant ERR-PATIENT-NOT-FOUND (err u105))
(define-constant ERR-TRANSFER-FAILED (err u106))

;; Status constants
(define-constant BED-AVAILABLE u0)
(define-constant BED-OCCUPIED u1)
(define-constant BED-MAINTENANCE u2)
(define-constant BED-RESERVED u3)

;; Department constants
(define-constant DEPT-ICU u1)
(define-constant DEPT-EMERGENCY u2)
(define-constant DEPT-GENERAL u3)
(define-constant DEPT-SURGERY u4)

;; Contract owner
(define-constant CONTRACT-OWNER tx-sender)

;; Data structures
(define-map beds
    { bed-id: uint }
    {
        department: uint,
        status: uint,
        patient-id: (optional uint),
        admitted-at: (optional uint),
        estimated-discharge: (optional uint)
    }
)

(define-map patients
    { patient-id: uint }
    {
        name: (string-ascii 50),
        bed-id: (optional uint),
        admission-date: uint,
        condition: (string-ascii 100)
    }
)

(define-map department-capacity
    { department: uint }
    {
        total-beds: uint,
        occupied-beds: uint,
        available-beds: uint
    }
)

;; Counter variables
(define-data-var next-bed-id uint u1)
(define-data-var next-patient-id uint u1)

;; Private functions
(define-private (is-valid-department (dept uint))
    (or (is-eq dept DEPT-ICU)
        (is-eq dept DEPT-EMERGENCY)
        (is-eq dept DEPT-GENERAL)
        (is-eq dept DEPT-SURGERY))
)

(define-private (update-department-capacity (dept uint) (occupied-change int))
    (let ((capacity (default-to { total-beds: u0, occupied-beds: u0, available-beds: u0 }
                                (map-get? department-capacity { department: dept }))))
        (let ((new-occupied (if (> occupied-change 0)
                               (+ (get occupied-beds capacity) (to-uint occupied-change))
                               (- (get occupied-beds capacity) (to-uint (* occupied-change -1))))))
            (map-set department-capacity
                { department: dept }
                {
                    total-beds: (get total-beds capacity),
                    occupied-beds: new-occupied,
                    available-beds: (- (get total-beds capacity) new-occupied)
                }
            )
        )
    )
)

;; Public functions

;; Add a new bed to a department
(define-public (add-bed (department uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (asserts! (is-valid-department department) ERR-INVALID-DEPARTMENT)
        
        (let ((bed-id (var-get next-bed-id)))
            (map-set beds
                { bed-id: bed-id }
                {
                    department: department,
                    status: BED-AVAILABLE,
                    patient-id: none,
                    admitted-at: none,
                    estimated-discharge: none
                }
            )
            
            ;; Update department capacity
            (let ((capacity (default-to { total-beds: u0, occupied-beds: u0, available-beds: u0 }
                                        (map-get? department-capacity { department: department }))))
                (map-set department-capacity
                    { department: department }
                    {
                        total-beds: (+ (get total-beds capacity) u1),
                        occupied-beds: (get occupied-beds capacity),
                        available-beds: (+ (get available-beds capacity) u1)
                    }
                )
            )
            
            (var-set next-bed-id (+ bed-id u1))
            (ok bed-id)
        )
    )
)

;; Admit a patient to a bed
(define-public (admit-patient (patient-name (string-ascii 50)) (bed-id uint) (condition (string-ascii 100)) (estimated-discharge uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        
        (let ((bed-data (unwrap! (map-get? beds { bed-id: bed-id }) ERR-BED-NOT-FOUND)))
            (asserts! (is-eq (get status bed-data) BED-AVAILABLE) ERR-BED-OCCUPIED)
            
            (let ((patient-id (var-get next-patient-id)))
                ;; Create patient record
                (map-set patients
                    { patient-id: patient-id }
                    {
                        name: patient-name,
                        bed-id: (some bed-id),
                        admission-date: stacks-block-height,
                        condition: condition
                    }
                )
                
                ;; Update bed status
                (map-set beds
                    { bed-id: bed-id }
                    {
                        department: (get department bed-data),
                        status: BED-OCCUPIED,
                        patient-id: (some patient-id),
                        admitted-at: (some stacks-block-height),
                        estimated-discharge: (some estimated-discharge)
                    }
                )
                
                ;; Update department capacity
                (update-department-capacity (get department bed-data) 1)
                
                (var-set next-patient-id (+ patient-id u1))
                (ok patient-id)
            )
        )
    )
)

;; Discharge a patient
(define-public (discharge-patient (patient-id uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        
        (let ((patient-data (unwrap! (map-get? patients { patient-id: patient-id }) ERR-PATIENT-NOT-FOUND)))
            (let ((bed-id (unwrap! (get bed-id patient-data) ERR-BED-NOT-OCCUPIED)))
                (let ((bed-data (unwrap! (map-get? beds { bed-id: bed-id }) ERR-BED-NOT-FOUND)))
                    ;; Update bed status
                    (map-set beds
                        { bed-id: bed-id }
                        {
                            department: (get department bed-data),
                            status: BED-AVAILABLE,
                            patient-id: none,
                            admitted-at: none,
                            estimated-discharge: none
                        }
                    )
                    
                    ;; Update patient record
                    (map-set patients
                        { patient-id: patient-id }
                        {
                            name: (get name patient-data),
                            bed-id: none,
                            admission-date: (get admission-date patient-data),
                            condition: (get condition patient-data)
                        }
                    )
                    
                    ;; Update department capacity
                    (update-department-capacity (get department bed-data) -1)
                    
                    (ok true)
                )
            )
        )
    )
)

;; Transfer patient between beds
(define-public (transfer-patient (patient-id uint) (new-bed-id uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        
        (let ((patient-data (unwrap! (map-get? patients { patient-id: patient-id }) ERR-PATIENT-NOT-FOUND)))
            (let ((old-bed-id (unwrap! (get bed-id patient-data) ERR-BED-NOT-OCCUPIED)))
                (let ((old-bed-data (unwrap! (map-get? beds { bed-id: old-bed-id }) ERR-BED-NOT-FOUND))
                      (new-bed-data (unwrap! (map-get? beds { bed-id: new-bed-id }) ERR-BED-NOT-FOUND)))
                    (asserts! (is-eq (get status new-bed-data) BED-AVAILABLE) ERR-BED-OCCUPIED)
                    
                    ;; Free old bed
                    (map-set beds
                        { bed-id: old-bed-id }
                        {
                            department: (get department old-bed-data),
                            status: BED-AVAILABLE,
                            patient-id: none,
                            admitted-at: none,
                            estimated-discharge: none
                        }
                    )
                    
                    ;; Occupy new bed
                    (map-set beds
                        { bed-id: new-bed-id }
                        {
                            department: (get department new-bed-data),
                            status: BED-OCCUPIED,
                            patient-id: (some patient-id),
                            admitted-at: (get admitted-at old-bed-data),
                            estimated-discharge: (get estimated-discharge old-bed-data)
                        }
                    )
                    
                    ;; Update patient record
                    (map-set patients
                        { patient-id: patient-id }
                        {
                            name: (get name patient-data),
                            bed-id: (some new-bed-id),
                            admission-date: (get admission-date patient-data),
                            condition: (get condition patient-data)
                        }
                    )
                    
                    ;; Update department capacities if transferring between departments
                    (if (not (is-eq (get department old-bed-data) (get department new-bed-data)))
                        (begin
                            (update-department-capacity (get department old-bed-data) -1)
                            (update-department-capacity (get department new-bed-data) 1)
                        )
                        true
                    )
                    
                    (ok true)
                )
            )
        )
    )
)

;; Set bed maintenance status
(define-public (set-bed-maintenance (bed-id uint) (maintenance bool))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        
        (let ((bed-data (unwrap! (map-get? beds { bed-id: bed-id }) ERR-BED-NOT-FOUND)))
            (asserts! (is-eq (get status bed-data) BED-AVAILABLE) ERR-BED-OCCUPIED)
            
            (let ((new-status (if maintenance BED-MAINTENANCE BED-AVAILABLE)))
                (map-set beds
                    { bed-id: bed-id }
                    {
                        department: (get department bed-data),
                        status: new-status,
                        patient-id: none,
                        admitted-at: none,
                        estimated-discharge: none
                    }
                )
                
                (ok true)
            )
        )
    )
)

;; Read-only functions

;; Get bed information
(define-read-only (get-bed-info (bed-id uint))
    (map-get? beds { bed-id: bed-id })
)

;; Get patient information
(define-read-only (get-patient-info (patient-id uint))
    (map-get? patients { patient-id: patient-id })
)

;; Get department capacity
(define-read-only (get-department-capacity (department uint))
    (map-get? department-capacity { department: department })
)

;; Get available beds in department
(define-read-only (get-available-beds (department uint))
    (let ((capacity (map-get? department-capacity { department: department })))
        (match capacity
            capacity-data (ok (get available-beds capacity-data))
            (ok u0)
        )
    )
)
