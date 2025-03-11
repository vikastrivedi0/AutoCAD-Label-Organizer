;;; Civil 3D Label Overlap Detector - Entity Data Analysis Method
;;; This script detects overlapping Civil 3D labels by analyzing complete entity data.
;;; Usage: Load the script and run the command C3D-ENTITY-OVERLAP

;; Function to get all points from entity data
(defun get-entity-points (ent / entdata pts pt)
  (setq entdata (entget ent)
        pts '())
  
  ;; Collect all points from entity data
  (while entdata
    (cond
      ((= 10 (caar entdata)) ; Base point
       (setq pts (cons (cdar entdata) pts)))
      ((= 11 (caar entdata)) ; Alignment point
       (setq pts (cons (cdar entdata) pts)))
      ((= 12 (caar entdata)) ; Second alignment point
       (setq pts (cons (cdar entdata) pts)))
      ((= 13 (caar entdata)) ; Third alignment point
       (setq pts (cons (cdar entdata) pts)))
      ((= 14 (caar entdata)) ; Drag point
       (setq pts (cons (cdar entdata) pts)))
      ((= 15 (caar entdata)) ; Text position
       (setq pts (cons (cdar entdata) pts)))
    )
    (setq entdata (cdr entdata))
  )
  pts
)

;; Function to calculate convex hull of points
(defun calc-convex-hull (points / hull p0 stack)
  ;; Find point with lowest y-coordinate (and leftmost if tied)
  (setq p0 (car points))
  (foreach pt (cdr points)
    (if (or (< (cadr pt) (cadr p0))
            (and (= (cadr pt) (cadr p0))
                 (< (car pt) (car p0))))
      (setq p0 pt)
    )
  )
  
  ;; Sort points by polar angle with respect to p0
  (setq points (vl-sort points
    (function (lambda (p1 p2)
      (< (angle-to-p0 p0 p1) (angle-to-p0 p0 p2))
    ))
  ))
  
  ;; Graham scan algorithm
  (setq stack (list (car points) (cadr points)))
  (foreach pt (cddr points)
    (while (and (>= (length stack) 2)
                (not (ccw (cadr (reverse stack))
                         (car (reverse stack))
                         pt)))
      (setq stack (reverse (cdr (reverse stack))))
    )
    (setq stack (cons pt stack))
  )
  stack
)

;; Helper function for convex hull - calculate angle
(defun angle-to-p0 (p0 p1)
  (atan (- (cadr p1) (cadr p0))
        (- (car p1) (car p0)))
)

;; Helper function for convex hull - counter-clockwise test
(defun ccw (p1 p2 p3)
  (> (* (- (car p2) (car p1))
        (- (cadr p3) (cadr p1)))
     (* (- (cadr p2) (cadr p1))
        (- (car p3) (car p1)))
  )
)

;; Function to check if two polygons overlap
(defun polygons-overlap (poly1 poly2 / i j)
  ;; Check if any point from poly1 is inside poly2
  (foreach pt poly1
    (if (point-in-polygon pt poly2)
      (return T)
    )
  )
  ;; Check if any point from poly2 is inside poly1
  (foreach pt poly2
    (if (point-in-polygon pt poly1)
      (return T)
    )
  )
  ;; Check if any edges intersect
  (setq i 0)
  (repeat (length poly1)
    (setq j 0)
    (repeat (length poly2)
      (if (lines-intersect (nth i poly1)
                          (nth (mod (1+ i) (length poly1)) poly1)
                          (nth j poly2)
                          (nth (mod (1+ j) (length poly2)) poly2))
        (return T)
      )
      (setq j (1+ j))
    )
    (setq i (1+ i))
  )
  nil
)

;; Helper function to check if a point is inside a polygon
(defun point-in-polygon (pt poly / i j n inside x1 y1 x2 y2)
  (setq n (length poly)
        inside nil
        j (1- n))
  
  (setq x (car pt)
        y (cadr pt))
  
  (repeat n
    (setq x1 (car (nth i poly))
          y1 (cadr (nth i poly))
          x2 (car (nth j poly))
          y2 (cadr (nth j poly)))
    
    (if (and (or (> y1 y) (> y2 y))
             (or (< y1 y) (< y2 y))
             (< x (+ x1 (/ (* (- x2 x1) (- y y1)) (- y2 y1)))))
      (setq inside (not inside))
    )
    (setq j i
          i (1+ i))
  )
  inside
)

;; Helper function to check if two line segments intersect
(defun lines-intersect (p1 p2 p3 p4 / d)
  (setq d (- (* (- (car p2) (car p1))
               (- (cadr p4) (cadr p3)))
            (* (- (cadr p2) (cadr p1))
               (- (car p4) (car p3)))))
  
  (if (/= d 0)
    (setq ua (/ (- (* (- (car p4) (car p1))
                      (- (cadr p4) (cadr p3)))
                   (* (- (cadr p4) (cadr p1))
                      (- (car p4) (car p3))))
                d))
    nil
  )
  
  (and ua (>= ua 0) (<= ua 1))
)

;; Function to process a single label
(defun process-label (ent mtextData / points hull)
  (if (and ent (not (null ent)) (entget ent))  ; Validate entity
    (progn
      (setq points (get-entity-points ent))
      (if points
        (progn
          (setq hull (calc-convex-hull points))
          (princ (strcat "\nProcessed label hull: " (vl-princ-to-string hull)))
          (setq mtextData 
            (cons 
              (list 
                ent 
                hull
              )
              mtextData
            )
          )
        )
        (princ (strcat "\nWarning: No points found for entity " (vl-princ-to-string ent)))
      )
    )
    (princ (strcat "\nWarning: Invalid entity encountered"))
  )
  mtextData
)

(defun c:C3D-ENTITY-OVERLAP (/ ss1 ss2 ent mtextData overlapList count)
  (princ "\nSearching for Civil 3D labels...")
  
  ;; Get both Pipe Labels and Structure Labels
  (setq ss1 (ssget "_X" '((0 . "AECC_PIPE_LABEL")))
        ss2 (ssget "_X" '((0 . "AECC_STRUCTURE_LABEL"))))
  
  (if (or ss1 ss2)
    (progn
      (setq mtextData '())
      (setq overlapList '())

      ;; Process Pipe Labels
      (if ss1
        (progn
          (princ (strcat "\nProcessing " (itoa (sslength ss1)) " Pipe Labels..."))
          (setq count (sslength ss1))
          (while (> count 0)
            (setq count (1- count))
            (setq ent (ssname ss1 count))
            (setq mtextData (process-label ent mtextData))
          )
        )
      )

      ;; Process Structure Labels
      (if ss2
        (progn
          (princ (strcat "\nProcessing " (itoa (sslength ss2)) " Structure Labels..."))
          (setq count (sslength ss2))
          (while (> count 0)
            (setq count (1- count))
            (setq ent (ssname ss2 count))
            (setq mtextData (process-label ent mtextData))
          )
        )
      )

      (princ "\nChecking for overlaps...")
      
      ;; Check for overlaps
      (foreach data1 mtextData
        (foreach data2 mtextData
          (if (and (not (eq (car data1) (car data2)))  ; Not the same label
                   (polygons-overlap (cadr data1) (cadr data2))  ; Check hulls overlap
                   (entget (car data1))                 ; Verify entities still exist
                   (entget (car data2)))
            (progn
              (if (not (member (car data2) overlapList))
                (progn
                  (princ (strcat "\nFound overlapping labels"))
                  (setq overlapList (cons (car data2) overlapList))
                )
              )
            )
          )
        )
      )

      ;; Remove duplicates from overlap list
      (setq overlapList (LM:remove-duplicates overlapList))

      ;; Delete overlapping labels with validation
      (print (strcat "\nFound " (itoa (length overlapList)) " overlapping Civil 3D labels."))
      
      (if (> (length overlapList) 0)
        (progn
          (princ "\nPress Enter to delete overlapping labels, or Esc to cancel...")
          (setq user-input (getstring))
          (if (= user-input "")  ; Check for empty string (Enter key)
            (progn
              (setq deleted-count 0)
              (foreach ent overlapList
                (if (and ent (entget ent))  ; Verify entity still exist
                  (progn
                    (entdel ent)
                    (setq deleted-count (1+ deleted-count))
                  )
                )
              )
              (print (strcat "\nCompleted. Successfully deleted " (itoa deleted-count) " overlapping Civil 3D labels."))
            )
            (print "\nOperation cancelled by user.")
          )
        )
        (print "\nNo overlapping labels found.")
      )
    )
    (print "\nNo Civil 3D Pipe or Structure labels found in the drawing.")
  )
  (princ)
)

;; Load the function
(princ "\nCivil 3D Label Overlap Detector (Entity Data Method) loaded. Type C3D-ENTITY-OVERLAP to run.")
(princ) 