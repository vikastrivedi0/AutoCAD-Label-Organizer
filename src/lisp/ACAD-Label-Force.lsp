;; AutoCAD Label Organizer
;; Version: 1.0.0
;; Author: Original implementation and improvements by the AutoCAD-Label-Organizer team
;; License: MIT
;; Repository: https://github.com/[your-username]/AutoCAD-Label-Organizer

;; Description:
;; This LISP application provides advanced label organization capabilities for AutoCAD,
;; implementing both greedy and force-directed placement algorithms to resolve
;; overlapping MTEXT labels while maintaining readability and visual clarity.

;; Global Parameters
(setq *MIN-DISTANCE* 8.0)        ; Minimum distance between labels
(setq *MARGIN* 4.0)              ; Margin around labels for overlap detection
(setq *MAX-PASSES* 3)            ; Number of placement attempts
(setq *MAX-ATTEMPTS* 1000)       ; Maximum attempts to find position
(setq *OVERLAP-WEIGHT* 0.9)      ; Priority weight for overlaps
(setq *SIZE-WEIGHT* 0.1)         ; Priority weight for label size

;; Utility Functions

(defun point-distance (p1 p2 / dx dy)
  "Calculate Euclidean distance between two points"
  (setq dx (- (car p2) (car p1))
        dy (- (cadr p2) (cadr p1)))
  (sqrt (+ (* dx dx) (* dy dy)))
)

(defun create-leader (start-pt end-pt / leader)
  "Create a leader line between two points"
  (princ (strcat "\nCreating leader from " (vl-princ-to-string start-pt) " to " (vl-princ-to-string end-pt)))
  
  ;; Check if points are different
  (if (equal start-pt end-pt 0.001)
    (progn
      (princ "\nPoints are the same, skipping leader creation")
      (return nil)
    )
  )
  
  ;; Create line entity
  (setq leader (entmakex (list
    (cons 0 "LINE")
    (cons 8 "0")         ; Layer
    (cons 10 start-pt)   ; Start point
    (cons 11 end-pt)     ; End point
    (cons 62 1)          ; Color (red)
    (cons 6 "CONTINUOUS"); Linetype
    (cons 48 0.5)        ; Linetype scale
  )))
  
  ;; Verify leader was created
  (if leader
    (princ "\nLeader created successfully")
    (princ "\nFailed to create leader")
  )
  
  leader
)

;; Overlap Detection Functions

(defun get-expanded-bbox (entdata / insert-pt width height margin)
  "Get expanded bounding box for an MTEXT entity"
  (setq insert-pt (cdr (assoc 10 entdata))
        width (cdr (assoc 41 entdata))
        height (cdr (assoc 43 entdata))
        margin *MARGIN*)
  
  (list
    (list (- (car insert-pt) (/ (+ width margin) 2))
          (- (cadr insert-pt) (/ (+ height margin) 2))
          (caddr insert-pt))
    (list (+ (car insert-pt) (/ (+ width margin) 2))
          (+ (cadr insert-pt) (/ (+ height margin) 2))
          (caddr insert-pt))
  )
)

(defun bbox-overlap (bbox1 bbox2)
  "Check if two bounding boxes overlap"
  (and
    (< (car (car bbox1)) (car (cadr bbox2)))
    (> (car (cadr bbox1)) (car (car bbox2)))
    (< (cadr (car bbox1)) (cadr (cadr bbox2)))
    (> (cadr (cadr bbox1)) (cadr (car bbox2)))
  )
)

(defun has-overlaps (pos label-data mtextData)
  "Check if a label at given position overlaps with other labels"
  (setq test-entdata (cadr label-data))
  (setq test-entdata (subst (cons 10 pos) (assoc 10 test-entdata) test-entdata))
  (setq test-bbox (get-expanded-bbox test-entdata))
  
  (foreach other-data mtextData
    (if (and (not (eq (car label-data) (car other-data)))
             (bbox-overlap test-bbox (get-expanded-bbox (cadr other-data))))
      (return t)
    )
  )
  nil
)

;; Position Finding Functions

(defun find-nearest-position (label-data mtextData bbox / current-pos original-pos width height step-size)
  "Find nearest non-overlapping position using spiral search"
  (setq current-pos (cdr (assoc 10 (cadr label-data)))
        original-pos current-pos
        width (cdr (assoc 41 (cadr label-data)))
        height (cdr (assoc 43 (cadr label-data)))
        step-size (min width height 8.0))
  
  (setq found nil
        attempt 0)
  
  (while (and (not found) (< attempt *MAX-ATTEMPTS*))
    (setq angle (* attempt (/ (* 2 pi) 32.0)))
    (setq radius (* step-size (+ 1.0 (/ attempt 32.0))))
    
    (setq test-pos (list
      (+ (car current-pos) (* radius (cos angle)))
      (+ (cadr current-pos) (* radius (sin angle)))
      (caddr current-pos)
    ))
    
    (if (and (point-in-bbox test-pos bbox)
             (not (has-overlaps test-pos label-data mtextData))
             (has-minimum-distance test-pos label-data mtextData))
      (setq found test-pos))
    
    (setq attempt (1+ attempt))
  )
  
  found
)

;; Main Command Functions

(defun c:ACAD-MTEXT-GREEDY-PLACE (/ ss ent obj mtextData count bbox-ent bbox)
  "Main command for greedy label placement"
  (vl-load-com)
  
  ;; Get bounding box
  (princ "\nSelect the bounding box polyline: ")
  (if (setq bbox-ent (entsel))
    (progn
      (setq bbox-ent (car bbox-ent))
      (if (= (cdr (assoc 0 (entget bbox-ent))) "LWPOLYLINE")
        (setq bbox (get-bbox-from-polyline bbox-ent))
        (progn
          (princ "\nError: Please select a polyline.")
          (exit)
        )
      )
    )
    (progn
      (princ "\nNo bounding box selected.")
      (exit)
    )
  )
  
  ;; Process labels
  (if (setq ss (ssget "_X" '((0 . "MTEXT"))))
    (progn
      (setq mtextData '())
      
      ;; Collect MTEXT data
      (repeat (setq count (sslength ss))
        (setq ent (ssname ss (setq count (1- count))))
        (if (and ent (not (null ent)))
          (progn
            (setq entdata (entget ent))
            (if entdata
              (setq mtextData (cons (list 
                ent 
                entdata 
                (cdr (assoc 10 entdata)))
                mtextData))
            )
          )
        )
      )

      (print (strcat "\nProcessing " (itoa (length mtextData)) " MTEXT labels."))
      
      ;; Process multiple passes
      (repeat *MAX-PASSES*
        (setq sorted-labels (sort-labels-by-priority mtextData))
        (setq processed-count 0)
        
        ;; Process each label
        (foreach label-data sorted-labels
          (setq ent (car label-data)
                entdata (cadr label-data)
                current-pos (cdr (assoc 10 entdata))
                original-pos (caddr label-data))
          
          (setq overlap-count (count-label-overlaps label-data mtextData))
          
          (if (> overlap-count 0)
            (progn
              (setq new-pos (find-nearest-position label-data mtextData bbox))
              (if new-pos
                (progn
                  (command "._move" ent "" current-pos new-pos)
                  (setq processed-count (1+ processed-count))
                  (create-leader original-pos new-pos)
                  (princ (strcat "\nMoved label " (vl-princ-to-string ent) 
                                " to reduce " (itoa overlap-count) " overlaps"))
                  (setq new-entdata (subst (cons 10 new-pos) (assoc 10 entdata) entdata))
                  (setq mtextData (subst (list ent new-entdata original-pos) label-data mtextData))
                )
                (progn
                  (princ (strcat "\nCould not find non-overlapping position for label " 
                                (vl-princ-to-string ent)))
                  (setq new-pos current-pos)
                  (setq new-entdata entdata)
                  (setq mtextData (subst (list ent new-entdata original-pos) label-data mtextData))
                )
              )
            )
            (progn
              (setq new-pos current-pos)
              (setq new-entdata entdata)
              (setq mtextData (subst (list ent new-entdata original-pos) label-data mtextData))
            )
          )
        )
        
        ;; Check if all overlaps are resolved
        (setq total-overlaps 0)
        (foreach label-data mtextData
          (setq total-overlaps (+ total-overlaps (count-label-overlaps label-data mtextData)))
        )
        (if (= total-overlaps 0)
          (setq *MAX-PASSES* 0)
        )
      )

      (print (strcat "\nGreedy placement completed. Processed " 
                     (itoa processed-count) " labels."))
    )
    (print "\nNo MTEXT objects found in the drawing.")
  )
  (princ)
)

;; Load message
(princ "\nAutoCAD Label Organizer loaded. Type ACAD-MTEXT-GREEDY-PLACE to run.")
(princ) 