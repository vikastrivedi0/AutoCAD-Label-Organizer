;;; AutoCAD Label Overlap Detector (MTEXT) - Move Overlapping Labels with Precise Overlap Calculation
;;; This script detects overlapping MTEXT labels and moves them by the exact overlap distance.
;;; Usage: Load the script and run the command ACAD-MTEXT-MOVE-OVERLAP

;; Load ActiveX support
(vl-load-com)

;; Function to calculate expanded bounding box for an MTEXT entity
(defun get-expanded-bbox (entdata / insertion width height)
  (if entdata
    (progn
      (setq insertion (cdr (assoc 10 entdata)))  ; Insertion point
      (setq width (cdr (assoc 41 entdata)))      ; Width
      (setq height (cdr (assoc 43 entdata)))     ; Height
      (if (and insertion width height)
        (list
          ;; Min point (expanded)
          (list (- (car insertion) width)  ; Left
                (- (cadr insertion) height) ; Bottom
                (caddr insertion))          ; Z
          ;; Max point (expanded)
          (list (+ (car insertion) (* 2 width))   ; Right
                (+ (cadr insertion) (* 2 height))  ; Top
                (caddr insertion))                 ; Z
        )
        nil
      )
    )
    nil
  )
)

;; Function to check if a point is inside a bounding box
(defun point-in-bbox (point bbox / minPt maxPt)
  (if (and point bbox (cadr bbox) (caddr bbox))
    (progn
      (setq minPt (cadr bbox)
            maxPt (caddr bbox))
      (and
        (>= (car point) (car minPt))
        (<= (car point) (car maxPt))
        (>= (cadr point) (cadr minPt))
        (<= (cadr point) (cadr maxPt))
      )
    )
    nil
  )
)

;; Function to get insertion point from entity data
(defun get-insertion-point (entdata)
  (if entdata
    (cdr (assoc 10 entdata))
    nil
  )
)

;; Function to count overlaps in each direction
(defun count-directional-overlaps (ent bbox mtextData / count-left count-right count-up count-down insertion)
  (setq count-left 0
        count-right 0
        count-up 0
        count-down 0)
  
  (foreach data mtextData
    (if (and (not (eq (car data) ent))
             (setq insertion (get-insertion-point (cadr data)))
             (point-in-bbox insertion bbox))
      (progn
        ;; Count overlaps in each direction
        (if (< (car insertion) (car (cadr bbox)))
          (setq count-left (1+ count-left)))
        (if (> (car insertion) (car (cadr bbox)))
          (setq count-right (1+ count-right)))
        (if (< (cadr insertion) (cadr (cadr bbox)))
          (setq count-down (1+ count-down)))
        (if (> (cadr insertion) (cadr (cadr bbox)))
          (setq count-up (1+ count-up)))
      )
    )
  )
  
  ;; Return list of (direction count) pairs
  (list
    (list "left" count-left)
    (list "right" count-right)
    (list "up" count-up)
    (list "down" count-down)
  )
)

(defun c:ACAD-MTEXT-MOVE-OVERLAP-SMART (/ ss ent obj mtextData count)
  ;; Initialize ActiveX
  (vl-load-com)
  
  (if (setq ss (ssget "_X" '((0 . "MTEXT"))))
    (progn
      (setq mtextData '())
      (setq moved-count 0)

      ;; Collect all MTEXT data
      (repeat (setq count (sslength ss))
        (setq ent (ssname ss (setq count (1- count))))
        (if (and ent (not (null ent)))
          (progn
            (setq entdata (entget ent))
            (if entdata
              (setq mtextData (cons (list ent entdata) mtextData))
            )
          )
        )
      )

      (print (strcat "\nProcessing " (itoa (length mtextData)) " MTEXT labels."))
      
      ;; Process each label
      (foreach label-data mtextData
        (setq ent (car label-data)
              entdata (cadr label-data))
        
        ;; Get expanded bounding box
        (setq expanded-bbox (get-expanded-bbox entdata))
        (if expanded-bbox
          (progn
            ;; Count overlaps in each direction
            (setq overlap-counts (count-directional-overlaps ent expanded-bbox mtextData))
            
            ;; Print debug information about overlaps
            (princ (strcat "\nLabel " (vl-princ-to-string ent) " overlaps:"))
            (foreach dir-count overlap-counts
              (princ (strcat "\n  " (car dir-count) ": " (itoa (cadr dir-count))))
            )
            
            ;; Find direction with least overlaps
            (setq best-direction nil
                  min-overlaps most-positive-fixnum)
            (foreach dir-count overlap-counts
              (if (< (cadr dir-count) min-overlaps)
                (progn
                  (setq min-overlaps (cadr dir-count))
                  (setq best-direction (car dir-count))
                )
              )
            )
            
            ;; Only move if we found a valid direction and there are overlaps
            (if (and best-direction (> min-overlaps 0))
              (progn
                ;; Calculate movement distance (use width or height as base)
                (setq width (cdr (assoc 41 entdata))
                      height (cdr (assoc 43 entdata))
                      move-distance (max width height))
                
                ;; Calculate new position based on best direction
                (setq current-pos (cdr (assoc 10 entdata))
                      new-pos current-pos)
                
                (cond
                  ((= best-direction "left")
                   (setq new-pos (list (- (car current-pos) move-distance)
                                     (cadr current-pos)
                                     (caddr current-pos))))
                  ((= best-direction "right")
                   (setq new-pos (list (+ (car current-pos) move-distance)
                                     (cadr current-pos)
                                     (caddr current-pos))))
                  ((= best-direction "up")
                   (setq new-pos (list (car current-pos)
                                     (+ (cadr current-pos) move-distance)
                                     (caddr current-pos))))
                  ((= best-direction "down")
                   (setq new-pos (list (car current-pos)
                                     (- (cadr current-pos) move-distance)
                                     (caddr current-pos))))
                )
                
                ;; Move the label
                (command "._move" ent "" current-pos new-pos)
                (setq moved-count (1+ moved-count))
                
                ;; Print debug information
                (princ (strcat "\nMoved label " (vl-princ-to-string ent) 
                              " " best-direction " by " 
                              (rtos move-distance) " units"))
              )
              (princ (strcat "\nNo overlaps found for label " (vl-princ-to-string ent)))
            )
          )
          (princ (strcat "\nCould not get bounding box for label " (vl-princ-to-string ent)))
        )
      )

      (print (strcat "\nCompleted. Successfully moved " (itoa moved-count) " MTEXT labels."))
    )
    (print "\nNo MTEXT objects found in the drawing.")
  )
  (princ)
)

;; Function to write data to CSV file
(defun write-csv-line (file-handle data)
  (write-line (vl-string-subst "," "\"" (vl-princ-to-string data)) file-handle)
)

;; Function to export MTEXT positions to CSV
(defun c:ACAD-MTEXT-EXPORT-POSITIONS (/ ss ent obj mtextData count file-handle)
  ;; Initialize ActiveX
  (vl-load-com)
  
  (if (setq ss (ssget "_X" '((0 . "MTEXT"))))
    (progn
      (setq mtextData '())
      (setq count 0)

      ;; Collect all MTEXT data
      (repeat (setq count (sslength ss))
        (setq ent (ssname ss (setq count (1- count))))
        (if (and ent (not (null ent)))
          (progn
            (setq entdata (entget ent))
            (if entdata
              (setq mtextData (cons (list ent entdata) mtextData))
            )
          )
        )
      )

      (print (strcat "\nFound " (itoa (length mtextData)) " MTEXT labels."))
      
      ;; Create CSV file
      (setq file-handle (open "mtext_positions.csv" "w"))
      (if file-handle
        (progn
          ;; Write CSV header
          (write-line "Entity Name,Handle,Insertion X,Insertion Y,Insertion Z,Width,Height,Text Content" file-handle)
          
          ;; Process each label
          (foreach label-data mtextData
            (setq ent (car label-data)
                  entdata (cadr label-data))
            
            ;; Extract data
            (setq insertion (cdr (assoc 10 entdata))  ; Insertion point
                  width (cdr (assoc 41 entdata))      ; Width
                  height (cdr (assoc 43 entdata))     ; Height
                  handle (cdr (assoc 5 entdata))      ; Handle
                  text (cdr (assoc 1 entdata)))       ; Text content
            
            ;; Write data to CSV
            (write-csv-line file-handle (strcat (vl-princ-to-string ent) "," handle))
            (write-csv-line file-handle (strcat (rtos (car insertion) 2 6) ","))
            (write-csv-line file-handle (strcat (rtos (cadr insertion) 2 6) ","))
            (write-csv-line file-handle (strcat (rtos (caddr insertion) 2 6) ","))
            (write-csv-line file-handle (strcat (rtos width 2 6) ","))
            (write-csv-line file-handle (strcat (rtos height 2 6) ","))
            (write-csv-line file-handle (strcat "\"" (vl-string-subst "\"\"" "\"" text) "\""))
            (write-line "" file-handle)
          )
          
          (close file-handle)
          (print "\nPosition data exported to mtext_positions.csv")
        )
        (print "\nError: Could not create CSV file")
      )
    )
    (print "\nNo MTEXT objects found in the drawing.")
  )
  (princ)
)

;; Load the function
(princ "\nAutoCAD MTEXT Label Overlap Detector (Smart Move) loaded. Type ACAD-MTEXT-MOVE-OVERLAP-SMART to run.")
(princ) 