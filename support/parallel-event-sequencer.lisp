(in-package :support)

;; Executes events in sequence based on a series of keys, supplying an ordered number to indicate the sequence of events
;; for other actors.

;; - This solution doesn't force a strict order of execution, but rather forces the order of execution of overlapping
;;   keys.
;; - The solution's sequence key does not indicate when an event happened but rather shows when it was greenlighted
;;   _and_ that events about the same triple will have a monotonic sequence key.
;; - The solution guarantees monotonicity of keys assuming some sensible defaults but does not guarantee ids rise one by one between restarts.

(defstruct ongoing-update
  key                 ; flight-check-sequence
  ;; used to notify one other when this got unlocked
  ;;
  ;; If this semaphore exists it means someone is waiting, when this is the case, you must follow next-update each time,
  ;; until you find one without a semaphore and register for that.  That creates a structure where we are only waiting
  ;; for what is necessary and will unblock requests as they arrive.  If/when it becomes possible to cancel requests, we
  ;; must rewrite the next-update and rely on signaling the sleeping thread instead.
  (finished-semaphore (bt:make-semaphore)) ; signaled when update has finished
  (lock (bt:make-lock))                    ; for manipulating this struct
  tokens                                   ; affected graph-subject-predicate, read-only in hash for fast checking
  (next-update nil))                       ; update which will be signaled to run after this update

(defun make-tokens (token-list)
  "Converts a list of tokens into an internal data structured used for more quickly comparing tokens"
  ;; we know this will only be checked through a single thread
  (let ((table (make-hash-table :test 'equal)))
    (dolist (token token-list)
      (setf (gethash token table) t))
    table))

(defun overlapping-tokens-p (left-tokens right-tokens)
  "Checks if a token in LEFT-TOKENS is also in RIGHT-TOKENS or vice-versa."
  (let* ((left-large-p (> (hash-table-size left-tokens) (hash-table-size right-tokens)))
         (a (if left-large-p right-tokens left-tokens))
         (b (if left-large-p left-tokens right-tokens)))
    (loop for k being the hash-keys of a
          when (gethash k b)
            return t)))

(defstruct parallel-event-sequencer
  (index 0)
  (ongoing-updates nil)
  (ongoing-updates-lock (bt:make-lock "event-sequencer")))



(defun end-update (sequencer update)
  "Finishes an update for the given index."
  (bt:with-lock-held ((parallel-event-sequencer-ongoing-updates-lock sequencer))
    (setf (parallel-event-sequencer-ongoing-updates sequencer)
          (delete-if (lambda (x) (eq update x))
                     (parallel-event-sequencer-ongoing-updates sequencer)
                     :count 1)))
  (bt:with-lock-held ((ongoing-update-lock update))
    (bt:signal-semaphore (ongoing-update-finished-semaphore update))))

(defun start-update (sequencer token-list)
  "Starts an update."
  ;; this code is more complex than what it should be because we have to ensure we lock correctly _and_ we want to clean
  ;; up our state to the best of our ability because a failure might bring the whole application down.
  (let* ((tokens (make-tokens token-list))
         (index (bt:with-lock-held ((parallel-event-sequencer-ongoing-updates-lock sequencer))
                  (incf (parallel-event-sequencer-index sequencer))))
         (new-update (make-ongoing-update :key index
                                          :finished-semaphore (bt:make-semaphore
                                                               :name (format nil "update-sem-~A" index)
                                                               :count 0)
                                          :lock (bt:make-lock (format nil "update-lock-~A" index))
                                          :tokens tokens
                                          :next-update nil)))
    ;; we have to search for each update which may depend on our data

    ;; Lock the list of current updates so we're the only ones updating it
    ;; - for each update which overlaps with us:
    ;;   - find the very latest in that path
    ;;   - if we did not encounter ourselves in this list
    ;;     - add ourselves to that item
    ;;     - add the semaphore to a list to wait for
    (let (semaphores updates complete-p)
      (unwind-protect
           (progn
             (bt:with-lock-held ((parallel-event-sequencer-ongoing-updates-lock sequencer))
               (dolist (previous-update (parallel-event-sequencer-ongoing-updates sequencer))
                 (when (overlapping-tokens-p (ongoing-update-tokens previous-update) tokens)
                   (loop for update-in-focus = previous-update then (ongoing-update-next-update update-in-focus)
                         when (eq update-in-focus new-update)
                           return nil ;; we already visited this update
                         unless (ongoing-update-next-update update-in-focus)
                           ;; no further update, we're next in line, just have to verify
                           return
                           (let ((inner-update-in-focus update-in-focus))
                             (loop
                               while inner-update-in-focus
                               do
                                  (bt:with-lock-held ((ongoing-update-lock inner-update-in-focus))
                                    (if (ongoing-update-next-update inner-update-in-focus)
                                        ;; was empty but now exists, let's move one further
                                        (setf inner-update-in-focus
                                              (ongoing-update-next-update inner-update-in-focus))
                                        (progn
                                          (setf (ongoing-update-next-update inner-update-in-focus)
                                                new-update) ; set update
                                          (push (ongoing-update-finished-semaphore update-in-focus)
                                                semaphores)                        ; set semaphore
                                          (push update-in-focus updates)           ; remember update for cleanup
                                          (setf inner-update-in-focus nil))))))))) ; stop loop
               ;; last step before waiting is adding ourselves

               ;; NOTE: This approach will crash when anything errors out in the
               ;; above code.  It may be worth extending such that an error above
               ;; will produce a failed check _after_ cleaning up its own state.
               (push new-update (parallel-event-sequencer-ongoing-updates sequencer)))
             ;; Why lock the list?  We will be registering ourselves.  If we
             ;; don't lock the list others may add themselves to the list.  We
             ;; may discover that they overlap with our content, but we might not
             ;; detect that they are dependent on us somewhere in a longer tree.
             ;; At that point we have a broken loop.  This could be resolved by
             ;; studying what we have.  Then too there might be a case where we
             ;; are depending on some semaphores of them and they may be waiting
             ;; on some semaphore of us, this is tricky to reason on so we choose
             ;; to keep it simpler as long as that doesn't turn out to be a
             ;; problem.

             ;; This approach with creating a long list may mean we wait longer
             ;; than we have to.  We join the dependency list if any of these
             ;; dependencies match our requirements even though the later items
             ;; in that list may have nothing to do with our request.  The
             ;; approach presented here should be fairly performant and should
             ;; be fairly easy to reason on when something would go haywire
             ;; hence we accept it for what it is.
             (dolist (semaphore semaphores)
               (bt:wait-on-semaphore semaphore))
             ;; everything necessary has ran, we can continue
             (setf complete-p t)
             new-update)
        ;; clean up when something went wrong in the process but keep nonlocal exit
        (unless complete-p
          (format t "~&Something went wrong when processing flight check.~%  token-list: ~{~A~,^ ~}~%" token-list)
          ;; we should not be in the list to check anymore
          (bt:with-lock-held ((parallel-event-sequencer-ongoing-updates-lock sequencer))
            (setf (parallel-event-sequencer-ongoing-updates sequencer)
                  (delete new-update
                      (parallel-event-sequencer-ongoing-updates sequencer)
                    :test #'eq)))
          ;; we should cut ourselves out of any item to which we added ourselves
          (loop for update in updates
                do (bt:with-lock-held ((ongoing-update-lock update))
                     (when (eq new-update (ongoing-update-next-update update))
                       ;; yup, we're the child so we cut ourselves from the list
                       (setf (ongoing-update-next-update update)
                             (ongoing-update-next-update new-update)))))
          ;; lastly we should signal our semaphore in case anyone chose to depend on us.  this could be the wrong
          ;; semaphore but we have no good place to listen as we already crashed.
          (bt:with-lock-held ((ongoing-update-lock new-update))
            (bt:signal-semaphore (ongoing-update-finished-semaphore new-update))))))))

(defmacro with-update-flight-check ((index-var) (sequencer tokens) &body body)
  "Executes a flight check for the supplied nested graph-combinations."
  (let ((ongoing-update-sym (gensym "this-ongoing-update"))
        (sequencer-sym (gensym "parallel-event-sequencer")))
    `(let* ((,sequencer-sym ,sequencer)
            (,ongoing-update-sym (start-update ,sequencer-sym ,tokens)))
       (unwind-protect
            (let* ((,index-var (bt:with-lock-held ((ongoing-update-lock ,ongoing-update-sym))
                             (ongoing-update-key ,ongoing-update-sym))))
              ,@body)
         (end-update ,sequencer-sym ,ongoing-update-sym)))))
