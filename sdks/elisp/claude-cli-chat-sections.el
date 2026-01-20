;;; claude-cli-chat-sections.el --- Magit-style sections for Claude Chat -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; This file provides magit-section based collapsible sections for the
;; Claude Chat interface.  Sections include turns, user messages,
;; assistant responses, thinking blocks, and tool calls.

;;; Code:

(require 'cl-lib)
(require 'magit-section)
(require 'claude-cli-chat-faces)

;;; Section Classes

(defclass claude-cli-chat-root-section (magit-section)
  ()
  "Root section for Claude chat buffer.")

(defclass claude-cli-chat-turn-section (magit-section)
  ((turn-number :initarg :turn-number
                :initform 0
                :documentation "The turn number."))
  "Section representing a conversation turn.")

(defclass claude-cli-chat-user-section (magit-section)
  ((user-content :initarg :user-content
                 :initform ""
                 :documentation "The user message content."))
  "Section for user messages.")

(defclass claude-cli-chat-assistant-section (magit-section)
  ((streaming :initarg :streaming
              :initform nil
              :documentation "Whether content is still streaming."))
  "Section for assistant responses.")

(defclass claude-cli-chat-thinking-section (magit-section)
  ()
  "Section for extended thinking, collapsed by default.")

(defclass claude-cli-chat-tool-section (magit-section)
  ((tool-id :initarg :tool-id
            :initform ""
            :documentation "Unique tool use ID.")
   (tool-name :initarg :tool-name
              :initform ""
              :documentation "Name of the tool.")
   (tool-status :initarg :tool-status
                :initform 'pending
                :documentation "Tool status: pending, complete, or error."))
  "Section for tool invocations.")

(defclass claude-cli-chat-tool-input-section (magit-section)
  ()
  "Section for tool input JSON.")

(defclass claude-cli-chat-tool-result-section (magit-section)
  ((is-error :initarg :is-error
             :initform nil
             :documentation "Whether the result is an error."))
  "Section for tool result.")

(defclass claude-cli-chat-error-section (magit-section)
  ((error-context :initarg :error-context
                  :initform nil
                  :documentation "Error context information."))
  "Section for error messages.")

;;; Section Insertion Functions

(defun claude-cli-chat-sections--insert-root ()
  "Insert root section for the buffer.
Returns the root section."
  (magit-insert-section (claude-cli-chat-root-section)
    (magit-insert-heading)))

(defun claude-cli-chat-sections--insert-turn (turn-number user-content)
  "Insert a turn section with TURN-NUMBER and USER-CONTENT.
Returns the turn section."
  (magit-insert-section turn-section (claude-cli-chat-turn-section
                                       :turn-number turn-number)
    ;; Turn heading
    (magit-insert-heading
      (propertize (format "Turn %d" turn-number)
                  'face 'claude-cli-chat-turn-number)
      " "
      (propertize (make-string 50 ?─) 'face 'claude-cli-chat-separator))
    ;; User subsection
    (claude-cli-chat-sections--insert-user user-content)
    ;; Return the section for later updates
    turn-section))

(defun claude-cli-chat-sections--insert-user (content)
  "Insert user message section with CONTENT."
  (magit-insert-section (claude-cli-chat-user-section :user-content content)
    (magit-insert-heading
      (propertize "You" 'face 'claude-cli-chat-user-label))
    (insert (propertize content 'face 'claude-cli-chat-user-content))
    (insert "\n\n")))

(defun claude-cli-chat-sections--insert-assistant-start ()
  "Insert start of assistant response section.
Returns the assistant section and a marker for content insertion."
  (let (section content-marker)
    (setq section
          (magit-insert-section sec (claude-cli-chat-assistant-section :streaming t)
            (magit-insert-heading
              (propertize "Claude" 'face 'claude-cli-chat-assistant-label))
            (setq content-marker (point-marker))
            (set-marker-insertion-type content-marker t)
            sec))
    (cons section content-marker)))

(defun claude-cli-chat-sections--append-assistant-text (marker text)
  "Append TEXT at MARKER position in assistant section."
  (when (and marker (marker-buffer marker))
    (save-excursion
      (goto-char marker)
      (insert (propertize text 'face 'claude-cli-chat-assistant-content)))))

(defun claude-cli-chat-sections--finalize-assistant (section)
  "Finalize assistant SECTION, marking streaming complete."
  (when section
    (oset section streaming nil)))

(defun claude-cli-chat-sections--insert-thinking (initial-text)
  "Insert thinking section with INITIAL-TEXT.
Returns the section and a marker for additional content."
  (let (section content-marker)
    (setq section
          (magit-insert-section sec (claude-cli-chat-thinking-section nil t) ; hidden by default
            (magit-insert-heading
              (propertize "Thinking" 'face 'claude-cli-chat-thinking-label)
              " "
              (propertize "(click to expand)" 'face 'claude-cli-chat-thinking-meta))
            (when initial-text
              (insert (propertize initial-text 'face 'claude-cli-chat-thinking-content)))
            (setq content-marker (point-marker))
            (set-marker-insertion-type content-marker t)
            (insert "\n")
            sec))
    (cons section content-marker)))

(defun claude-cli-chat-sections--append-thinking (marker text)
  "Append TEXT at MARKER in thinking section."
  (when (and marker (marker-buffer marker))
    (save-excursion
      (goto-char marker)
      (insert (propertize text 'face 'claude-cli-chat-thinking-content)))))

(defun claude-cli-chat-sections--insert-tool-start (tool-id tool-name)
  "Insert tool section for TOOL-ID with TOOL-NAME.
Returns the section and markers for input and result."
  (let (section input-marker result-marker)
    (setq section
          (magit-insert-section sec (claude-cli-chat-tool-section
                                      :tool-id tool-id
                                      :tool-name tool-name
                                      :tool-status 'pending)
            (magit-insert-heading
              (propertize "Tool: " 'face 'claude-cli-chat-tool-label)
              (propertize tool-name 'face 'claude-cli-chat-tool-name)
              " "
              (propertize "[Running...]" 'face 'claude-cli-chat-tool-pending))
            ;; Input subsection
            (magit-insert-section (claude-cli-chat-tool-input-section)
              (magit-insert-heading
                (propertize "Input" 'face 'claude-cli-chat-tool-sublabel))
              (setq input-marker (point-marker))
              (set-marker-insertion-type input-marker t)
              (insert "\n"))
            ;; Result subsection placeholder
            (magit-insert-section (claude-cli-chat-tool-result-section)
              (magit-insert-heading
                (propertize "Result" 'face 'claude-cli-chat-tool-sublabel))
              (setq result-marker (point-marker))
              (set-marker-insertion-type result-marker t)
              (insert "\n"))
            sec))
    (list section input-marker result-marker)))

(defun claude-cli-chat-sections--update-tool-input (marker json-input)
  "Update tool input at MARKER with JSON-INPUT."
  (when (and marker (marker-buffer marker))
    (save-excursion
      (goto-char marker)
      (insert (propertize (claude-cli-chat-sections--format-json json-input)
                          'face 'claude-cli-chat-tool-json)))))

(defun claude-cli-chat-sections--update-tool-result (marker result is-error)
  "Update tool result at MARKER with RESULT.
IS-ERROR indicates if the result is an error."
  (when (and marker (marker-buffer marker))
    (save-excursion
      (goto-char marker)
      (if is-error
          (insert (propertize (format "%s" result) 'face 'claude-cli-chat-tool-error))
        (insert (propertize (claude-cli-chat-sections--truncate-result result)
                            'face 'claude-cli-chat-assistant-content))))))

(defun claude-cli-chat-sections--update-tool-status (section status)
  "Update tool SECTION with STATUS (complete or error)."
  (when section
    (oset section tool-status status)
    ;; Update the heading - find and modify the status indicator
    (save-excursion
      (goto-char (oref section start))
      (when (re-search-forward "\\[Running\\.\\.\\.\\]\\|\\[Done\\]\\|\\[Error\\]"
                               (oref section end) t)
        (let ((inhibit-read-only t))
          (replace-match (pcase status
                           ('complete "[Done]")
                           ('error "[Error]")
                           (_ "[Running...]")))
          (put-text-property (match-beginning 0) (match-end 0)
                             'face (pcase status
                                     ('complete 'claude-cli-chat-tool-success)
                                     ('error 'claude-cli-chat-tool-error)
                                     (_ 'claude-cli-chat-tool-pending))))))))

(defun claude-cli-chat-sections--insert-error (error-msg &optional context)
  "Insert error section with ERROR-MSG and optional CONTEXT."
  (magit-insert-section (claude-cli-chat-error-section :error-context context)
    (magit-insert-heading
      (propertize "Error" 'face 'claude-cli-chat-error-label))
    (insert (propertize (format "%s" error-msg) 'face 'claude-cli-chat-error-content))
    (insert "\n")
    (when context
      (insert (propertize (format "Context: %s" context)
                          'face 'claude-cli-chat-error-context))
      (insert "\n"))
    (insert "\n")))

;;; Formatting Helpers

(defun claude-cli-chat-sections--format-json (data)
  "Format DATA as indented JSON for display."
  (if (null data)
      "{}"
    (condition-case nil
        (with-temp-buffer
          (insert (json-encode data))
          (json-pretty-print-buffer)
          (buffer-string))
      (error (format "%S" data)))))

(defun claude-cli-chat-sections--truncate-result (result &optional max-lines)
  "Truncate RESULT to MAX-LINES for display."
  (let* ((max-lines (or max-lines 20))
         (result-str (format "%s" result))
         (lines (split-string result-str "\n")))
    (if (> (length lines) max-lines)
        (concat (string-join (seq-take lines max-lines) "\n")
                (propertize (format "\n... (%d more lines)"
                                    (- (length lines) max-lines))
                            'face 'claude-cli-chat-truncated))
      result-str)))

;;; Section Queries

(defun claude-cli-chat-sections--find-turn-section (turn-number)
  "Find the turn section for TURN-NUMBER."
  (let ((root (magit-current-section)))
    ;; Navigate to root
    (while (and root (oref root parent))
      (setq root (oref root parent)))
    ;; Search for turn section
    (claude-cli-chat-sections--find-section-by-predicate
     root
     (lambda (section)
       (and (cl-typep section 'claude-cli-chat-turn-section)
            (= (oref section turn-number) turn-number))))))

(defun claude-cli-chat-sections--find-section-by-predicate (section predicate)
  "Recursively find a section under SECTION matching PREDICATE."
  (if (funcall predicate section)
      section
    (cl-loop for child in (oref section children)
             for result = (claude-cli-chat-sections--find-section-by-predicate child predicate)
             when result return result)))

(defun claude-cli-chat-sections--all-sections-of-type (type)
  "Return all sections of TYPE in the buffer."
  (let ((root (magit-current-section))
        (sections nil))
    (while (and root (oref root parent))
      (setq root (oref root parent)))
    (claude-cli-chat-sections--collect-sections root type sections)
    (nreverse sections)))

(defun claude-cli-chat-sections--collect-sections (section type acc)
  "Collect sections of TYPE under SECTION into ACC."
  (when (cl-typep section type)
    (push section acc))
  (dolist (child (oref section children))
    (claude-cli-chat-sections--collect-sections child type acc)))

(provide 'claude-cli-chat-sections)
;;; claude-cli-chat-sections.el ends here
