;;; org-gitlab.el --- Org mode - GitLab synchronisation

;; Copyright (C) 2023 Janos Gerzson

;; Author: Janos Gerzson <gerzsonj@gmail.com>
;; Version: 0.8
;; Keywords: orgmode
;; URL: https://github.com/grzs/emacs-org-gitlab
;; Package-Requires: ((org "9.6.15"))

;;; Commentary:

;; This package provides functions to synchronize an org-mode task
;; with a GitLab issue. Currently supported properties:
;; title, description, time_estimate, time_spent

(defvar org-gitlab-url nil "Gitlab server URL")
(defvar org-gitlab-username nil "Gitlab user name")
(defconst org-gitlab-base-path "/api/v4")

(defconst org-gitlab-keyword-todo "#+TODO: TODO IN_PROGRESS | TO_REVIEW DONE"
  "Gitlab issue states")

(defconst org-gitlab-property-pid "GITLAB_PROJECT_ID"
  "Gitlab project ID")

(defconst org-gitlab-property-iid "GITLAB_ISSUE_ID"
  "Gitlab issue ID")

(defconst org-gitlab-property-assignee "GITLAB_ASSIGNEE"
  "Gitlab issue assignee")

(defconst org-gitlab-property-web-url "GITLAB_WEB_URL"
  "Gitlab issue URL")

(defconst org-gitlab-description-block-header "#+NAME: GITLAB_DESCRIPTION")
(defconst org-gitlab-description-block-header-re "^#\\+NAME: GITLAB_DESCRIPTION$")
(defconst org-gitlab-description-block-begin "#+BEGIN_SRC markdown")
(defconst org-gitlab-description-block-end "#+END_SRC")

(defvar org-gitlab-bind-and-update t
  "when non NIL, update issue after successful bind")

;;; dependencies
(require 'org)
(require 'org-element)
(require 'org-clock)
(require 'url)
(require 'request)
(require 'cl-extra)

;;; helper functions

(defun org-gitlab--below-issue-p ()
  "check if currest position is below issue level (nat toplevel)"
  (> (org-outline-level) 0))

(defun org-gitlab--get-heading-title ()
  (car (last (org-heading-components) 2)))

(defun org-gitlab--get-title ()
  "get issue headline"
  (when (org-gitlab--below-issue-p)
    (let ((headline (car (last (org-element-lineage (org-element-at-point)) 2))))
      (if (eq (org-element-type headline) 'headline) headline
        (let ((headline (org-element-at-point)))
          ;; last empty line after last headline
          (if (and (eq (org-element-type headline) 'headline) (eq (org-outline-level) 1))
              headline
            (error "Couldn't find title")))))))

(defun org-gitlab--get-title-begin ()
  "get current headline begin position"
  (when-let ((title (org-gitlab--get-title)))
    (org-element-property :begin title)))

(defun org-gitlab--goto-title ()
  "goto title"
  (when-let ((title-begin (org-gitlab--get-title-begin)))
    (goto-char title-begin)))

(defun org-gitlab--goto-description-block ()
  "go to beginning of description block"
  (when (org-gitlab--goto-title)
    (when (re-search-forward
           org-gitlab-description-block-header-re
           (save-excursion (org-get-next-sibling))
           t)
      (org-next-block 1))))

(defun org-gitlab--parse-duration (duration)
  (let ((hour-minute (split-string duration ":")))
    (format "%sh %sm" (car hour-minute) (cadr hour-minute))))

;;; project params

(defun org-gitlab-set-project-title (&optional title)
  (interactive)
  (unless title (setq title (read-string "title: ")))
  (unless (string-equal title (org-get-title))
    (save-excursion
      (goto-char (point-min)) (or (org-goto-first-child) (goto-char (point-max)))
      (search-backward-regexp "^#\\+TITLE:" nil t)
      (when (and (org-at-keyword-p)
                 (string-equal "TITLE" (org-element-property :key (org-element-at-point))))
        (delete-line)
        (while (progn
                 (unless (org-at-keyword-p) (delete-line))
                 (or (org-at-heading-p) (eq (point) (buffer-end 1))))))
      (insert (format "#+TITLE: %s" title))
      (newline))))

(defun org-gitlab--set-todo-keywords ()
  (save-excursion)
  (goto-char (point-min))
  (when (search-forward-regexp "#\\+TITLE:" nil t)
    (if (search-forward-regexp "#\\+TODO:" nil t) (delete-line)
      (forward-line))
    (insert org-gitlab-keyword-todo)
    (newline)))

(defun org-gitlab-get-project-id ()
  "get main property project ID"
  (if-let ((pid (cdr (car (org-entry-properties 0 org-gitlab-property-pid)))))
      pid
    (org-gitlab-set-project-id)))

(defun org-gitlab-set-project-id (&optional pid)
  "set main property project ID"
  (unless pid (setq pid (read-string "pid: ")))
  (if (numberp pid) (setq pid (number-to-string pid)))
  (save-excursion
    (goto-char 0)
    (when (org-at-heading-p)
      (newline) (goto-char 0))
    (org-entry-put 0 org-gitlab-property-pid pid))
  pid)

;; callback helper

(defun org-gitlab--update-project-info (data)
  (org-gitlab-set-project-id (alist-get 'id data))
  (org-entry-put 0 "GITLAB_PROJECT_NAME" (alist-get 'name data))
  (org-entry-put 0 "GITLAB_PROJECT_REPO_URL" (alist-get 'ssh_url_to_repo data))
  (org-entry-put 0 "GITLAB_PROJECT_WEB_URL" (alist-get 'web_url data))
  (org-gitlab-set-project-title (alist-get 'name data))
  (org-gitlab--set-todo-keywords))

;;; issue params

(defun org-gitlab-get-iid ()
  "get issue ID"
  (let ((title-begin (org-gitlab--get-title-begin)))
    (when title-begin
      (if-let ((iid (org-entry-get title-begin org-gitlab-property-iid))) iid
        (org-gitlab-set-iid)))))

(defun org-gitlab-set-iid (&optional iid)
  "set issue ID"
  (when-let ((title-begin (org-gitlab--get-title-begin)))
    (unless iid (setq iid (read-string "iid: ")))
    (org-entry-put title-begin org-gitlab-property-iid iid))
  iid)

(defun org-gitlab-get-title ()
  "get issue title"
  (save-excursion
    (when (org-gitlab--goto-title)
      (org-gitlab--get-heading-title))))

(defun org-gitlab-set-title (title)
  "set issue title"
  (save-excursion
    (when (org-gitlab--goto-title)
      (org-edit-headline title)))
  title)

(defun org-gitlab-set-web-url (url)
  "Set web URL as a property"
  (save-excursion
    (when-let (title-begin (org-gitlab--get-title-begin))
      (org-entry-put title-begin org-gitlab-property-web-url url)))
  url)

(defun org-gitlab-set-assignee (assignee)
  "Set assignee a property"
  (let ((assignee (if (stringp assignee) assignee org-gitlab-username)))
    (save-excursion
      (when-let (title-begin (org-gitlab--get-title-begin))
        (org-entry-put title-begin org-gitlab-property-assignee assignee)))
    assignee))

;; effort estimate

(defun org-gitlab-get-effort ()
  "get effort estimate in minutes"
  (when-let ((title-begin (org-gitlab--get-title-begin)))
    (when-let ((effort-string (org-entry-get title-begin org-effort-property)))
      (org-duration-to-minutes effort-string))))

(defun org-gitlab-set-effort (minutes)
  "set effort estimate to MINUTES"
  (if (numberp minutes) minutes
    (error "MINUTES has to be a number"))
  (save-excursion
    (let ((duration (org-duration-from-minutes minutes)))
      (org-gitlab--goto-title)
      (org-set-effort nil duration)
      duration)))

;; description block

(defun org-gitlab-get-description ()
  "get gitlab description"
  (save-excursion
    (when-let ((description-begin (org-gitlab--goto-description-block)))
      (org-element-property :value (org-element-at-point)))))

(defun org-gitlab-set-description (description)
  "set description to DESCRIPTION, if it's a string."
  (when (stringp description)
    (save-excursion
      (if (org-gitlab--goto-description-block)
          (progn (forward-line -1)
                 (delete-region
                  (point) (org-element-property :end (org-element-at-point))))
        (org-end-of-meta-data))
      (insert org-gitlab-description-block-header) (newline)
      (insert org-gitlab-description-block-begin) (newline)
      (insert description) (newline)
      (insert org-gitlab-description-block-end) (newline))))

(defun org-gitlab-edit-description ()
  "Narrowing window to description and switch to markdown mode"
  (interactive)
  (when (org-gitlab--goto-description-block) (org-edit-src-code)))

;; token helper
(defun org-gitlab--get-token ()
  (let* ((auth-source-creation-prompts
          '((secret . "Private Access Token for %u@%h: ")))
         (url-struct (url-generic-parse-url org-gitlab-url))
         (secret (car
                  (auth-source-search :host (cl-struct-slot-value 'url 'host url-struct)
                                      :port (cl-struct-slot-value 'url 'type url-struct)
                                      :user org-gitlab-username
                                      :max 1
                                      :require '(:secret)
                                      :create t
                                      :type 'netrc))))
    (unless secret
      (error "Couldn't find or ask for token ..."))

    ;; save token if necessary
    (when-let ((save-function (plist-get secret :save-function)))
      (funcall save-function))

    ;; return token
    (auth-info-password secret)))

;; url helper
(defun org-gitlab--compose-url (&optional path-list query-params-alist)
  (let* ((url-struct (url-generic-parse-url org-gitlab-url))
         (path (if (listp path-list)
                   (string-join
                    (append (string-split org-gitlab-base-path "/") path-list)
                    "/")
                org-gitlab-base-path))
         (query-string (cdr (url-path-and-query url-struct)))
         (query-params-alist-combined
          (if (eq 0 (length query-string)) query-params-alist
            (append (url-parse-query-string query-string) query-params-alist))))
    (when query-params-alist-combined
      (setq query-string (url-build-query-string query-params-alist-combined)))

    (setf (url-filename url-struct)
          (if (eq 0 (length query-string)) path
            (string-join (list path query-string) "?")))

    (url-recreate-url url-struct)))

(defun org-gitlab--get-issue-path ()
  "get issue API url"
  (let ((title-begin (org-gitlab--get-title-begin)))
    (when title-begin
      (let ((pid (org-gitlab-get-project-id))
            (iid (org-gitlab-get-iid)))
        (when (and pid iid)
	  (list "projects" pid "issues" iid))))))

;; callback helpers
(defun org-gitlab--set-ids (pid iid)
  "Set ids as properties: PID, IID"
  (when-let ((title-begin (org-gitlab--get-title-begin)))
    (org-entry-put title-begin org-gitlab-property-pid pid)
    (org-entry-put title-begin org-gitlab-property-iid iid)))

(defun org-gitlab--update-issue (data)
  (org-gitlab-set-title (alist-get 'title data))
  (org-gitlab-set-assignee (alist-get 'username (alist-get 'assignee data)))
  (org-gitlab-set-description (alist-get 'description data))
  (org-gitlab-set-web-url (alist-get 'web_url data))
  (org-gitlab-set-effort
   (/ (alist-get 'time_estimate (alist-get 'time_stats data)) 60)))

(defun org-gitlab--create-or-get-issue (iid data)
  (when (eq 0 (string-to-number iid))
    (error "issue id must be a string representing an integer")) 
 (save-excursion
    (if-let ((heading-pos (org-find-property org-gitlab-property-iid iid)))
        (goto-char heading-pos)
      (goto-char (point-max)) (end-of-line)
      (unless (eq (point) (save-excursion (beginning-of-line) (point))) (newline))
      (org-insert-heading)
      (org-entry-put (point) org-gitlab-property-iid iid))
    (when (alist-get 'title data)) (org-gitlab--update-issue data)))

(defun org-gitlab--update-issues (data)
  (let ((i 0))
    (while (< i (length data))
      (let* ((issue-data (aref data i))
             (iid (number-to-string (alist-get 'iid issue-data))))
        (org-gitlab--create-or-get-issue iid issue-data)
      (setq i (1+ i))))))

;;; queries
(defun org-gitlab-project-search (name)
  "Search project by name, update if found only one"
  (interactive "sname: ")
  (request (org-gitlab--compose-url
            '("projects")
            (list (list "search" (url-encode-url name)) '("in" "name")))
    :headers (list (cons "PRIVATE-TOKEN" (org-gitlab--get-token)))
    :parser 'json-read
    :success (cl-function
	      (lambda (&key data &allow-other-keys)
		(if (length= data 1)
		    (org-gitlab--update-project-info (elt data 0))
		  (if (length= data 0)
		      (message "Not found any project")
		    (message "More then one project found")))))))

(defun org-gitlab-project-info-update ()
  "Update project info as file properties"
  (interactive)
  (let ((pid (org-gitlab-get-project-id)))
    (request (org-gitlab--compose-url (list "projects" pid))
      :headers (list (cons "PRIVATE-TOKEN" (org-gitlab--get-token)))
      :parser 'json-read
      :success (cl-function
		(lambda (&key data &allow-other-keys)
		  (org-gitlab--update-project-info data))))))

(defun org-gitlab-update-all ()
  "Get issues assigned to user"
  (interactive)
  (request (org-gitlab--compose-url
            (list "projects" (org-gitlab-get-project-id) "issues")
            '(("scope" "assigned_to_me") ("state" "opened")))
    :headers (list (cons "PRIVATE-TOKEN" (org-gitlab--get-token)))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (org-gitlab--update-issues data)))))

(defun org-gitlab-pull ()
  "Update issue at point"
  (interactive)
  (when-let ((issue-path (org-gitlab--get-issue-path)))
    (request (org-gitlab--compose-url issue-path)
      :headers (list (cons "PRIVATE-TOKEN" (org-gitlab--get-token)))
      :parser 'json-read
      :success (cl-function
		(lambda (&key data &allow-other-keys)
                  (org-gitlab--update-issue data))))))

(defun org-gitlab-bind ()
  "Search by PID and IID, if PULL is not nil, pull details as well"
  (interactive)
  (when-let ((issue-path (org-gitlab--get-issue-path)))
    (request (org-gitlab--compose-url issue-path)
      :headers (list (cons "PRIVATE-TOKEN" (org-gitlab--get-token)))
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (let* ((pid (number-to-string (alist-get 'project_id data)))
                         (iid (number-to-string (alist-get 'iid data)))
                         (msg (format "Issue has bound successfully (pid: %s, iid: %s)"
                                      pid iid)))
                    (org-gitlab--set-ids pid iid)
                    (if org-gitlab-bind-and-update (org-gitlab--update-issue data)
                      (setq msg (concat msg " - no pull requested")))
                    (message msg)))))))

(defun org-gitlab-bind-by-title ()
  "Search by title and set ids if found only one"
  (interactive)
  (when-let ((title (org-gitlab-get-title)))
    (request (org-gitlab--compose-url
              '("issues")
              (list '("scope" "assigned_to_me")
                    (list "search" (url-encode-url title))))
      :headers (list (cons "PRIVATE-TOKEN" (org-gitlab--get-token)))
      :parser 'json-read
      :success (cl-function
		(lambda (&key data &allow-other-keys)
		  (if (length= data 1)
                      (let ((data0 (elt data 0)))
		        (org-gitlab--set-ids
		         (number-to-string (alist-get 'project_id data))
		         (number-to-string (alist-get 'iid data)))
                        (when org-gitlab-bind-and-update
                            (org-gitlab--update-issue data)))
		    (if (length= data 0)
			(message "Not found any issue")
		      (message "More then one issue found"))))))))

(defun org-gitlab-push ()
  "Push dscription and title to remote"
  (interactive)
  (when-let* ((issue-path (org-gitlab--get-issue-path))
	      (title (org-gitlab-get-title))
	      (description (org-gitlab-get-description))
	      (data (list (cons "title" title)
                          (cons "description" description))))
    (request (org-gitlab--compose-url issue-path)
      :type "PUT"
      :headers (list (cons "PRIVATE-TOKEN" (org-gitlab--get-token))
		     '("Content-Type" . "application/json"))
      :data (json-encode-alist data)
      :parser 'json-read
      :success (cl-function
		(lambda (&key data &allow-other-keys)
		  (message "Pushed successfully"))))))

;;; Logging time

(defun org-gitlab-log-at-point ()
  "Push duration at point, with closest heading as summary"
  (interactive)
  (if (org-at-clock-log-p)
      (if-let ((summary (org-gitlab--get-heading-title))
               (duration (org-element-property :duration (org-element-at-point))))
          (let ((path-list (append (org-gitlab--get-issue-path) '("add_spent_time")))
	        (data))
            (message (format "/spent %s hour(s) on: %s" duration summary))
            (add-to-list 'data (cons "summary" summary))
            (add-to-list 'data (cons "duration" (org-gitlab--parse-duration duration)))
            (request (org-gitlab--compose-url path-list)
              :type "POST"
              :headers (list (cons "PRIVATE-TOKEN" (org-gitlab--get-token))
	                     '("Content-Type" . "application/json"))
              :data (json-encode data)
              :parser 'json-read
              :success (cl-function
	                (lambda (&key data &allow-other-keys)
	                  (message (format "Total logged: %s"
                                           (alist-get 'human_total_time_spent data)))))))
	(message "It seems that clock is still running"))
    (message "Not at a clock line")))

(defun org-gitlab-log-last-clocked ()
  "Log last clocked duration as spent time on remote"
  (interactive)
  (org-check-running-clock)
  (when-let ((clock-marker (car org-clock-history)))
    ;; check if last clock was in the current buffer
    (let ((clock-buffer (marker-buffer clock-marker)))
      (if (not (eq clock-buffer (current-buffer)))
          (message (format "Last clock in other buffer: %s" clock-buffer))
        (save-excursion
          (goto-char clock-marker)
          (org-clock-find-position nil)
          (org-gitlab-log-at-point))))))

(defun org-gitlab-estimate-push ()
  "Push estimate"
  (interactive)
  (if-let ((duration (org-gitlab-get-effort)))
      (request (org-gitlab--compose-url
                (append (org-gitlab--get-issue-path) '("time_estimate")))
	:type "POST"
	:headers (list (cons "PRIVATE-TOKEN" (org-gitlab--get-token))
		       '("Content-Type" . "application/json"))
	:data (json-encode (list (cons "duration" (format "%dm" duration))))
	:parser 'json-read
	:success (cl-function
		  (lambda (&key data &allow-other-keys)
		    (message (format "Estimated time: %s"
                                     (alist-get 'human_time_estimate data))))))
    (message "Effort estimate is not set")))

(provide 'org-gitlab)
