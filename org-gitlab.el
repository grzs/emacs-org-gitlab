;;; org-gitlab.el --- Org mode - GitLab synchronisation

;; Copyright (C) 2023 Janos Gerzson

;; Author: Janos Gerzson <gerzsonj@gmail.com>
;; Version: 0.3
;; Keywords: orgmode
;; URL: https://github.com/grzs/emacs-org-gitlab

;;; Commentary:

;; This package provides functions to synchronize an org-mode task
;; with a GitLab issue. Currently supported properties:
;; title, description, time_estimate, time_spent

(defvar org-gitlab-base-url nil "Gitlab server URL")
(defconst org-gitlab-token
  (alist-get 'org-sync-gitlab-auth-token safe-local-variable-values)
  "Get gitlab token from safe-local-variable-values")

(defconst org-gitlab-property-pid "GITLAB_PROJECT_ID"
  "Gitlab project ID")

(defconst org-gitlab-property-iid "GITLAB_ISSUE_ID"
  "Gitlab issue ID")

(defconst org-gitlab-property-web-url "GITLAB_WEB_URL"
  "Gitlab issue URL")

(defconst org-gitlab-description-block-header "#+NAME: GITLAB_DESCRIPTION")
(defconst org-gitlab-description-block-header-re "^#\\+NAME: GITLAB_DESCRIPTION$")
(defconst org-gitlab-description-block-begin "#+BEGIN_SRC markdown")
(defconst org-gitlab-description-block-end "#+END_SRC")

;;; helper functions

(defun org-gitlab--below-issue-p ()
  "check if currest position is below issue level (nat toplevel)"
  (> (org-outline-level) 0))

(defun org-gitlab--get-heading-title ()
  (car (last (org-heading-components) 2)))

(defun org-gitlab--get-title ()
  "get main headline"
  (when (org-gitlab--below-issue-p)
    (let ((headline (car (last (org-element-lineage (org-element-at-point)) 2))))
      (when (eq (org-element-type headline) 'headline) headline))))

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

(defun org-gitlab-get-project-id ()
  "get main property project ID"
  (cdr (car (org-entry-properties 0 org-gitlab-property-pid))))

(defun org-gitlab-set-project-id (pid)
  "set main property project ID"
  (when pid
    (if (numberp pid) (setq pid (number-to-string pid)))
    (save-excursion
      (goto-char 0)
      (when (org-at-heading-p)
	(insert "\n") (goto-char 0))
      (org-entry-put 0 org-gitlab-property-pid pid))))

(defun org-gitlab-update-project-info (data)
  (org-gitlab-set-project-id (alist-get 'id data))
  (org-entry-put 0 "GITLAB_PROJECT_NAME" (alist-get 'name data))
  (org-entry-put 0 "GITLAB_PROJECT_REPO_URL" (alist-get 'ssh_url_to_repo data))
  (org-entry-put 0 "GITLAB_PROJECT_WEB_URL" (alist-get 'web_url data))
  (message "Project info updated"))

;;; issue params

(defun org-gitlab-set-ids (pid iid)
  "Set ids as properties: PID, IID"
  (when-let ((title-begin (org-gitlab--get-title-begin)))
    (org-entry-put title-begin org-gitlab-property-pid pid)
    (org-entry-put title-begin org-gitlab-property-iid iid)
    (message (format "Issue has bound successfully (pid: %s, iid: %s" pid iid))))

(defun org-gitlab-get-title ()
  "get issue title"
  (save-excursion
    (when (org-gitlab--goto-title)
      (org-gitlab--get-heading-title))))

(defun org-gitlab-set-title (title)
  "set issue title"
  (save-excursion
    (when (org-gitlab--goto-title)
      (org-edit-headline title))))

(defun org-gitlab-set-web-url (url)
  "Set web URL as a property"
  (save-excursion
    (when (org-gitlab--goto-title)
      (org-entry-put (org-element-at-point) org-gitlab-property-web-url url))))

;; effort estimate

(defun org-gitlab-get-effort ()
  "get effort estimate in minutes"
  (when-let ((title-begin (org-gitlab--get-title-begin)))
    (when-let ((effort-string (org-entry-get title-begin org-effort-property)))
      (org-duration-string-to-minutes effort-string))))

(defun org-gitlab-set-effort (minutes)
  "set effort estimate to MINUTES"
  (if (numberp minutes) minutes
    (error "MINUTES has to be a number"))
  (save-excursion
    (org-gitlab--goto-title)
    (org-set-effort nil (org-duration-from-minutes minutes))))

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
          (or (previous-line)
              (delete-region (point) (org-element-property :end (org-element-at-point))))
        (org-end-of-meta-data))
      (insert (string-join
               (list
                org-gitlab-description-block-header
                org-gitlab-description-block-begin
                description
                org-gitlab-description-block-end
                "")
               "\n")))))

(defun org-gitlab-edit-description ()
  "Narrowing window to description and switch to markdown mode"
  (interactive)
  (when (org-gitlab--goto-description-block) (org-edit-src-code)))

;;; queries

(defun org-gitlab--get-api-url ()
  "get issue API url"
  (let ((title-begin (org-gitlab--get-title-begin)))
    (when title-begin
      (let ((pid (org-entry-get title-begin org-gitlab-property-pid))
            (iid (org-entry-get title-begin org-gitlab-property-iid)))
        (when (and pid iid)
	  (concat org-gitlab-base-url "/projects/" pid "/issues/" iid))))))

(defun org-gitlab-project-search (name)
  "Search project by name, update if found only one"
  (interactive "sname: ")
  (request (concat org-gitlab-base-url
		   "/projects?&search=" (url-encode-url name) "&in=name")
    :headers (list (cons "PRIVATE-TOKEN" org-gitlab-token))
    :parser 'json-read
    :success (cl-function
	      (lambda (&key data &allow-other-keys)
		(if (length= data 1)
		    (org-gitlab-update-project-info (elt data 0))
		  (if (length= data 0)
		      (message "Not found any project")
		    (message "More then one project found")))))))

(defun org-gitlab-project-info-update ()
  "Update project info as file properties"
  (interactive)
  (let ((pid (org-gitlab-get-project-id)))
    (unless pid
      (setq pid (read-string "pid: "))
      (org-gitlab-set-project-id pid))
    (save-excursion
      (goto-char 0)
      (when (org-at-heading-p)
	(insert "\n") (goto-char 0))
      (org-entry-put 0 org-gitlab-property-pid pid))
    (request (concat org-gitlab-base-url "/projects/" pid)
      :headers (list (cons "PRIVATE-TOKEN" org-gitlab-token))
      :parser 'json-read
      :success (cl-function
		(lambda (&key data &allow-other-keys)
		  (org-gitlab-update-project-info data))))))

(defun org-gitlab-pull ()
  "Get description and title from remote"
  (interactive)
  (let ((org-gitlab-url (org-gitlab--get-api-url)))
    (when org-gitlab-url
      (request org-gitlab-url
	:headers (list (cons "PRIVATE-TOKEN" org-gitlab-token))
	:parser 'json-read
	:success (cl-function
		  (lambda (&key data &allow-other-keys)
		    (org-gitlab-set-title (alist-get 'title data))
		    (org-gitlab-set-description (alist-get 'description data))
		    (org-gitlab-set-web-url (alist-get 'web_url data))
		    (org-gitlab-set-effort (/ (alist-get 'time_estimate (alist-get 'time_stats data)) 60))
		    (message "Pulled successfully")))))))

(defun org-gitlab-push ()
  "Push dscription and title to remote"
  (interactive)
  (let ((org-gitlab-url (org-gitlab--get-api-url))
	(title (org-gitlab-get-title))
	(description (org-gitlab-get-description))
	(data))
    (when org-gitlab-url
      (if title (add-to-list 'data (cons "title" title)))
      (if description (add-to-list 'data (cons "description" description)))
      (request org-gitlab-url
	:type "PUT"
	:headers (list (cons "PRIVATE-TOKEN" org-gitlab-token)
		       '("Content-Type" . "application/json"))
	:data (json-encode data)
	:parser 'json-read
	:success (cl-function
		  (lambda (&key data &allow-other-keys)
		    (message "Pushed successfully")))))))

(defun org-gitlab-bind (iid)
  "Search by PID and IID"
  (interactive "niid: ")
  (let ((pid (org-gitlab-get-project-id)))
    (unless pid
      (setq pid (read-string "pid: "))
      (org-gitlab-set-project-id pid))
    (request (concat org-gitlab-base-url (format "/projects/%s/issues/%d" pid iid))
      :headers (list (cons "PRIVATE-TOKEN" org-gitlab-token))
      :parser 'json-read
      :success (cl-function
		(lambda (&key data &allow-other-keys)
		  (org-gitlab-set-ids
		   (number-to-string (alist-get 'project_id data))
		   (number-to-string (alist-get 'iid data))))))))

(defun org-gitlab-bind-by-title ()
  "Search by title and set ids if found only one"
  (interactive)
  (save-excursion
    (let ((title (org-gitlab-get-title)))
      (when title
	(request (concat org-gitlab-base-url
			 "/issues?scope=assigned_to_me&search=" (url-encode-url title) "&in=title")
	  :headers (list (cons "PRIVATE-TOKEN" org-gitlab-token))
	  :parser 'json-read
	  :success (cl-function
		    (lambda (&key data &allow-other-keys)
		      (if (length= data 1)
			  (org-gitlab-set-ids
			   (number-to-string (alist-get 'project_id (elt data 0)))
			   (number-to-string (alist-get 'iid (elt data 0))))
			(if (length= data 0)
			    (message "Not found any issue")
			  (message "More then one issue found"))))))))))

;;; Logging time

(defun org-gitlab-log-at-point ()
  "Push duration at point, with closest heading as summary"
  (interactive)
  (if (org-at-clock-log-p)
      (if-let ((summary (org-gitlab--get-heading-title))
               (duration (org-element-property :duration (org-element-at-point))))
          (let ((org-gitlab-url (org-gitlab--get-api-url))
	        (data))
            (message (format "/spent %s hour(s) on: %s" duration summary))
            (add-to-list 'data (cons "summary" summary))
            (add-to-list 'data (cons "duration" (org-gitlab--parse-duration duration)))
            (request (concat org-gitlab-url "/add_spent_time")
              :type "POST"
              :headers (list (cons "PRIVATE-TOKEN" org-gitlab-token)
	                     '("Content-Type" . "application/json"))
              :data (json-encode data)
              :parser 'json-read
              :success (cl-function
	                (lambda (&key data &allow-other-keys)
	                  (message (format "Total logged: %s" (alist-get 'human_total_time_spent data)))))))
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
  (let ((org-gitlab-url (org-gitlab--get-api-url))
	(duration (org-gitlab-get-effort)))
    (if duration
	(request (concat org-gitlab-url "/time_estimate")
	  :type "POST"
	  :headers (list (cons "PRIVATE-TOKEN" org-gitlab-token)
			 '("Content-Type" . "application/json"))
	  :data (json-encode (list (cons "duration" (format "%dm" duration))))
	  :parser 'json-read
	  :success (cl-function
		    (lambda (&key data &allow-other-keys)
		      (message (format "Estimated time: %s" (alist-get 'human_time_estimate data))))))
      (message "Effort estimate is not set"))))

(provide 'org-gitlab)
