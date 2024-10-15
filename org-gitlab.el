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

(defvar org-gitlab-bind-and-update t
  "when non NIL, update issue after successful bind")

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
          (if (and (eq (org-element-type headline) 'headline) (eq (org-outline-level) 1)) headline
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
      (insert "\n") (goto-char 0))
    (org-entry-put 0 org-gitlab-property-pid pid))
  pid)

;; callback helper

(defun org-gitlab--update-project-info (data)
  (org-gitlab-set-project-id (alist-get 'id data))
  (org-entry-put 0 "GITLAB_PROJECT_NAME" (alist-get 'name data))
  (org-entry-put 0 "GITLAB_PROJECT_REPO_URL" (alist-get 'ssh_url_to_repo data))
  (org-entry-put 0 "GITLAB_PROJECT_WEB_URL" (alist-get 'web_url data)))

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
    (when (org-gitlab--goto-title)
      (org-entry-put (org-element-at-point) org-gitlab-property-web-url url)))
  url)

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
          (or (previous-line)
              (delete-region
               (point) (org-element-property :end (org-element-at-point))))
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

;; callback helpers

(defun org-gitlab--set-ids (pid iid)
  "Set ids as properties: PID, IID"
  (when-let ((title-begin (org-gitlab--get-title-begin)))
    (org-entry-put title-begin org-gitlab-property-pid pid)
    (org-entry-put title-begin org-gitlab-property-iid iid)))

(defun org-gitlab--update-issue (data)
  (org-gitlab-set-title (alist-get 'title data))
  (org-gitlab-set-description (alist-get 'description data))
  (org-gitlab-set-web-url (alist-get 'web_url data))
  (org-gitlab-set-effort
   (/ (alist-get 'time_estimate (alist-get 'time_stats data)) 60)))

;;; queries

(defun org-gitlab--get-api-url ()
  "get issue API url"
  (let ((title-begin (org-gitlab--get-title-begin)))
    (when title-begin
      (let ((pid (org-gitlab-get-project-id))
            (iid (org-gitlab-get-iid)))
        (when (and pid iid)
	  (format "%s/projects/%s/issues/%s"
                  org-gitlab-base-url pid iid))))))

(defun org-gitlab-project-search (name)
  "Search project by name, update if found only one"
  (interactive "sname: ")
  (request (format "/projects?&search=%s&in=name"
                   org-gitlab-base-url (url-encode-url name))
    :headers (list (cons "PRIVATE-TOKEN" org-gitlab-token))
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
    (request (format "%s/projects/%s" org-gitlab-base-url pid)
      :headers (list (cons "PRIVATE-TOKEN" org-gitlab-token))
      :parser 'json-read
      :success (cl-function
		(lambda (&key data &allow-other-keys)
		  (org-gitlab--update-project-info data))))))

(defun org-gitlab-pull ()
  "Get description and title from remote"
  (interactive)
  (when-let ((url (org-gitlab--get-api-url)))
    (request url
      :headers (list (cons "PRIVATE-TOKEN" org-gitlab-token))
      :parser 'json-read
      :success (cl-function
		(lambda (&key data &allow-other-keys)
                  (org-gitlab--update-issue data))))))

(defun org-gitlab-bind ()
  "Search by PID and IID, if PULL is not nil, pull details as well"
  (interactive)
  (when-let ((url (org-gitlab--get-api-url)))
    (request url
      :headers (list (cons "PRIVATE-TOKEN" org-gitlab-token))
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (let* ((pid (number-to-string (alist-get 'project_id data)))
                         (iid (number-to-string (alist-get 'iid data)))
                         (msg (format "Issue has bound successfully (pid: %s, iid: %s)" pid iid)))
                    (org-gitlab--set-ids pid iid)
                    (if org-gitlab-bind-and-update (org-gitlab--update-issue data)
                      (setq msg (concat msp " - no pull requested")))
                    (message msg)))))))

(defun org-gitlab-bind-by-title ()
  "Search by title and set ids if found only one"
  (interactive)
  (when-let* ((title (org-gitlab-get-title))
              (url (format "%s/issues?scope=assigned_to_me&search=%s&in=title"
                           org-gitlab-base-url (url-encode-url title))))
    (request url
      :headers (list (cons "PRIVATE-TOKEN" org-gitlab-token))
      :parser 'json-read
      :success (cl-function
		(lambda (&key data &allow-other-keys)
		  (if (length= data 1)
		      (org-gitlab-set-ids
		       (number-to-string (alist-get 'project_id (elt data 0)))
		       (number-to-string (alist-get 'iid (elt data 0))))
                    (if org-gitlab-bind-and-update
                        (org-gitlab--update-issue data)
                      (message "No pull requested"))
		    (if (length= data 0)
			(message "Not found any issue")
		      (message "More then one issue found"))))))))

(defun org-gitlab-push ()
  "Push dscription and title to remote"
  (interactive)
  (when-let* ((url (org-gitlab--get-api-url))
	      (title (org-gitlab-get-title))
	      (description (org-gitlab-get-description))
	      (data (list (cons "title" title)
                          (cons "description" description))))
    (request url
      :type "PUT"
      :headers (list (cons "PRIVATE-TOKEN" org-gitlab-token)
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
		      (message (format "Estimated time: %s"
                                       (alist-get 'human_time_estimate data))))))
      (message "Effort estimate is not set"))))

(provide 'org-gitlab)
