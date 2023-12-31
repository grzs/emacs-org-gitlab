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

(defconst org-gitlab-description-header "#+NAME: GITLAB_DESCRIPTION")
(defconst org-gitlab-description-re "^#\\+NAME: GITLAB_DESCRIPTION$")

 (defun org-gitlab--get-project-id ()
  "get main property project ID"
  (save-excursion
    (goto-char 0)
    (unless (org-at-heading-p)
      (org-entry-get 0 org-gitlab-property-pid))))

(defun org-gitlab--set-project-id (pid)
  "set main property project ID"
  (when pid
    (if (numberp pid) (setq pid (number-to-string pid)))
    (save-excursion
      (goto-char 0)
      (when (org-at-heading-p)
	(insert "\n") (goto-char 0))
      (org-entry-put 0 org-gitlab-property-pid pid))))

(defun org-gitlab--get-headline-props ()
  "get main headline"
  ;; TODO: empty parapgraph
  (beginning-of-line)
  (let ((headline
	 (car (last (org-element-lineage (org-element-at-point)) 2))))
    (if (eq (org-element-type headline) 'headline)
	(cadr headline)
      (if (org-at-heading-p)
	  (cadr (org-element-headline-parser (point-at-eol)))))))

(defun org-gitlab-get-title ()
  "get title"
  (let ((headline-props (org-gitlab--get-headline-props)))
    (if headline-props (plist-get headline-props :raw-value))))

(defun org-gitlab-set-title (title)
  "set title to TITLE"
  (save-excursion
    (let ((headline-props (org-gitlab--get-headline-props)))
      (goto-char (plist-get headline-props :begin))
      (org-edit-headline title))))

(defun org-gitlab-set-web-url (url)
  "Set web URL as a property"
  (let ((headline-props (org-gitlab--get-headline-props)))
    (when headline-props
      (org-entry-put (plist-get headline-props :begin) "GITLAB_WEB_URL" url))))
  
(defun org-gitlab--get-description-src-block ()
  "get gitlab description source block element"
  (let ((headline-props (org-gitlab--get-headline-props)))
    (when headline-props
      (goto-char (plist-get headline-props :begin))
      (when (re-search-forward org-gitlab-description-re
			       (save-excursion (org-next-visible-heading 1) (point))
			       t)
	(forward-line)
	(org-element-src-block-parser (save-excursion (re-search-forward "#\\+END_SRC")) nil)))))

(defun org-gitlab-get-description ()
  "get gitlab description"
  (save-excursion
    (let ((description-element (org-gitlab--get-description-src-block)))
      (if description-element (org-element-property :value description-element)))))

(defun org-gitlab--create-description-header ()
  "Create description header"
  (let ((headline-props (org-gitlab--get-headline-props)))
    (when headline-props
      (goto-char (plist-get headline-props :begin))
      (forward-line)
      (if (org-at-property-drawer-p)
	  (goto-char (org-element-property :end (org-element-at-point))))
      (insert (concat org-gitlab-description-header "\n")))))

(defun org-gitlab-set-description (description)
  "set description to DESCRIPTION"
  (save-excursion
    (let ((description-element (org-gitlab--get-description-src-block)))
      (if description-element
	  (delete-region (point) (org-element-property :end description-element))
	(org-gitlab--create-description-header))
      (insert (concat "#+BEGIN_SRC markdown\n"
		      description "\n"
		      "#+END_SRC\n")))))

(defun org-gitlab--set-ids (pid iid)
  "Set ids as properties: PID, IID"
  (message pid iid)
  (let ((headline-pos (org-element-property :begin (org-gitlab--get-headline-props))))
    (org-entry-put headline-pos org-gitlab-property-pid pid)
    (org-entry-put headline-pos org-gitlab-property-iid iid)
    (message "Issue has bound successfully")))

(defun org-gitlab-get-url ()
  "get issue url"
  (let ((headline-pos) (headline-props (org-gitlab--get-headline-props)) (pid) (iid))
    (when headline-props
      (setq headline-pos (plist-get headline-props :begin))
      (setq pid (org-entry-get headline-pos org-gitlab-property-pid))
      (setq iid (org-entry-get headline-pos org-gitlab-property-iid))
      (if (and pid iid)
	  (concat org-gitlab-base-url "/projects/" pid "/issues/" iid)))))

(defun org-gitlab--update-project-info (data)
  (org-gitlab--set-project-id (alist-get 'id data))
  (org-entry-put 0 "GITLAB_PROJECT_NAME" (alist-get 'name data))
  (org-entry-put 0 "GITLAB_PROJECT_REPO_URL" (alist-get 'ssh_url_to_repo data))
  (org-entry-put 0 "GITLAB_PROJECT_WEB_URL" (alist-get 'web_url data))
  (message "Project info updated"))

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
		    (org-gitlab--update-project-info (elt data 0))
		  (if (length= data 0)
		      (message "Not found any project")
		    (message "More then one project found")))))))

(defun org-gitlab-project-info-update ()
  "Update project info as file properties"
  (interactive)
  (let ((pid (org-gitlab--get-project-id)))
    (unless pid
      (setq pid (read-string "pid: "))
      (org-gitlab--set-project-id pid))
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
		  (org-gitlab--update-project-info data))))))

(defun org-gitlab-pull ()
  "Get description and title from remote"
  (interactive)
  (let ((org-gitlab-url (org-gitlab-get-url)))
    (when org-gitlab-url
      (request org-gitlab-url
	:headers (list (cons "PRIVATE-TOKEN" org-gitlab-token))
	:parser 'json-read
	:success (cl-function
		  (lambda (&key data &allow-other-keys)
		    (org-gitlab-set-title (alist-get 'title data))
		    (org-gitlab-set-description (alist-get 'description data))
		    (org-gitlab-set-web-url (alist-get 'web_url data))
		    (org-gitlab--set-effort (/ (alist-get 'time_estimate (alist-get 'time_stats data)) 60))
		    (message "Pulled successfully")))))))

(defun org-gitlab-push ()
  "Push dscription and title to remote"
  (interactive)
  (let ((org-gitlab-url (org-gitlab-get-url))
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
  (let ((pid (org-gitlab--get-project-id)))
    (unless pid
      (setq pid (read-string "pid: "))
      (org-gitlab--set-project-id pid))
    (request (concat org-gitlab-base-url (format "/projects/%s/issues/%d" pid iid))
      :headers (list (cons "PRIVATE-TOKEN" org-gitlab-token))
      :parser 'json-read
      :success (cl-function
		(lambda (&key data &allow-other-keys)
		  (org-gitlab--set-ids
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
			  (org-gitlab--set-ids
			   (number-to-string (alist-get 'project_id (elt data 0)))
			   (number-to-string (alist-get 'iid (elt data 0))))
			(if (length= data 0)
			    (message "Not found any issue")
			  (message "More then one issue found"))))))))))

;; logging time
(defun org-gitlab--push-clocked(summary duration)
  "Push log data to remote"
  (let ((org-gitlab-url (org-gitlab-get-url))
	(data))
    (add-to-list 'data (cons "summary" summary))
    (add-to-list 'data (cons "duration" (org-gitlab--parse-duration duration)))
    (message (format "Logging time: %s - %s" summary duration))
    (request (concat org-gitlab-url "/add_spent_time")
      :type "POST"
      :headers (list (cons "PRIVATE-TOKEN" org-gitlab-token)
		     '("Content-Type" . "application/json"))
      :data (json-encode data)
      :parser 'json-read
      :success (cl-function
		(lambda (&key data &allow-other-keys)
		  (message (format "Total logged: %s" (alist-get 'human_total_time_spent data))))))))

(defun org-gitlab--parse-duration (duration)
  (let ((hour-minute (split-string duration ":")))
    (format "%sh %sm" (car hour-minute) (cadr hour-minute))))

(defun org-gitlab-log-last-clocked ()
  "Log last clocked duration as spent time on remote"
  (interactive)
  (let ((clock-marker (car org-clock-history))
	(clock-heading-props) (clock-props))
    (with-current-buffer (marker-buffer clock-marker)
      (goto-char clock-marker)
      (setq clock-heading-props (cadr (org-element-headline-parser (point-at-eol))))
      (when (re-search-forward "^CLOCK: .* =>  " nil t)
	(beginning-of-line)
	(setq clock-props (cadr (org-element-clock-parser (point-at-eol))))
	(org-gitlab--push-clocked
	 (plist-get clock-heading-props :raw-value)
	 (plist-get clock-props :duration))))))

(defun org-gitlab-log-at-point ()
  "Push duration at point"
  (interactive)
  (if (org-at-clock-log-p)
      (let ((clock-heading-props
	     (save-excursion (re-search-backward org-heading-regexp)
			     (cadr (org-element-headline-parser (point-at-eol)))))
	    (clock-props (cadr (org-element-clock-parser (point-at-eol)))))
	(if (plist-get clock-props :duration)
	    (org-gitlab--push-clocked
	     (plist-get clock-heading-props :raw-value)
	     (plist-get clock-props :duration))
	  (message "Clock is still running")))))

(defun org-gitlab--get-effort ()
  "get effort estimate in minutes"
  (let ((headline-props (org-gitlab--get-headline-props))
	(effort-string))
    (setq effort-string (org-entry-get (plist-get headline-props :begin) org-effort-property))
    (if effort-string
	(org-duration-string-to-minutes effort-string))))

(defun org-gitlab--set-effort (minutes)
  "set effort estimate to MINUTES"
  (if (numberp minutes) minutes
    (error "MINUTES has to be a number"))
  (save-excursion
    (goto-char (plist-get (org-gitlab--get-headline-props) :begin))
    (org-set-effort nil (org-duration-from-minutes minutes))))

(defun org-gitlab-estimate-push ()
  "Push estimate"
  (interactive)
  (let ((org-gitlab-url (org-gitlab-get-url))
	(duration (org-gitlab--get-effort)))
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

;; edit mode on off
(defun org-gitlab-edit-description ()
  "Narrowing window to description and switch to markdown mode"
  (interactive)
  (let ((src-block-start) (src-block-end)
	(src-block (org-gitlab--get-description-src-block)))
    (if (not src-block) (message "Couldn't find description source block"))
    (when src-block
      (org-show-entry)
      (if (org-check-for-hidden 'items) (org-show-block-all))
      (next-line) (beginning-of-line)
      (setq src-block-start (point))
      (save-excursion
	(goto-char (org-element-property :end src-block)) (previous-line) (beginning-of-line)
	(setq src-block-end (point)))
      (narrow-to-region src-block-start src-block-end)
      (markdown-mode))))

(defun org-gitlab-widen ()
  "Get back to normal mode"
  (interactive)
  (widen) (org-mode))

(provide 'org-gitlab)
