;;; org-gitlab-hydra.el --- Hydra for Org mode - GitLab synchronisation

;; Copyright (C) 2023 Janos Gerzson

;; Author: Janos Gerzson <gerzsonj@gmail.com>
;; Version: 1.0
;; Package-Requires: ((org-gitlab "0.3") (hydra "20220910.1206"))
;; Keywords: orgmode
;; URL: https://github.com/grzs/emacs-org-gitlab

(require 'org-gitlab)
(require 'hydra)

(defhydra hydra-org-gitlab (:color pink :hint nil)
  "
^Project^      ^Issue^              ^Description^         ^Time^
^^^^^^^^---------------------------------------------------------------------------
_P_: search    _b_: bind by ID      _d_: start editing    _l_: log clock at point
_U_: update    _B_: bind by title   _x_: finish editing   _L_: log last clocked
^ ^            _f_: pull            ^ ^                   _e_: push effort estimate
^ ^            _p_: push

"
  ("P" org-gitlab-project-search)
  ("U" org-gitlab-project-info-update)
  ("b" org-gitlab-bind)
  ("B" org-gitlab-bind-by-title)
  ("f" org-gitlab-pull)
  ("p" org-gitlab-push)
  ("d" org-gitlab-edit-description)
  ("x" org-gitlab-widen)
  ("l" org-gitlab-log-at-point)
  ("L" org-gitlab-log-last-clocked)
  ("e" org-gitlab-estimate-push)
  ("c" nil "cancel"))

(define-key global-map (kbd "C-c g") 'hydra-org-gitlab/body)

(provide 'org-gitlab-hydra)
