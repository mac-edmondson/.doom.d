;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Macallyster Edmondson"
      user-mail-address "mac_edmondson@outlook.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-tokyo-night)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type `relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; STARTING HERE is my basic custom confiugration

;; Change the splash screen to only the best logo
(setq fancy-splash-image (concat doom-user-dir "splash.svg"))

;; Change the default font size
(let ((font-size 10))
  ;; If the machine ID is defined, check its value.
  (when (boundp 'my-machine-id)
    (setq font-size
          (cl-case my-machine-id
            ('machineconst-id-macbook 20)
            ('machineconst-id-asus    30)
            (otherwise                font-size))))
  ;;Set the font only once using the selected size.
  (setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size font-size) 
        doom-variable-pitch-font (font-spec :family "Iosevka Aile" :size (+ font-size 2)) 
        doom-symbol-font (font-spec :family "Symbola" :size font-size))) ;; Fallback font for symbols

;; Define the python interpreter for 'org-mode'
(setq org-babel-python-command "python3")

;; Define some new evil commands
(after! evil
  (evil-ex-define-cmd "ele[vate]" #'+popup/raise))


;; Make a new function to org-capture at a point 'org-capture-at-point'
(defun org-capture-at-point ()
  "Insert an org-capture template at point."
  (interactive)
  (org-capture 0))

;; Define a few functions for some keymaps
(defun my/insert-newline-and-move ()
  "Inserts a newline at the current cursor, while keeping the cursor stationary"
  (interactive)
  (insert ?\n)
  (evil-previous-line)
  (evil-end-of-line)
  (append ?\ ))

(defun my/comment-or-uncomment-line-or-region ()
  "Comments or uncomments the current line or region."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(after! evil
  ;; Make it so 'q' doesn't just kill emacs, but instead just closes the current
  ;; buffer
  (evil-ex-define-cmd "q" #'kill-current-buffer)
  (evil-ex-define-cmd "wq" #'doom/save-and-kill-buffer)
  ;; Re-map the 'help-map'
  (map! :leader :desc "help" "H" help-map)
  ;; Swap the position of the dired and the debug bindings in the open menu
  (map! :leader :prefix "o" 
        (:desc "Dired" "d" #'dired-jump)
        (:desc "Start a debugger" "-" #'+debugger/start))
  ;; Make some 'extras' keymaps
  (map! :leader :prefix-map ("e" . "extras")
        (:desc "Fireplace" "f" #'fireplace))
  ;; Make a new binding to search for all files in a project or at the current directory if not in a project
  (map! :leader
        (:prefix-map ("f" . "file")
         :desc "Find all files" "a" #'consult-fd))
  ;; Re-map leader x to open the scratch buffer in a new window, not just just
  ;; as a popup
  ;; TODO (map! :leader :desc "Open scratch buffer in new winodow" "x" #'+popup/raise)
  ;; Add some custom keybindings
  (map! :leader :n 
        "l" #'end-of-line
        "h" #'beginning-of-line)
  (map! :n 
        "K" #'my/insert-newline-and-move
        "C-/" #'my/comment-or-uncomment-line-or-region )
  ;; Map a spell-check correction to be more like what I'm used to
  (map! :n "C-." #'+spell/correct)
  ;; Make some "g" key bindings
  (map! :prefix "g" 
        :n "h" #'+lookup/documentation))

;; Below is copied and modified from
;; ~/.emacs.d.doom/modules/config/default/config.el to ensure the 'which-key'
;; descriptions are correct for the re-mapped help menu.
(after! which-key
  (let ((prefix-re (regexp-opt (list doom-leader-key doom-leader-alt-key))))
    (cl-pushnew `((,(format "\\`\\(?:<\\(?:\\(?:f1\\|help\\)>\\)\\|C-h\\|%s H\\) d\\'" prefix-re))
                  nil . "doom")
                which-key-replacement-alist)
    (cl-pushnew `((,(format "\\`\\(?:<\\(?:\\(?:f1\\|help\\)>\\)\\|C-h\\|%s H\\) r\\'" prefix-re))
                  nil . "reload")
                which-key-replacement-alist)
    (cl-pushnew `((,(format "\\`\\(?:<\\(?:\\(?:f1\\|help\\)>\\)\\|C-h\\|%s H\\) b\\'" prefix-re))
                  nil . "bindings")
                which-key-replacement-alist)))

;; Setup projectile's indexing method use a combination of tools like git and projectiles built-in method
(setq projectile-indexing-method 'hybrid)

;; Make sure 'consult-fd' can see ignored files (e.g. files in .gitignore)
;; https://github.com/sharkdp/fd/issues/839
(after! consult
  (setq consult-fd-args
        `(,(if (executable-find "fdfind") "fdfind" "fd")
          "--type" "f"
          "--color=never"
          "--absolute-path"
          "--no-ignore"      ;; Don't respect .gitignore
          "--hidden"         ;; Search hidden files
          "--exclude" ".git" ;; Exclude the .git directory itself
          )))

;; Setup org-roam
(use-package! org-roam
  :custom
  (org-roam-directory (file-truename "~/org/org-roam"))
  :config
  (defvar org-roam-templates-directory (concat org-roam-directory "/templates")))


(after! org-roam
  (let ((default-file-name "${slug}.org")
        (default-header-template "#+title: ${title}\n#+date: %U\n"))
    (setq org-roam-capture-templates 
          `(("d" "default" plain 
             "%?" 
             :target (file+head ,default-file-name ,default-header-template)
             :unnarrowed t)
            ("n" "Basic Note Template" plain 
             (file ,(concat org-roam-templates-directory "/basic-note.org"))
             :target (file+head ,default-file-name ,default-header-template)
             :unnarrowed t)
            ("u" "Unix Command Template" plain 
             (file ,(concat org-roam-templates-directory "/unix-command.org"))
             :target (file+head ,default-file-name ,default-header-template)
             :unnarrowed t)
            ("i" "Index Template" plain 
             (file ,(concat org-roam-templates-directory "/index.org"))
             :target (file+head ,(concat "index/" default-file-name) ,default-header-template)
             :unnarrowed t))))) 

;; Setup org-roam-ui
(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam 
  ;; If you want the UI to run on boot, add this
  ;; :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))


;; Use the org-fragtog package for inline previews
;; TODO figure out why this doesn't work. I think you need compiled elisp support?
;; (use-package! org-fragtog
;;   :after org
;;   :hook (org-mode . org-fragtog)) ; this auto-enables it when you enter an org-buffer

;; Use the ultra-scroll package
(use-package! ultra-scroll
  :init
  (setq scroll-conservatively 3 ; or whatever value you prefer, since v0.4
        scroll-margin 0)        ; important: scroll-margin>0 not yet supported
  :config
  (ultra-scroll-mode 1))

;; Use VHDL ext
;; TODO Get flycheck working with a linter. Investigate the other commented out features.
(use-package! vhdl-ext
  :hook ((vhdl-mode . vhdl-ext-mode))
  :init
  ;; Can also be set through `M-x RET customize-group RET vhdl-ext':
  ;; Comment out/remove the ones you do not need
  (setq vhdl-ext-feature-list
        '(font-lock
          xref
          capf
          hierarchy
          eglot
          lsp
          ;; lsp-bridge
          ;; lspce
          ;; flycheck
          beautify
          navigation
          template
          compilation
          imenu
          which-func
          hideshow
          ;; time-stamp
          ports))
  :config
  (vhdl-ext-mode-setup))

;; TODO Consider adding below to align all org-mode tables when an org file is
;; opened. Need to figure out if an !after should be used with this?
;; (setq org-startup-align-all-tables t)

;; ~/.doom.d/config.el
(use-package! org-ics-import
  :custom
  ;; Set the update interval to once an hour
  (org-ics-import-update-interval 3600)
  ;; Grab the calendars we want
  (org-ics-import-calendars-alist '(("https://canvas.utn.de/feeds/calendars/user_d4RT3Yeyn7F1VRPgwrJjiX6MYzyiQ7O6KOo3w2sO.ics" . "~/org/calendars/canvas.org")))
  (org-ics-import-exclude-strings '("Cancelled"))
  (org-ics-import-exclude-passed-events nil))

;; Setup some org-mode stuff

;; Function definitions for use in org-mode configuration
(defun my/enable-visual-line-numbers ()
  (display-line-numbers-mode -1) ;; disable line numbers temporarily
  (setq-local display-line-numbers-type `visual) ;; for the buffer, make the line numbers visual
  (display-line-numbers-mode t)) ;; re-enable the display of line numbers

;; TODO this is getting to be a mess and could use some serious cleanup.
;; Probably need to move things into separate files.
(defun my/corfu-cleanup-once (&rest _)
  "Run cleanup tasks after Corfu closes."
  (modify-syntax-entry ?: ".")
  (advice-remove #'corfu--teardown #'my/corfu-cleanup))

(defun my/completion-at-point-with-temp-symbol-char ()
  "Run #'completion-at-point after temporarily making : a word character.  When
corfu closes, it will be restored to a punctuation character with the
'my/corfu-cleanup-once' function."
  (modify-syntax-entry ?: "_")
  (funcall #'completion-at-point)
  (advice-add #'corfu--teardown :after #'my/corfu-cleanup))

(defun my/org-trigger-dabbrev-for-latex-ref ()
  "Manually trigger 'dabbrev-completion' after 'eq:'."
  ;; Check if the last typed char was a colon
  (when (and (eq last-command-event ?:)
             ;; Check if text just before the colon is "eq"
             (looking-back "ref{\\w+:" nil))
    
    ;; If so, manually call the function you found.
    ;; Corfu will intercept this and show its popup.
    (my/completion-at-point-with-temp-symbol-char)))

(defun my/org-add-buffer-completions ()
  ;; Add dabbrev to the list of completion sources
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;; 2. Add our special trigger function to a local hook
  (add-hook 'post-self-insert-hook
            #'my/org-trigger-dabbrev-for-latex-ref
            nil ; hook-depth
            t   ; make hook buffer-local
            ))

;; Customize the doom modeline
(after! doom-modeline
  (setq doom-modeline-buffer-file-name-style 'file-name-with-project)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-percent-position '(0 ""))
  )

(defun my/pyvenv-activate-deactivate-hook()
  ;; Find the path of the python interpreter that is furthest up in the
  ;; "exec-path"
  (let ((p-int (executable-find "python3")))
    (setq python-shell-interpreter p-int)
    ;; Make org-babel use the venv
    (setq org-babel-python-command p-int)))

(after! pyvenv
  ;; Set the workon directory for pyvenv
  (setenv "WORKON_HOME" "~/.venv")
  ;; set some hooks to make pyvenv work as as it should with Doom's "eval". 
  (add-hook! 'pyvenv-post-activate-hooks :append #'my/pyvenv-activate-deactivate-hook)
  (add-hook! 'pyvenv-post-deactivate-hooks :append #'my/pyvenv-activate-deactivate-hook))

(after! python
  ;; On boot of emacs, find the python executable on the path
  ;; TODO make this specific to each machine? Or figure something else out.
  (setq python-interpreter "~/.venv/global/bin/python3")
  ;; Note: For python stuff to work without pain, ensure the executable needed
  ;; are on the path. 'doom doctor' will tell you if anything is missing.
  (setq lsp-pyright-langserver-command "basedpyright"))

(after! org
  ;; Make org-mode display line numbers as visual, which displays a relative number even for wrapped lines.
  (add-hook! 'org-mode-hook :append #'my/enable-visual-line-numbers)

  ;; Make org-mode use completions from the local buffer
  (add-hook! 'org-mode-hook :append #'my/org-add-buffer-completions))

;; Setup some stuff for Magit
(after! magit
  (setq git-commit-style-convention-checks
        (remove 'overlong-summary-line git-commit-style-convention-checks)))

;; Setup treemacs
(after! treemacs
  (treemacs-follow-mode 1))

;; Setup ox-latex stuff
(load! "./lisp/latex-classes.el")

;; Setup org-agenda stuff
(load! "./lisp/org-agenda-config.el")

;; Make some custom org-capture templates
(after! org
  (add-to-list
   'org-capture-templates
   '("c" "Class LU Entry" entry
     (here)
     "* [[%^{LU Link}][LU%^{LU Number} -- %^{LU Name}]]
** Learning Objectives
** Class Prep.
*** Checklist
*** Materials
*** Notes 
** Class Lecture
** Assignments
** Things to look into" :immediate-finish t)))

