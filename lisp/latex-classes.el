;;; latex-classes.el -*- lexical-binding: t; -*-
(require 'ox-latex)
(add-to-list 'org-latex-classes  
             '("mimore"  
               "\\documentclass{mimore}  
[NO-DEFAULT-PACKAGES]  
[PACKAGES]  
[EXTRA]"  
               ("\\section{%s}" . "\\section\*{%s}")  
               ("\\subsection{%s}" . "\\subsection\*{%s}")  
               ("\\subsubsection{%s}" . "\\subsubsection\*{%s}")  
               ("\\paragraph{%s}" . "\\paragraph\*{%s}")  
               ("\\subparagraph{%s}" . "\\subparagraph\*{%s}")))

(setq org-latex-text-markup-alist 
      '((bold . "\\textbf{%s}") (code . protectedtexttt) (italic . "\\textit{%s}")
        (strike-through . "\\sout{%s}") (underline . "\\underline{%s}")
        (verbatim . protectedtexttt)))

(setq org-latex-subtitle-format "}\n\\subtitle{%s")

;; Setup minted for code block exporting in ox
(setq org-latex-listings 'minted)
(add-to-list 'org-latex-packages-alist '("" "minted"))

;; This tells the exporter to run pdflatex with the -shell-escape flag
(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; TODO consider adding the following to make the minted setup how I want it for all latex exporting
;; ;; 1. Define your custom color
;; (add-to-list 'org-latex-packages-alist '("definecolor{codebg}{rgb}{0.95,0.95,0.95}" "xcolor" nil))

;; ;; 2. Set the default minted options
;; (setq org-latex-minted-options
;;       '(("bgcolor" "codebg")
;;         ("frame" "lines")))
