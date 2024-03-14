
(require 'ox-latex)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)
(setq org-export-with-sub-superscripts nil)
(setq org-latex-minted-options
      '(("frame" "none") ("linenos=false") ("xleftmargin=1.5cm") ("bgcolor=lightgray") ("breaklines=true") ("breakanywhere=true")))

(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))
(setq org-image-actual-width nil)
;;(setq org-latex-classes nil)
(setq org-latex-pdf-process
      '("xelatex -8bit -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -8bit -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -8bit -shell-escape -interaction nonstopmode -output-directory %o %f"))

(setq org-latex-with-hyperref t)
(setq org-src-fontify-natively 't)

(org-add-link-type
 "latex" nil
 (lambda (path desc format)
   (cond
    ((eq format 'html)
     (format "<span style=\"color:grey;\">%s</span>" desc))
    ((eq format 'latex)
     (format "\\%s{%s}" path desc)))))

(add-to-list 'org-latex-classes
             '("article"
               "\\documentclass[12pt, a4paper]{article}
\\usepackage{xeCJK}
\\usepackage{minted}
\\usepackage{fontspec}
\\setmainfont{AppleGothic}
\\setCJKmainfont{AppleGothic}
\\setCJKsansfont{AppleGothic}
\\setCJKmonofont{AppleGothic}
%\\usepackage[dvipsnames]{xcolor}
%\\usepackage[margin=1.6cm]{geometry}
\\XeTeXlinebreaklocale ``zh''
\\XeTeXlinebreakskip = 0pt plus 1pt
%\\linespread{1.36}
\\usepackage{hyperref}
\\hypersetup{
  colorlinks=true, %把紅框框移掉改用字體顏色不同來顯示連結
  linkcolor=[rgb]{0,0.37,0.53},
  citecolor=[rgb]{0,0.47,0.68},
  filecolor=[rgb]{0,0.37,0.53},
  urlcolor=[rgb]{0,0.37,0.53},
  pagebackref=true,
  linktoc=all,}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


(setq org-beamer-outline-frame-title "目录")
(add-to-list 'org-latex-classes
             '("beamer"
               "\\documentclass[dvipdfmx,presentation]{beamer}
\\usepackage{xeCJK}
\\usepackage{fontspec}
\\setmainfont{AppleGothic}
\\setCJKmainfont{AppleGothic}
\\setCJKsansfont{AppleGothic}
\\setCJKmonofont{AppleGothic}
               [NO-DEFAULT-PACKAGES] [PACKAGES] [EXTRA]"
               ("\\section\{%s\}" . "\\section*\{%s\}")
               ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
               ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")))

(provide 'init-latex)
