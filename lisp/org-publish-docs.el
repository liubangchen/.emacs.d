;;; -*- lexical-binding: t; -*-
(require 'ox-publish)
(setq org-html-coding-system 'utf-8-unix)

(setq org-publish-project-alist
      '(
        ("笔记"
         :base-directory "~/notes/org/notes/"
         :base-extension "org"
         :publishing-directory "~/notes/publish/笔记/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 2
         :auto-sitemap nil
         :sitemap-filename "sitemap.org"
         :sitemap-title "Sitemap"
         :makeindex nil
         :with-creator nil
         :with-email nil
         :with-author nil
         :with-date nil
         ;; :auto-preamble t
         ;; :auto-postamble t
         :html-pretable nil
         :html-postable nil
         :table-of-contents nil
         :section-numbers nil
         )

        ("工作"
         :base-directory "~/notes/org/工作/"
         :base-extension "org"
         :publishing-directory "~/notes/publish/工作/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 3
         :auto-sitemap nil
         :sitemap-filename "sitemap.org"
         :sitemap-title "Sitemap"
         :makeindex nil
         :with-creator nil
         :with-email nil
         :with-author nil
         :with-date nil
         ;; :auto-preamble t
         ;; :auto-postamble t
         :html-pretable nil
         :html-postable nil
         :table-of-contents nil
         :section-numbers t
         )

        ("生活"
         :base-directory "~/notes/org/生活/"
         :base-extension "org"
         :publishing-directory "~/notes/publish/生活/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 2
         :auto-sitemap nil
         :sitemap-filename "sitemap.org"
         :sitemap-title "Sitemap"
         :makeindex nil
         :with-creator nil
         :with-email nil
         :with-author nil
         :with-date nil
         ;; :auto-preamble t
         ;; :auto-postamble t
         :html-pretable nil
         :html-postable nil

         :table-of-contents nil
         :section-numbers nil
         )

        ("static-工作"
         :base-directory "~/notes/org/工作/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory "~/notes/publish/工作/"
         :recursive t
         :publishing-function org-publish-attachment
         )

        ("static-笔记"
         :base-directory "~/notes/org/notes/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory "~/notes/publish/notes/"
         :recursive t
         :publishing-function org-publish-attachment
         )

        ("static-生活"
         :base-directory "~/notes/org/生活/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory "~/notes/publish/生活/"
         :recursive t
         :publishing-function org-publish-attachment
         )

        ("工作ALL" :components ("工作" "static-工作"))
        ("笔记ALL" :components ("笔记" "static-笔记"))
        ("生活ALL" :components ("生活" "static-生活"))

        )
      )

(setq org-html-validation-link nil)
(provide 'org-publish-docs)
