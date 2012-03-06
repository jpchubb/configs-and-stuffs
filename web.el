(require 'org-publish)
(setq org-publish-project-alist
      '(("blog"
 :base-directory "~/Documents/blog/"
 :base-extension "org"
 :publishing-directory "~/dav/webpage/"
 :recursive t
 :publishing-function org-publish-org-to-html
 :headline-levels 4             ; Just the default for this project.
 :section-numbers nil
 :LaTeX-fragments t
 :author "Joshua Chubb"
 :email "jpc@null.net"
 :table-of-contents nil
 :auto-preamble t
 :style-include-scripts "~/Documents/blog/styles.css"
 )
	("org-static"
 :base-directory "~/Documents/blog/"
 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
 :publishing-directory "~/dav/webpage/"
 :recursive t
 :publishing-function org-publish-attachment
 )
	("org" :components ("org-notes" "org-static"))
))
