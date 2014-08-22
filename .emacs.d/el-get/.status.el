((cl-lib status "installed" recipe
	 (:name cl-lib :builtin "24.3" :type elpa :description "Properly prefixed CL functions and macros" :url "http://elpa.gnu.org/packages/cl-lib.html"))
 (color-theme status "installed" recipe
	      (:name color-theme :description "An Emacs-Lisp package with more than 50 color themes for your use. For questions about color-theme" :type http-tar :options
		     ("xzf")
		     :url "http://download.savannah.gnu.org/releases/color-theme/color-theme-6.6.0.tar.gz" :load "color-theme.el" :features "color-theme" :post-init
		     (lambda nil
		       (color-theme-initialize)
		       (setq color-theme-is-global t))))
 (color-theme-ir-black status "required" recipe
		       (:name color-theme-ir-black :description "IR Black Color Theme for Emacs." :type git :url "https://github.com/burke/color-theme-ir-black.git" :depends color-theme))
 (color-theme-solarized status "required" recipe
			(:name color-theme-solarized :description "Emacs highlighting using Ethan Schoonover's Solarized color scheme" :type git :url "https://github.com/sellout/emacs-color-theme-solarized.git" :load "color-theme-solarized.el" :depends color-theme))
 (dired+ status "installed" recipe
	 (:name dired+ :description "Extensions to Dired" :type emacswiki :features dired+))
 (el-get status "installed" recipe
	 (:name el-get :website "https://github.com/dimitri/el-get#readme" :description "Manage the external elisp bits and pieces you depend upon." :type github :branch "4.stable" :pkgname "dimitri/el-get" :info "." :load "el-get.el"))
 (git-modes status "installed" recipe
	    (:name git-modes :description "GNU Emacs modes for various Git-related files" :type github :pkgname "magit/git-modes"))
 (htmlize status "installed" recipe
	  (:name htmlize :website "http://www.emacswiki.org/emacs/Htmlize" :description "Convert buffer text and decorations to HTML." :type http :url "http://fly.srk.fer.hr/~hniksic/emacs/htmlize.el.cgi" :localname "htmlize.el"))
 (magit status "installed" recipe
	(:name magit :website "https://github.com/magit/magit#readme" :description "It's Magit! An Emacs mode for Git." :type github :pkgname "magit/magit" :depends
	       (cl-lib git-modes)
	       :info "." :build
	       (if
		   (version<= "24.3" emacs-version)
		   `(("make" ,(format "EMACS=%s" el-get-emacs)
		      "all"))
		 `(("make" ,(format "EMACS=%s" el-get-emacs)
		    "docs")))
	       :build/berkeley-unix
	       (("touch" "`find . -name Makefile`")
		("gmake"))))
 (magithub status "required" recipe nil)
 (metaweblog status "installed" recipe
	     (:name metaweblog :description "Metaweblog" :type github :pkgname "punchagan/metaweblog"))
 (org2blog status "installed" recipe
	   (:name org2blog :description "Blog from Org mode to wordpress" :type github :pkgname "punchagan/org2blog" :depends
		  (xml-rpc-el metaweblog)
		  :features org2blog))
 (po-mode status "installed" recipe
	  (:name po-mode :description "Major mode for GNU gettext PO files" :type http :url "http://cvs.savannah.gnu.org/viewvc/*checkout*/gettext/gettext/gettext-tools/misc/po-mode.el" :features po-mode :post-init
		 (progn
		   (add-to-list 'auto-mode-alist
				'("\\.po$" . po-mode))
		   (add-to-list 'auto-mode-alist
				'("\\.pot$" . po-mode)))))
 (po-mode+ status "required" recipe
	   (:name po-mode+ :auto-generated t :type emacswiki :description "Extensions to GNU gettext's `po-mode.el'." :website "https://raw.github.com/emacsmirror/emacswiki.org/master/po-mode+.el"))
 (solarized-theme status "required" recipe
		  (:name solarized-theme :type github :pkgname "sellout/emacs-color-theme-solarized" :description "Solarized themes for Emacs" :prepare
			 (add-to-list 'custom-theme-load-path default-directory)))
 (xml-rpc-el status "installed" recipe
	     (:name xml-rpc-el :description "An elisp implementation of clientside XML-RPC" :type bzr :url "lp:xml-rpc-el"))
 (zenburn-theme status "installed" recipe
		(:name zenburn-theme :description "Zenburn theme for Emacs" :type http :url "https://raw.github.com/djcb/elisp/master/themes/zenburn-theme.el" :post-init
		       (add-to-list 'custom-theme-load-path default-directory))))
