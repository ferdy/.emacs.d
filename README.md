.emacs.d
=====

This is the Emacs configuration I use everyday.

It requires **Emacs trunk** to work. I regularly update my sources from here:
```console
$ git clone git://git.savannah.gnu.org/emacs.git
```

I use Emacs on **LinuxBBQ**. The ```esetup``` script helps to create the right
environment *before* starting Emacs with this configuration for the first
time. The script only works with **Debian-based** systems.

I mainly use Emacs for **LaTeX**, **Elisp** and **Clojure**, so my setup is
planned accordingly.

This configuration comes with more than **100 packages** carefully set up for my
daily usage. It may not be suitable for newbies. Check the ```.emacs.d/lisp``` directory for the gory details.

Overview
--------
- Theme: [Solarized Light](https://github.com/bbatsov/solarized-emacs)
- Font: [Source Code Pro](https://github.com/adobe-fonts/source-code-pro)
- Package management: [use-package](https://github.com/jwiegley/use-package), [Paradox](https://github.com/Bruce-Connor/paradox)
- Mode-line: [smart-mode-line](https://github.com/Bruce-Connor/smart-mode-line), [anzu](https://github.com/syohex/emacs-anzu)
- Buffers selection and completion: [Helm](https://github.com/emacs-helm/helm), [ibuffer-vc](https://github.com/purcell/ibuffer-vc)
- Navigation: [Avy](https://github.com/abo-abo/avy),
[ace-window](https://github.com/abo-abo/ace-window),
[ace-link](https://github.com/abo-abo/ace-link)
- Files and directories: [Dired+](http://www.emacswiki.org/emacs/DiredPlus), [Bookmark+](http://www.emacswiki.org/emacs/BookmarkPlus),
[ztree](https://github.com/fourier/ztree)
- Editing: [iedit](https://github.com/victorhge/iedit),
  [transpose-mark](https://github.com/AtticHacker/transpose-mark),
  [hungry-delete](https://github.com/nflath/hungry-delete),
  [multiple-cursors](https://github.com/magnars/multiple-cursors.el)
- Undo: [undo-tree](http://www.dr-qubit.org/emacs.php#undo-tree)
- Killing: [easy-kill](https://github.com/leoliu/easy-kill), [helm-show-kill-ring](https://tuhdo.github.io/helm-intro.html#sec-6)
- Coding: [Paredit](http://mumble.net/%7Ecampbell/emacs/paredit.html),
[Redshank](http://www.foldr.org/~michaelw/emacs/redshank/),
[aggressive-indent-mode](https://github.com/Malabarba/aggressive-indent-mode),
[macrostep](https://github.com/joddie/macrostep)
- Filling: [aggressive-fill-paragraph](https://github.com/davidshepherd7/aggressive-fill-paragraph-mode),
  [visual-fill-column](https://github.com/joostkremers/visual-fill-column)
- Search: [helm-ag](https://github.com/syohex/emacs-helm-ag),
[helm-swoop](https://github.com/ShingoFukuyama/helm-swoop)
- Highlights: [rainbow-delimiters](https://github.com/jlr/rainbow-delimiters),
[highlight-symbol](https://github.com/nschum/highlight-symbol.el)
- Terms: [multi-term](http://www.emacswiki.org/emacs/download/multi-term.el), [helm-mt](https://github.com/dfdeshom/helm-mt)
- LaTeX: [AUCTeX](http://www.gnu.org/software/auctex/index.html), [helm-bibtex](https://github.com/tmalsburg/helm-bibtex)
- Clojure: [CIDER](https://github.com/clojure-emacs/cider), [flycheck-clojure](https://github.com/clojure-emacs/squiggly-clojure)
- Web: [SX](https://github.com/vermiculus/sx.el),
  [Elfeed](https://github.com/skeeto/elfeed), [org2blog](https://github.com/punchagan/org2blog)
- PDF: [pdf-tools](https://github.com/politza/pdf-tools), [interleave](https://github.com/rudolfochrist/interleave)
- Auto-completion: [company-mode](https://github.com/company-mode/company-mode), [helm-company](https://github.com/yasuyk/helm-company)
- Syntax checking: [Flycheck](https://github.com/flycheck/flycheck),
[helm-flycheck](https://github.com/yasuyk/helm-flycheck)
- Language tools: [Synosaurus](https://github.com/rootzlevel/synosaurus),
  [langtool](https://github.com/mhayashi1120/Emacs-langtool), [voca-builder](https://github.com/yitang/voca-builder)
- Version control: [Magit](https://github.com/magit/magit),
[magit-gh-pulls](https://github.com/sigma/magit-gh-pulls),
[diff-hl](https://github.com/dgutov/diff-hl)
- Project management: [Projectile](https://github.com/bbatsov/projectile)
- Slides: [Org-HTML-Slideshow](https://github.com/relevance/org-html-slideshow)
- Document conversion: [pandoc-mode](https://github.com/joostkremers/pandoc-mode)
- Utilities: [bug-hunter](https://github.com/Malabarba/elisp-bug-hunter),
  [esup](https://github.com/jschaf/esup),
  [camcorder](https://github.com/Malabarba/camcorder.el), [unkillable-scratch](https://github.com/EricCrosson/unkillable-scratch)

Setup
-----
- clone Emacs trunk and build it
- run ```esetup``` script
- clone this repo in your home directory
- run Emacs

Acknowledgements
----------------
This configuration would not have been possible without the work of and the
inspiration from these people:
- [Mickey Petersen](https://github.com/mickeynp)
- [Sebastian Wiesner](https://github.com/lunaryorn)
- [Artur Malabarba](https://github.com/Bruce-Connor)
- [Sacha Chua](https://github.com/sachac)
- [John Wiegley](https://github.com/jwiegley)
- [Bozhidar Batsov](https://github.com/bbatsov)
- [Magnar Sveen](https://github.com/magnars)
- [Steve Purcell](https://github.com/purcell)
- [Oleh Krehel](https://github.com/abo-abo)
- [Joe Brock](https://github.com/DebianJoe)

Warranty
--------
This configuration is provided with no guarantee and no support.
