emacs
=====

This is the Emacs configuration I use everyday.

It requires **Emacs trunk** to work. I regularly update my sources from here:
```console
$ git clone git://git.savannah.gnu.org/emacs.git
```

I use Emacs on **LinuxBBQ**. This configuration requires some external packages,
all of which are highlighted in the comments when necessary. The packages can be
easily installed using ```apt-get``` or built from sources.

The ```esetup``` script helps to create the right environment *before* starting
Emacs with this configuration for the first time. The script only works with
**Debian-based** systems.

I mainly use Emacs for **LaTeX**, **Elisp** and **Clojure**, so my setup is
planned accordingly.

This configuration comes with more than **100 packages** carefully set up for my
daily usage. It may not be suitable for newbies. Check the ```.emacs.d/lisp``` directory for the gory details.

Overview
--------
- Default theme: [Solarized Light](https://github.com/bbatsov/solarized-emacs)
- Default font: [Source Code Pro](https://github.com/adobe-fonts/source-code-pro)
- Package management: [use-package](https://github.com/jwiegley/use-package), [Paradox](https://github.com/Bruce-Connor/paradox)
- Mode-line: [smart-mode-line](https://github.com/Bruce-Connor/smart-mode-line), [anzu](https://github.com/syohex/emacs-anzu)
- Buffers selection and completion: [Helm](https://github.com/emacs-helm/helm)
- Files: [Dired+](http://www.emacswiki.org/emacs/DiredPlus), [Bookmark+](http://www.emacswiki.org/emacs/BookmarkPlus)
- Editing: [iedit](https://github.com/victorhge/iedit),
  [expand-region](https://github.com/magnars/expand-region.el),
  [transpose-mark](https://github.com/AtticHacker/transpose-mark),
  [hungry-delete](https://github.com/nflath/hungry-delete),
  [multiple-cursors](https://github.com/magnars/multiple-cursors.el)
- Undo: [undo-tree](http://www.dr-qubit.org/emacs.php#undo-tree)
- Killing: [easy-kill](https://github.com/leoliu/easy-kill), [helm-show-kill-ring](https://tuhdo.github.io/helm-intro.html#sec-6)
- Coding: [Paredit](http://mumble.net/%7Ecampbell/emacs/paredit.html),
[aggressive-indent-mode](https://github.com/Malabarba/aggressive-indent-mode),
[macrostep](https://github.com/joddie/macrostep)
- Filling: [aggressive-fill-paragraph](https://github.com/davidshepherd7/aggressive-fill-paragraph-mode),
  [visual-fill-column](https://github.com/joostkremers/visual-fill-column)
- Search: [helm-ag](https://github.com/syohex/emacs-helm-ag),
[helm-swoop](https://github.com/ShingoFukuyama/helm-swoop)
- Highlights: [rainbow-delimiters](https://github.com/jlr/rainbow-delimiters), [highlight-symbol](https://github.com/nschum/highlight-symbol.el)
- LaTeX: [AUCTeX](http://www.gnu.org/software/auctex/index.html)
- Clojure: [CIDER](https://github.com/clojure-emacs/cider)
- Web: [SX](https://github.com/vermiculus/sx.el), [Elfeed](https://github.com/skeeto/elfeed)
- Blogging: [org2blog](https://github.com/punchagan/org2blog)
- PDF: [pdf-tools](https://github.com/politza/pdf-tools), [interleave](https://github.com/rudolfochrist/interleave)
- Auto-completion: [company-mode](https://github.com/company-mode/company-mode), [helm-company](https://github.com/yasuyk/helm-company)
- Syntax checking: [Flycheck](https://github.com/flycheck/flycheck), [flycheck-pos-tip](https://github.com/flycheck/flycheck-pos-tip)
- Language tools: [Synosaurus](https://github.com/rootzlevel/synosaurus),
  [langtool](https://github.com/mhayashi1120/Emacs-langtool), [voca-builder](https://github.com/yitang/voca-builder)
- Version control: [Magit](https://github.com/magit/magit)
- Project management: [Projectile](https://github.com/bbatsov/projectile), [helm-projectile](https://tuhdo.github.io/helm-projectile.html)

Setup
-----
- clone Emacs trunk and build it
- run ```esetup``` script
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
This configuration is provided with *absolutely no warranty*, *no guarantee* and
*no support*.
