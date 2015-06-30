#.emacs.d

##Table of Contents
- [Preface](https://github.com/manuel-uberti/.emacs.d#preface)
- [Overview](https://github.com/manuel-uberti/.emacs.d#overview)
- [Setup](https://github.com/manuel-uberti/.emacs.d#setup)
- [Updates](https://github.com/manuel-uberti/.emacs.d#updates)
- [Acknowledgements](https://github.com/manuel-uberti/.emacs.d#acknowledgements)

##Preface
This is the Emacs configuration I use everyday.

It requires **Emacs trunk** to work. I regularly update my sources from here:
```console
git://git.savannah.gnu.org/emacs.git
```

I use Emacs on [LinuxBBQ](http://linuxbbq.org/). The ```esetup``` script helps
to create the right environment *before* starting Emacs with this configuration
for the first time. The script only works with **Debian-based** systems.

I mainly use Emacs for **LaTeX**, **Elisp** and **Clojure**, so my setup is
planned accordingly.

This configuration comes with about **120 packages** carefully set up for my
daily usage. Check the ```.emacs.d/lisp``` directory for the gory details.

##Overview
- Theme: [Solarized Light](https://github.com/bbatsov/solarized-emacs)
- Default font:
[Source Code Pro](https://github.com/adobe-fonts/source-code-pro)
- Fonts utilities:
  [unicode-fonts](https://github.com/rolandwalker/unicode-fonts),
  [dynamic-fonts](https://github.com/rolandwalker/dynamic-fonts)
- Package management: [use-package](https://github.com/jwiegley/use-package),
  [Paradox](https://github.com/Bruce-Connor/paradox)
- Mode-line: [smart-mode-line](https://github.com/Bruce-Connor/smart-mode-line)
- Buffers selection and completion: [Helm](https://github.com/emacs-helm/helm),
  [ibuffer-vc](https://github.com/purcell/ibuffer-vc)
- Navigation: [Avy](https://github.com/abo-abo/avy),
[ace-window](https://github.com/abo-abo/ace-window),
[ace-link](https://github.com/abo-abo/ace-link)
- Editing: [Iedit](https://github.com/victorhge/iedit),
  [transpose-mark](https://github.com/AtticHacker/transpose-mark),
  [multiple-cursors](https://github.com/magnars/multiple-cursors.el)
- Undo: [Undo Tree](http://www.dr-qubit.org/emacs.php#undo-tree)
- Killing: [easy-kill](https://github.com/leoliu/easy-kill),
  [helm-show-kill-ring](https://tuhdo.github.io/helm-intro.html#sec-6)
- Coding: [Smartparens](https://github.com/Fuco1/smartparens),
[aggressive-indent-mode](https://github.com/Malabarba/aggressive-indent-mode),
[macrostep](https://github.com/joddie/macrostep)
- Filling:
  [aggressive-fill-paragraph](https://github.com/davidshepherd7/aggressive-fill-paragraph-mode),
  [visual-fill-column](https://github.com/joostkremers/visual-fill-column)
- Search: [helm-ag](https://github.com/syohex/emacs-helm-ag),
[helm-swoop](https://github.com/ShingoFukuyama/helm-swoop)
- Highlights: [rainbow-delimiters](https://github.com/jlr/rainbow-delimiters),
[highlight-symbol](https://github.com/nschum/highlight-symbol.el),
[highlight-numbers](https://github.com/Fanael/highlight-numbers)
- Org-mode: [org-bullets](https://github.com/sabof/org-bullets),
  [toc-org](https://github.com/snosov1/toc-org),
  [org2blog](https://github.com/punchagan/org2blog)
- Documents formatting:
  [pandoc-mode](https://github.com/joostkremers/pandoc-mode),
  [ox-pandoc](https://github.com/kawabata/ox-pandoc)
- LaTeX: [AUCTeX](http://www.gnu.org/software/auctex/index.html),
  [helm-bibtex](https://github.com/tmalsburg/helm-bibtex)
- Clojure: [CIDER](https://github.com/clojure-emacs/cider),
[flycheck-clojure](https://github.com/clojure-emacs/squiggly-clojure),
[clj-refactor](https://github.com/clojure-emacs/clj-refactor.el)
- Scheme: [Geiser](https://github.com/jaor/geiser)
- Web development: [web-mode](http://web-mode.org/),
  [js2-mode](https://github.com/mooz/js2-mode),
  [php-mode](https://github.com/ejmr/php-mode)
- Web: [SX](https://github.com/vermiculus/sx.el),
  [Elfeed](https://github.com/skeeto/elfeed)
- PDF: [PDF Tools](https://github.com/politza/pdf-tools),
  [interleave](https://github.com/rudolfochrist/interleave)
- Auto-completion: [company-mode](https://github.com/company-mode/company-mode),
  [company-statistics](https://github.com/company-mode/company-statistics),
  [helm-company](https://github.com/yasuyk/helm-company)
- Syntax checking: [Flycheck](https://github.com/flycheck/flycheck),
[helm-flycheck](https://github.com/yasuyk/helm-flycheck)
- Language tools: [Synosaurus](https://github.com/rootzlevel/synosaurus),
[helm-wordnet](https://github.com/raghavgautam/helm-wordnet)
- Version control: [Magit](https://github.com/magit/magit),
[diff-hl](https://github.com/dgutov/diff-hl)
- Project management: [Projectile](https://github.com/bbatsov/projectile),
  [helm-projectile](https://github.com/bbatsov/projectile/blob/master/helm-projectile.el)
- Slides: [Org-HTML-Slideshow](https://github.com/relevance/org-html-slideshow),
  [org-reveal](https://github.com/yjwen/org-reveal)
- Utilities: [The Bug Hunter](https://github.com/Malabarba/elisp-bug-hunter),
  [ESUP](https://github.com/jschaf/esup),
  [camcorder](https://github.com/Malabarba/camcorder.el)

##Setup
On your **Debian-based** machine:

- clone Emacs trunk:
```console
$ git clone git://git.savannah.gnu.org/emacs.git
```
- build Emacs trunk:
```console
$ cd emacs
$ sudo apt-get build-dep emacs24
$ ./configure
$ make
$ sudo make install
$ make clean
```
- clone this repo to your home directory:
```console
$ cd
$ git clone https://github.com/manuel-uberti/.emacs.d
```
- run ```esetup```:
```console
$ cd .emacs.d
$ chmod +x esetup
$ ./esetup
```
- run Emacs

##Updates
This configuration tracks latest Emacs developments. If you intend to use it, I highly recommend you update and re-build your sources at least once a week.

That is why if your build is more than seven days old, a warning will show up reminding you to update the sources.

You can use [Magit](https://github.com/magit/magit), which is bound to
```<f3>```. Or you can do it with the command line:
```console
$ cd emacs
$ git pull
```
Now you can build Emacs:
```console
$ ./configure
$ make
$ sudo make install
$ make clean
```
I would also recommend you upgrade every package installed regularly. You can easily do it with [Paradox](https://github.com/Bruce-Connor/paradox), which is bound to ```<f4>```.

If sources do not build correctly, or you find errors while using the latest commit, you can still revert to a working commit and re-build:
```console
$ git reset --hard <commit>
$ ./configure
$ make
$ sudo make install
$ make clean
```
With the help of tools such as [Magit](https://github.com/magit/magit),
[Paradox](https://github.com/Bruce-Connor/paradox) and your preferred shell,
maintenance is fairly simple.

##Acknowledgements
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
- [Wilfred Hughes](https://github.com/Wilfred)
