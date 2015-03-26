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
planned accordingly. Check the ```.emacs.d/lisp``` directory for the gory
details.

Noteworthy details
------------------
- Default theme: [Solarized Light](https://github.com/bbatsov/solarized-emacs)
- Default font: [Source Code Pro](https://github.com/adobe-fonts/source-code-pro)
- Package management: [use-package](https://github.com/jwiegley/use-package), [Paradox](https://github.com/Bruce-Connor/paradox)
- Mode-line: [smart-mode-line](https://github.com/Bruce-Connor/smart-mode-line)
- Buffers selection and completion: [Helm](https://github.com/emacs-helm/helm)
- LaTeX: [AUCTeX](http://www.gnu.org/software/auctex/index.html)
- Clojure: [CIDER](https://github.com/clojure-emacs/cider)
- PDF: [pdf-tools](https://github.com/politza/pdf-tools)
- Auto-completion: [company-mode](https://github.com/company-mode/company-mode)
- Syntax checking: [Flycheck](https://github.com/flycheck/flycheck)
- Language tools: [Synosaurus](https://github.com/rootzlevel/synosaurus), [langtool](https://github.com/mhayashi1120/Emacs-langtool)
- Version control: [Magit](https://github.com/magit/magit)
- Project management: [Projectile](https://github.com/bbatsov/projectile)

Setup
-----
- clone Emacs trunk and build it
- run ```esetup``` script
- run Emacs

Acknowledgements
----------------
This configuration would not have been possible without the work of and the
inspiration from these people:
- [Joe Brock](https://github.com/DebianJoe)
- [Mickey Petersen](https://github.com/mickeynp)
- [Sebastian Wiesner](https://github.com/lunaryorn)
- [Artur Malabarba](https://github.com/Bruce-Connor)
- [Sacha Chua](https://github.com/sachac)
- [Bozhidar Batsov](https://github.com/bbatsov)
- [Magnar Sveen](https://github.com/magnars)
- [Steve Purcell](https://github.com/purcell)
- [Oleh Krehel](https://github.com/abo-abo)
- [John Wiegley](https://github.com/jwiegley)

Warranty
--------
This configuration is provided with *absolutely no warranty*, *no guarantee* and
*no support*.
