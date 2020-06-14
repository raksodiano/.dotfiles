Configuración de Emacs
======================

Mi configuración de **GNU Emacs**.

Paquetes Requeridos
-----------------------
- Hunspell
  * Usado para los diccionarios

- Texlive y Auctex
  * Usado para la generación de archivos PDF's a través de org-mode y Latex

- PIP
  * Programación en Python

- SBCL
  * Usados para la programación en **Common Lisp**

Instalación
-----------

```bash
$ git clone https://github.com/raksodiano/Emacs.git ~/.emacs.d/

$ emacs --batch --eval='(load-file "~/.emacs.d/init.el")'
```

Para el uso de la agenda (opcional)
``` bash
$ cd ~/org && touch archive.org fromstuff.org fromwork.org inbox.org notes.org stuff.org work.org
```

Una vez iniciado **GNU Emacs**

``` common-lisp
M-x all-the-icons-install-fonts [RET]
```

Emacs como demonio (opcional)
-------------------------------------

``` bash
$ emacs --daemon
```
