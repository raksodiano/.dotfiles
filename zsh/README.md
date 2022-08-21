Configuración de ZSH
======================

Mi configuración de **ZSH**.

Paquetes Requeridos
-----------------------
- ZSH
  * La SHELL a usar

- Oh-My-ZSH
  * Usado para gestionar facilmente los plugins

- Docker (Opcional)
  * Se pueden comentar o eliminar los plugins de docker si no se tiene instalado:
    + Docker
    + Docker Compose
    + Docker Machine

- [BAT](https://github.com/sharkdp/bat) (Opcional)
  * Comando para poder listar mucho mejor los archivos que cat

- [LSD](https://github.com/Peltoche/lsd) (Opcional)
  * Comando para listar carpetas y archivos con colores

Instalación
-----------

```bash
$ sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
```

Eliminar el .zshrc
```bash
$ rm .zshrc
```

Entramos en la carpeta de dotfiles e instalamos
```bash
$ cd ~/.dotfiles && stow zsh
```


Salimos al home
```bash
$ cd ~
```

Instalamos plugins faltantes de forma manual
-------------------------------------

* zsh-autosuggestions
```bash
$ git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions
```

* zsh-history-substring-search
```bash
$  git clone https://github.com/zsh-users/zsh-history-substring-search ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-history-substring-search
```

* zsh-syntax-highlighting
```bash
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting
```
