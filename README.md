<p align="center"><img src="assets/dotfiles-logo.svg" width=512></p>
<p align="center">
  <a href="https://www.gnu.org/software/stow/"><img src="https://img.shields.io/badge/GNU%20Stow-2.4.1-b48ead.svg?style=flat-square"/></a>
</p>
<p align="center">This repository contains all of my dotfiles configuration.</p>

---

### Dotfiles Manager ###

I'm using [GNU Stow](https://www.gnu.org/software/stow/) a free, lightweight
dotfiles manager written in Perl to manages my dotfiles.

What's make differentiates it from other dotfiles managers is that it does
not require various Python, Ruby or Perl dependencies like most dotfiles
manager.

With that, it is easy to share files among multiple users or computers with a
few command lines.

--------------------

### List of configurations: ###

```
 htop            ➔ interactive process viewer
 emacs           ➔ Extensible, powerfull, editor
 zsh             ➔ Customizable shell
 fish            ➔ User-friendly, modern shell
```

--------------------

### Getting Started ###

No matter what your Linux distribution, `stow` can easily be installed according
to your package manager:

- Arch Linux
```bash
  sudo pacman -S stow
```

- OpenSUSE
  ```bash
  sudo zypper in stow
  ```

- Fedora
  ```bash
  sudo dnf install stow
  ```

- Debian
  ```bash
  sudo apt-get install stow
  ```

Once the installation is complete, make a clone of the repository:

```bash
git clone https://github.com/raksodiano/.dotfiles.git
```

You can now install any configurations you wish to copy using GNU Stow:

```bash
# Make sure you are in the right directory
cd .dotfiles

# Example to install the htop config
stow htop

# Uninstall the htop config
stow -D htop
```
