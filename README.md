<p align="center"><img src="assets/dotfiles-logo.svg" width=512></p>
<p align="center">
<a href="https://getfedora.org//"><img src="https://img.shields.io/badge/Fedora-32.svg?style=flat-square"/></a>
  <a href="https://www.gnu.org/software/stow/"><img src="https://img.shields.io/badge/GNU%20Stow-2.3.1-b48ead.svg?style=flat-square"/></a>
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

### List of files: ###

```
 emacs           ➔ configuration for python, js, php, org, latex, etc.
 git             ➔ global git config and aliases
 htop            ➔ interactive process viewer
 systemd         ➔ systemd units
 tmux            ➔ terminal multiplexer
 xonsh           ➔ xonsh settings, aliases, and custom prompts
 zsh             ➔ zshell settings, aliases, and custom prompts
```

--------------------

### Getting Started ###

No matter what your Linux distribution, `stow` can easily be installed according
to your package manager:

	sudo dnf install stow
	sudo apt-get install stow

Once the installation is complete, make a clone of the repository:

    git clone https://github.com/raksodiano/.dotfiles.git

You can now install any configurations you wish to copy using GNU Stow:

```bash
# Make sure you are in the right directory
cd .dotfiles

# Example to install the htop config
stow htop

# Uninstall the htop config
stow -D htop
```
