# ZSH Configuration

This is my configuration for **ZSH**.

Dependencies
-------------

- **ZSH**
  * The shell to use.

- **Oh-My-ZSH**
  * Used to easily manage plugins.

- **[Nerd Fonts](https://github.com/ryanoasis/nerd-fonts)**
  * Fonts that contain icons for various programming and development tools, enhancing the visual appearance of the terminal and making it easier to identify different types of files and commands.

- **[BAT](https://github.com/sharkdp/bat)**
  * A command to list files more effectively than `cat`.

- **[LSD](https://github.com/Peltoche/lsd)**
  * A command to list directories and files with colors.

## Installation

To install Oh-My-ZSH, run:

```bash
sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
```

Remove the existing .zshrc

```bash
rm .zshrc
```

Navigate to the dotfiles folder and install

```bash
cd ~/.dotfiles && stow zsh
```

Return to the home directory

```bash
cd ~
```

Manually Install Missing Plugins
--------------------------------

* zsh-autosuggestions
    ```bash
    git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions
    ```

* zsh-history-substring-search
    ```bash
    git clone https://github.com/zsh-users/zsh-history-substring-search ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-history-substring-search
    ```

* zsh-syntax-highlighting
    ```bash
    git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting
    ```