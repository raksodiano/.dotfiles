#!/bin/sh

source ./scripts/utils.sh

# Function to install emacs
install_emacs() {
  local packages=(
    "emacs"
    "sbcl"
    "nodejs"
    "npm"
    "graphviz"
    "imagemagick"
    "pandoc"
    "texlive-core"
    "texlive-latexextra"
    "texlive-fontsextra"
    "texlive-pictures"
    "texlive-science"
    "mpv"
    "enchant"
    "hunspell"
    "hunspell-en_us"
    "hunspell-es_any"
  )

  install_packages "${packages[@]}"

  animate "Cloning Doom Emacs..."
  git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs

  animate "Adding Doom Emacs settings..."
  stow doom-emacs

  animate "Installing Doom Emacs..."
  ~/.config/emacs/bin/doom install

  animate "Doom Emacs installation completed successfully!"
}

# Function to install fastfetch
install_fastfetch() {
  animate "Adding Fastfetch settings..."
  stow fastfetch

  animate "Fastfetch installation completed successfully!"
}

# Function to install fish
install_fish() {
  local packages=(
    "fish"
    "lsd"
    "bat"
  )

  install_packages "${packages[@]}"

  animate "Changing the default shell to Fish..."
  chsh -s /usr/bin/fish

  curl -L https://github.com/oh-my-fish/oh-my-fish/raw/master/bin/install | fish

  animate "Adding Fish settings..."
  stow fish

  animate "Fish installation completed successfully!"
}

# Function to install htop
install_htop() {
  animate "Adding htop settings..."
  stow htop

  animate "htop installation completed successfully!"
}
