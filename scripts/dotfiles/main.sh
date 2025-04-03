#!/bin/sh

source ./scripts/utils.sh

# Function to install emacs
install_emacs() {
  install_package_if_missing "emacs"

  message_dependencies "Doom Emacs"

  install_package_if_missing "mpv"
  install_package_if_missing "enchant"
  install_package_if_missing "hunspell"
  install_package_if_missing "hunspell-en_us"
  install_package_if_missing "hunspell-es_any"

  git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs

  message_stow_linking "Doom Emacs"
  stow doom-emacs
  message_stow_linked "Doom Emacs"

  echo "Installing Doom Emacs..."
  ~/.config/emacs/bin/doom install
  echo "Doom Emacs installation completed successfully!"
}

# Function to install fastfetch
install_fastfetch() {
  install_package_if_missing "fastfetch"

  message_stow_linking "fastfetch"
  stow fastfetch
  message_stow_linked "fastfetch"
}

# Function to install fish
install_fish() {
  install_package_if_missing "fish"

  message_dependencies "fish"

  install_package_if_missing "lsd"
  install_package_if_missing "bat"

  echo "Changing the default shell to Fish..."
  chsh -s /usr/bin/fish

  curl -L https://github.com/oh-my-fish/oh-my-fish/raw/master/bin/install | fish

  message_stow_linking "fish"
  stow fish
  message_stow_linked "fish"
}

# Function to install htop
install_htop() {
  install_package_if_missing "htop"

  echo "Removing the default configuration of htop..."
  rm -f ~/.config/htop/htoprc

  message_stow_linking "htop"
  stow htop
  message_stow_linked "htop"
}

# Function to install all
install_all() {
  install_htop
  install_fish
  install_fastfetch
  install_emacs
}

# Menu function
show_menu_dotfiles() {
  clear
  echo -e "--------------------------------------"
  echo -e "      ${BLUE}ArchLinux${RESET} Setup Menu          "
  echo -e "--------------------------------------"
  echo -e "1. Install all"
  echo -e "2. Install emacs and doom emacs"
  echo -e "3. Install fastfetch"
  echo -e "4. Install fish (by default)"
  echo -e "5. Install htop"
  echo -e "0. Exit"
  echo -e "--------------------------------------"
  echo -n "Please choose an option [0-5]: "
}

while true; do
  show_menu_dotfiles
  read -r option
  case $option in
    0)
      echo "Exiting..."
      exit 0
      ;;
    1)
      install_all
      ;;
    2)
      install_emacs
      ;;
    3)
      install_fastfetch
      ;;
    4)
      install_fish
      ;;
    5)
      install_htop
      ;;
    *)
      echo "Invalid option, please choose a valid option [0-5]."
      ;;
  esac
  echo -n "Press [Enter] to continue..."
  read -r
done
