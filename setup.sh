#!/bin/sh

# This script automates the installation of applications and the configuration
# of personal dotfiles. It is focused on my daily workflow and usage, allowing
# for quick installation of dependencies and programs I use, in controlled environments,
# or simply adding configurations across my different workstations.

source ./scripts/utils.sh

# Function to install yay (AUR helper)
install_yay() {
  if ! is_package_installed "yay"; then
    install_package_if_missing "base-devel"
    install_package_if_missing "git"

    git clone https://aur.archlinux.org/yay.git
    cd yay

    makepkg -si --noconfirm
    cd ..

    rm -rf yay
  fi
}

# Function to install stow
install_stow() {
  install_package_if_missing "stow"
}

# Function to install volta
install_volta() {
  if ! is_package_installed "volta-bin"; then
    yay --noconfirm volta-bin
  fi
}

# Function to install fonts
install_fonts() {
  local packages=(
    "ttf-3270-nerd"
    "ttf-nerd-fonts-symbols"
    "ttf-nerd-fonts-symbols-common"
    "ttf-iosevka-nerd"
    "ttf-iosevkaterm-nerd"
    "ttf-hack-nerd"
    "ttf-hack"
    "ttf-font-awesome"
    "noto-fonts"
  )

  install_from_array "${packages[@]}"

  fc-cache -fv
}

# Function to install dotfiles
install_dotfiles() {
  source ./scripts/dotfiles/main.sh
}

# Function to install apps
install_apps() {
  source ./scripts/apps/main.sh
}

# Function to install aur apps
install_aur_packages() {
  source ./scripts/aur_packages/main.sh
}

# Menu function
show_menu() {
  clear
  echo -e "--------------------------------------"
  echo -e "      ${Blue}ArchLinux${RESET} Setup Menu          "
  echo -e "--------------------------------------"
  echo -e "1. ${BLUE}Update${RESET} System"
  echo -e "2. Install yay"
  echo -e "3. Install stow"
  echo -e "4. Install volta (with yay, package volta-bin)"
  echo -e "5. Install fonts 󰣇  "
  echo -e "6. Install ${GREEN}dotfiles${RESET}"
  echo -e "7. Install ${GREEN}apps${RESET}"
  echo -e "8. Install ${GREEN}aur packages${RESET} (depends on yay)"
  echo -e "0. Exit"
  echo -e "--------------------------------------"
  echo -n "Please choose an option [0-8]: "
}

while true; do
  show_menu
  read -r option
  case $option in
    0)
      echo "Exiting..."
      exit 0
      ;;
    1)
      clear
      update_system
      ;;
    2)
      clear
      install_yay
      ;;
    3)
      clear
      install_stow
      ;;
    4)
      clear
      install_volta
      ;;
    5)
      clear
      install_fonts
      ;;
    6)
      install_dotfiles
      ;;
    7)
      install_apps
      ;;
    8)
      install_aur_packages
      ;;
    *)
      echo "Invalid option, please choose a valid option [0-8]."
      ;;
  esac
  echo -n "Press [Enter] to continue..."
  read -r
done
