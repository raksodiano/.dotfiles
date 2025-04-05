#!/bin/sh

source ./scripts/utils.sh

# Function to install brave
install_brave() {
  if ! is_package_installed "brave-bin"; then
    yay --noconfirm brave-bin
  fi
}

# Function to install insomnia
install_insomnia() {
  if ! is_package_installed "insomnia-bin"; then
    yay --noconfirm insomnia-bin
  fi
}

# Function to install vscodium
install_codium() {
  if ! is_package_installed "vscodium-bin"; then
    yay --noconfirm vscodium-bin
  fi

  if ! is_package_installed "vscodium-bin-marketplace"; then
    yay --noconfirm vscodium-bin-marketplace
  fi
}

# Function to install slack
install_slack() {
  if ! is_package_installed "slack-desktop"; then
    yay --noconfirm slack-desktop
  fi
}

# Function to install jetbrains-toolbox
install_jetbrains() {
  if ! is_package_installed "jetbrains-toolbox"; then
    yay --noconfirm jetbrains-toolbox
  fi
}

# Function to install all
install_all() {
  install_brave
  install_insomnia
  install_codium
  install_slack
  install_jetbrains
}

# Menu function
show_menu_aur_packages() {
  clear
  echo -e "--------------------------------------"
  echo -e "      ${Blue}ArchLinux${RESET} Setup Menu          "
  echo -e "--------------------------------------"
  echo -e "1. Install all ${GREEN}packages${RESET}"
  echo -e "2. Install brave"
  echo -e "3. Install insomnia"
  echo -e "4. Install codium"
  echo -e "5. Install slack"
  echo -e "6. Install jetbrains toolbox"
  echo -e "0. Exit"
  echo -e "--------------------------------------"
  echo -n "Please choose an option [0-6]: "
}

while true; do
  show_menu_aur_packages
  read -r option
  case $option in
    0)
      echo "Exiting..."
      exit 0
      ;;
    1)
      clear
      install_all
      ;;
    2)
      clear
      install_brave
      ;;
    3)
      clear
      install_insomnia
      ;;
    4)
      clear
      install_codium
      ;;
    5)
      clear
      install_slack
      ;;
    6)
      install_jetbrains
      ;;
    *)
      echo "Invalid option, please choose a valid option [0-6]."
      ;;
  esac
  echo -n "Press [Enter] to continue..."
  read -r
done
