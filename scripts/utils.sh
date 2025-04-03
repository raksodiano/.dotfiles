#!/bin/sh

RED="\033[31m"
GREEN="\033[32m"
YELLOW="\033[33m"
BLUE="\033[34m"
RESET="\033[0m"

# Function to update the system
update_system() {
  echo "Updating the system..."
  sudo pacman -Syu --noconfirm
}

# Function to check if a package is installed
is_package_installed() {
  local package_name=$1
  if pacman -Q "$package_name" &>/dev/null; then
    echo -e "${GREEN}${package_name}${RESET} is already installed."
    return 0  # Package is installed

  else
    echo -e "Installing ${YELLOW}${package_name}${RESET}"
    return 1  # Package is not installed

  fi
}

install_package_if_missing() {
  local package_name="$1"
  if ! is_package_installed "$package_name"; then
    sudo pacman -S --noconfirm "$package_name"
  fi
}

message_dependencies() {
  local name=$1
  echo -e "Installing ${YELLOW}${name}${RESET} dependencies..."
}

message_stow_linking() {
  local name=$1
  echo -e "Linking ${YELLOW}${name}${RESET} configurations using stow..."
}

message_stow_linked() {
  local name=$1
  echo -e "${YELLOW}${name}${RESET} configurations successfully linked."
}
