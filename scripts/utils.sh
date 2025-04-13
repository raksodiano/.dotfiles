#!/bin/bash

RED="\033[31m"
GREEN="\033[32m"
YELLOW="\033[33m"
BLUE="\033[34m"
RESET="\033[0m"

# Cursor animation
blinking_cursor() {
  local symbol="${1:-_}"
  local times="${2:-3}"
  local delay_on="${3:-0.3}"
  local delay_off="${4:-0.2}"

  for ((i=0; i<times; i++)); do
    echo -ne "$symbol\r"
    sleep "$delay_on"
    echo -ne "  \r"
    sleep "$delay_off"
  done

  echo
}

# Text animation
animate() {
  text="$1"
  for ((i=0; i<${#text}; i++)); do
    echo -n "${text:$i:1}"
    sleep 0.04
  done
  echo
}

# Block/Logo animation
animate_block() {
  local block="$1"
  while IFS= read -r line; do
    echo "$line"
    sleep 0.05
  done <<< "$block"
  echo
}

# Hide native cursor
hide_cursor() {
  echo -ne "\e[?25l"
}

# Show cursor
show_cursor() {
  echo -ne "\e[?25h"
}

# Function to install yay (AUR helper)
install_yay() {
  if ! command -v yay &> /dev/null; then
    animate "Installing yay AUR helper..."
    sudo pacman -S --needed git base-devel --noconfirm
    git clone https://aur.archlinux.org/yay.git
    cd yay

    animate "Building yay.......     yaaaaaayyyyy"

    makepkg -si --noconfirm
    cd ..

    rm -rf yay
  fi

  animate "yay is already installed"
}

# Function to check if a package is installed
is_installed() {
  pacman -Qi "$1" &> /dev/null
}

# Function to check if a package is installed
is_group_installed() {
  pacman -Qg "$1" &> /dev/null
}

# Function to install packages if not already installed
install_packages() {
  local packages=("$@")
  local to_install=()

  for pkg in "${packages[@]}"; do
    if ! is_installed "$pkg" && ! is_group_installed "$pkg"; then
      to_install+=("$pkg")
    else
      animate "${pkg} already installed"
    fi
  done

  if [ ${#to_install[@]} -ne 0 ]; then
    animate "Installing: ${to_install[*]}"
    yay -S --noconfirm "${to_install[@]}"
  fi

  echo
}
