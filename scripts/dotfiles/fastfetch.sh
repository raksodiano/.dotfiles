#!/bin/bash

source ../utils.sh

# Function to install stow
fastfetch_main() {
    if is_package_installed "fastfetch"; then
        echo "fastfetch is already installed."
    else
        echo "Installing fastfetch..."
        sudo pacman -S --noconfirm fastfetch
    fi
}
