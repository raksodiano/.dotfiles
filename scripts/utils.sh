#!/bin/bash

# Function to update the system
update_system() {
    echo "Updating the system..."
    sudo pacman -Syu --noconfirm
}

# Function to check if a package is installed
is_package_installed() {
    local package_name=$1
    if pacman -Q "$package_name" &>/dev/null; then
        return 0  # Package is installed
    else
        return 1  # Package is not installed
    fi
}
