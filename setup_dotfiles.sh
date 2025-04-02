#!/bin/bash

source ./scripts/utils.sh

# Function to install yay (AUR helper)
install_yay() {
    if is_package_installed "yay"; then
        echo "yay is already installed."

    else
        echo "Installing yay..."
        sudo pacman -S --noconfirm base-devel git

        git clone https://aur.archlinux.org/yay.git
        cd yay

        makepkg -si --noconfirm
        cd ..

        rm -rf yay
    fi
}

# Function to install stow
install_stow() {
    if is_package_installed "stow"; then
        echo "stow is already installed."
    else
        echo "Installing stow..."
        sudo pacman -S --noconfirm stow
    fi
}

# Function to install volta
install_volta() {
    if is_package_installed "volta-bin"; then
        echo "volta-bin is already installed."
    else
        echo "Installing volta-bin..."
        yay -Sua --noconfirm volta-bin
    fi
}

# Function to install dotfiles
install_dotfiles() {
    source ./scripts/dotfiles/main.sh
}

# Menu function
show_menu() {
    clear
    echo "--------------------------------------"
    echo "      Arch Linux Setup Menu          "
    echo "--------------------------------------"
    echo "1. Update System"
    echo "2. Install yay"
    echo "3. Install stow"
    echo "4. Install volta (with yay, package volta-bin)"
    echo "5. Install dotfiles"
    echo "6. Install aur packages (with yay)"
    echo "0. Exit"
    echo "--------------------------------------"
    echo -n "Please choose an option [0-6]: "
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
            update_system
            ;;
        2)
            install_yay
            ;;
        3)
            install_stow
            ;;
        4)
            install_volta
            ;;
        5)
            install_dotfiles
            ;;
        6)
            echo "in development"
            ;;
        *)
            echo "Invalid option, please choose a valid option [0-6]."
            ;;
    esac
    echo -n "Press [Enter] to continue..."
    read -r
done
