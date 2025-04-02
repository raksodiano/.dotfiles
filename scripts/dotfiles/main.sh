#!/bin/bash

source ../utils.sh

install_emacs() {
    if is_package_installed "emacs"; then
        echo "emacs is already installed."
    else
        echo "Installing emacs..."
        sudo pacman -S --noconfirm emacs
    fi

    echo "Installing Doom Emacs dependencies..."
    sudo pacman -S --noconfirm mpv enchant hunspell hunspell-en_us hunspell-es_eny

    git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs

    echo "Linking Doom Emacs configurations using stow..."
    stow doom-emacs
    echo "Doom Emacs configurations successfully linked."

    echo "Installing Doom Emacs..."
    ~/.config/emacs/bin/doom install
    echo "Doom Emacs installation completed successfully!"
}

install_fastfetch() {
    if is_package_installed "fastfetch"; then
        echo "fastfetch is already installed."
    else
        echo "Installing fastfetch..."
        sudo pacman -S --noconfirm fastfetch
    fi

    echo "Linking fastfetch configurations using stow..."
    stow fastfetch

    echo "Fastfetch configurations successfully linked."
}

install_fish() {
    if is_package_installed "fish"; then
        echo "fish is already installed."
    else
        echo "Installing fish..."
        sudo pacman -S --noconfirm fish
    fi

    echo "Changing the default shell to Fish..."
    chsh -s /usr/bin/fish

    echo "Linking fish configurations using stow..."
    stow fish

    echo "fish configurations successfully linked."
}

install_htop() {
    if is_package_installed "htop"; then
        echo "htop is already installed."
    else
        echo "Installing htop..."
        sudo pacman -S --noconfirm htop
    fi

    echo "Removing the default configuration of htop..."
    rm -f ~/.config/htop/htoprc

    echo "Linking htop configurations using stow..."
    stow htop
    echo "htop configurations successfully linked."
}

install_all() {
    install_htop
    install_fish
    install_fastfetch
    install_emacs
}

# Menu function
show_menu_dotfiles() {
    clear
    echo "--------------------------------------"
    echo "      Arch Linux Setup Menu          "
    echo "--------------------------------------"
    echo "1. Install all"
    echo "2. Install emacs and doom emacs"
    echo "3. Install fastfetch"
    echo "4. Install fish (by default)"
    echo "5. Install htop"
    echo "0. Exit"
    echo "--------------------------------------"
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
