#!/bin/bash

# Exit on any error
set -e

# Default AUR helper
AUR_HELPER="paru"

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
    --aur-helper)
        if [[ "$2" == "paru" || "$2" == "yay" ]]; then
            AUR_HELPER="$2"
            shift 2
        else
            echo "Error: --aur-helper must be 'paru' or 'yay'"
            exit 1
        fi
        ;;
    --paru)
        AUR_HELPER="paru"
        shift
        ;;
    --yay)
        AUR_HELPER="yay"
        shift
        ;;
    *)
        shift
        ;;
    esac
done

# Clean screen
clear

printf "\n%.0s" {1..2}
echo -e "\e[35m
██████╗  █████╗ ██╗  ██╗███████╗ ██████╗ ██████╗ ██╗ █████╗ ███╗   ██╗ ██████╗ 
██╔══██╗██╔══██╗██║ ██╔╝██╔════╝██╔═══██╗██╔══██╗██║██╔══██╗████╗  ██║██╔═══██╗
██████╔╝███████║█████╔╝ ███████╗██║   ██║██║  ██║██║███████║██╔██╗ ██║██║   ██║
██╔══██╗██╔══██║██╔═██╗ ╚════██║██║   ██║██║  ██║██║██╔══██║██║╚██╗██║██║   ██║
██║  ██║██║  ██║██║  ██╗███████║╚██████╔╝██████╔╝██║██║  ██║██║ ╚████║╚██████╔╝
╚═╝  ╚═╝╚═╝  ╚═╝╚═╝  ╚═╝╚══════╝ ╚═════╝ ╚═════╝ ╚═╝╚═╝  ╚═╝╚═╝  ╚═══╝ ╚═════╝ 
\e[0m"
printf "\n%.0s" {1..1}

# Set some colors for output messages
OK="$(tput setaf 2)[OK]$(tput sgr0)"
ERROR="$(tput setaf 1)[ERROR]$(tput sgr0)"
NOTE="$(tput setaf 3)[NOTE]$(tput sgr0)"
INFO="$(tput setaf 4)[INFO]$(tput sgr0)"
WARN="$(tput setaf 1)[WARN]$(tput sgr0)"
CAT="$(tput setaf 6)[ACTION]$(tput sgr0)"
MAGENTA="$(tput setaf 5)"
ORANGE="$(tput setaf 214)"
WARNING="$(tput setaf 1)"
YELLOW="$(tput setaf 3)"
GREEN="$(tput setaf 2)"
BLUE="$(tput setaf 4)"
SKY_BLUE="$(tput setaf 6)"
RESET="$(tput sgr0)"

# Function to install AUR helper
install_aur_helper() {
    if command -v "$AUR_HELPER" &>/dev/null; then
        echo "$INFO $MAGENTA$AUR_HELPER$RESET already installed"
        return 0
    fi
    echo "$INFO Installing $MAGENTA$AUR_HELPER$RESET AUR helper..."

    # Dependencies
    sudo pacman -S --needed --noconfirm base-devel git
    local tmp_dir=$(mktemp -d)
    cd "$tmp_dir"
    if [[ "$AUR_HELPER" == "paru" ]]; then
        git clone https://aur.archlinux.org/paru.git
        cd paru
    else
        git clone https://aur.archlinux.org/yay.git
        cd yay
    fi

    makepkg -si --noconfirm
    cd ~
    rm -rf "$tmp_dir"

    echo "$INFO $MAGENTA $AUR_HELPER $RESET installed successfully"
}

# Function to check if a package is installed
is_installed() {
    pacman -Qi "$1" &>/dev/null
}

# Function to check if a package is installed
is_group_installed() {
    pacman -Qg "$1" &>/dev/null
}

# Function to install packages if not already installed
install_packages() {
    local packages=("$@")
    local to_install=()

    for pkg in "${packages[@]}"; do
        if ! is_installed "$pkg" && ! is_group_installed "$pkg"; then
            to_install+=("$pkg")
        else
            echo "$INFO ${pkg} already installed"
        fi
    done

    if [ ${#to_install[@]} -ne 0 ]; then
        echo "$INFO Installing: ${to_install[*]}"
        $AUR_HELPER -S --noconfirm "${to_install[@]}"
    fi

    echo "-----"
}

# Source the package and service list
if [ ! -f "./scripts/packages.conf" ]; then
    echo "$ERROR packages.conf not found!"
    exit 1
fi
source ./scripts/packages.conf

if [ -n "$(grep -i arch </etc/os-release)" ]; then
    echo "$OK Verified this is Arch Linux."
    echo "-----"
else
    echo "$ERROR This is not Arch Linux or the distribution information is not available."
    exit 1
fi

echo "$INFO Starting system setup..."
echo "-----"

echo "$INFO Updating the system..."
echo "-----"

sudo pacman -Syu --noconfirm

if command -v git &>/dev/null; then
    echo "$OK Git is installed, continuing with installation."
    echo "-----"
else
    echo "$ERROR Git is not installed."
    echo "$INFO Installing git..."
    sudo pacman -S --needed --noconfirm git
    if command -v git &>/dev/null; then
        echo "$OK Git is installed, continuing with installation."
        echo "-----"
    else
        echo "$ERROR Git is not installed. Please install Git and try again."
        exit 1
    fi
fi

if command -v stow &>/dev/null; then
    echo "$OK Stow is installed, continuing with installation."
    echo "-----"
else
    echo "$ERROR Stow is not installed."
    echo "$INFO Installing stow..."
    sudo pacman -S --needed --noconfirm stow
    if command -v stow &>/dev/null; then
        echo "$OK Stow is installed, continuing with installation."
        echo "-----"
    else
        echo "$ERROR Stow is not installed. Please install Stow and try again."
        exit 1
    fi
fi

echo "$INFO Using AUR helper: $MAGENTA$AUR_HELPER$RESET"
echo "-----"
install_aur_helper

echo "$INFO Installing packages..."
echo "-----"

if [ ${#SYSTEM_UTILS[@]} -gt 0 ]; then
    echo "$INFO Installing system utilities..."
    echo "-----"
    install_packages "${SYSTEM_UTILS[@]}"
fi

if [ ${#DEV_TOOLS[@]} -gt 0 ]; then
    echo "$INFO Installing development tools..."
    echo "-----"
    install_packages "${DEV_TOOLS[@]}"
fi

if [ ${#MAINTENANCE[@]} -gt 0 ]; then
    echo "$INFO Installing system maintenance tools..."
    echo "-----"
    install_packages "${MAINTENANCE[@]}"
fi

if [ ${#DESKTOP[@]} -gt 0 ]; then
    echo "$INFO Installing desktop environment..."
    echo "-----"
    install_packages "${DESKTOP[@]}"
fi

if [ ${#VIRTUALIZATION[@]} -gt 0 ]; then
    echo "$INFO Installing virtualization tools..."
    echo "-----"
    install_packages "${VIRTUALIZATION[@]}"
fi

if [ ${#OFFICE[@]} -gt 0 ]; then
    echo "$INFO Installing office..."
    echo "-----"
    install_packages "${OFFICE[@]}"
fi

if [ ${#MEDIA[@]} -gt 0 ]; then
    echo "$INFO Installing media packages..."
    echo "-----"
    install_packages "${MEDIA[@]}"
fi

if [ ${#CODECS[@]} -gt 0 ]; then
    echo "$INFO Installing codecs..."
    echo "-----"
    install_packages "${CODECS[@]}"
fi

if [ ${#FONTS[@]} -gt 0 ]; then
    echo "$INFO Installing fonts..."
    echo "-----"
    install_packages "${FONTS[@]}"
    echo "-----"
fi

if [ ${#SERVICES[@]} -gt 0 ]; then
    echo "$INFO Configuring system services..."
    echo "-----"
    for service in "${SERVICES[@]}"; do
        if [ ! systemctl is-enabled "$service" ] &>/dev/null; then
            echo "$INFO Enabling $ORANGE$service$RESET..."
            sudo systemctl enable "$service"
        else
            echo "$INFO $ORANGE$service$RESET is already enabled"
        fi
    done
    echo "-----"
fi

if [ ${#SERVICES_USER[@]} -gt 0 ]; then
    echo "$INFO Configuring user services..."
    echo "-----"
    for service in "${SERVICES_USER[@]}"; do
        if [ ! systemctl --user is-enabled "$service" ] &>/dev/null; then
            echo "$INFO Enabling $ORANGE$service$RESET for the user..."
            sudo systemctl --user enable "$service"
        else
            echo "$INFO $ORANGE$service$RESET is already enabled for the user"
        fi
    done
    echo "-----"
fi

echo "$INFO Configuring Dotfiles..."
echo "-----"

# Source the dotfiles functions
source ./scripts/dotfiles/default.sh

# Install all dotfiles
install_all_dotfiles

echo "$INFO System setup completed."
echo "-----"
