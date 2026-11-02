#!/bin/bash

# Exit on any error
set -euo pipefail

# Get script directory to handle relative paths correctly
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Parse arguments
HOSTNAME=""
while [[ $# -gt 0 ]]; do
    case $1 in
        --hostname|-H)
            HOSTNAME="$2"
            shift 2
            ;;
        *)
            echo "$ERROR Unknown option: $1"
            exit 1
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

# Set colors for output messages
OK="$(tput setaf 2)[OK]$(tput sgr0)"
ERROR="$(tput setaf 1)[ERROR]$(tput sgr0)"
NOTE="$(tput setaf 3)[NOTE]$(tput sgr0)"
INFO="$(tput setaf 4)[INFO]$(tput sgr0)"
WARN="$(tput setaf 1)[WARN]$(tput sgr0)"
ORANGE="$(tput setaf 214)"
RESET="$(tput sgr0)"

# Error trap to show line number on failure
trap 'echo "$ERROR Error occurred on line $LINENO. Exiting."; exit 1' ERR

# Cache sudo credentials upfront
echo "$INFO Caching sudo credentials (you may be prompted once)..."
sudo -v

# Background process to refresh sudo cache every 4 minutes to avoid re-prompting
(while true; do sudo -v; sleep 240; done) &
SUDO_REFRESH_PID=$!
# Kill the background process on script exit
trap 'kill $SUDO_REFRESH_PID 2>/dev/null' EXIT

# Package manager
PACKAGE_MANAGER=paru

# Check if an Arch Linux package is installed
is_installed() {
    pacman -Qi "$1" &>/dev/null
}

# Check if an Arch Linux group is installed
is_group_installed() {
    pacman -Si "$1" &>/dev/null
}

# Check if a repo file exists in /etc/pacman.d/
is_repo_file_present() {
    [ -f "/etc/pacman.d/$1" ]
}

# Function to install packages if not already installed
install_packages() {
    local packages=("$@")
    local to_install=()

    for pkg in "${packages[@]}"; do
        if ! is_installed "$pkg" && ! is_group_installed "$pkg"; then
            to_install+=("$pkg")
        fi
    done

    if [ ${#to_install[@]} -ne 0 ]; then
        echo "$INFO Installing: ${to_install[*]}"
        sudo $PACKAGE_MANAGER -S --noconfirm --needed "${to_install[@]}"
        echo "$OK Installation completed"
    else
        echo "$NOTE All packages already installed, skipping"
    fi
}

# Source the package and service list
if [ ! -f "$SCRIPT_DIR/scripts/packages.conf" ]; then
    echo "$ERROR packages.conf not found!"
    exit 1
fi
source "$SCRIPT_DIR/scripts/packages.conf"

echo "$INFO Starting system setup..."
echo "-----"

if [ -n "$HOSTNAME" ]; then
    echo "$INFO Setting hostname to $HOSTNAME..."
    sudo hostnamectl set-hostname "$HOSTNAME"
    echo "$OK Hostname set to $HOSTNAME"
fi

echo "$INFO Appending missing pacman config options if not present..."
echo "-----"
options=("ParallelDownloads=5")
for option in "${options[@]}"; do
    if ! grep -v '^\s*#' /etc/pacman.conf | grep -qFx "$option"; then
        echo "$option" | sudo tee -a /etc/pacman.conf
    fi
done

echo "$INFO Applying system optimizations..."
echo "-----"

## Swappiness
if ! grep -q "vm.swappiness=10" /etc/sysctl.conf 2>/dev/null; then
    echo "vm.swappiness=10" | sudo tee -a /etc/sysctl.conf
    sudo sysctl -p
    echo "$OK Swappiness set to 10"
fi

## MESA GL Thread
if ! grep -q "MESA_GLTHREAD=true" ~/.profile 2>/dev/null; then
    echo 'export MESA_GLTHREAD=true' >> ~/.profile
    export MESA_GLTHREAD=true
    echo "$OK MESA_GLTHREAD enabled"
fi

## Tracker miner mask
if ! systemctl --user is-enabled --quiet tracker-miner-fs-3.service 2>/dev/null | grep -q masked; then
    systemctl --user mask tracker-miner-fs-3.service
    echo "$OK Tracker miner masked"
fi

## MAC randomization
if [ ! -f /etc/NetworkManager/conf.d/00-macrandomize.conf ]; then
    echo "$INFO Configuring MAC randomization..."
    sudo tee /etc/NetworkManager/conf.d/00-macrandomize.conf > /dev/null << 'EOF'
[device]
wifi.scan-rand-mac-address=yes

[connection]
wifi.cloned-mac-address=stable
ethernet.cloned-mac-address=stable
connection.stable-id=${CONNECTION}/${BOOT}
EOF
    sudo systemctl restart NetworkManager
    echo "$OK MAC randomization configured"
fi

echo "$INFO Updating the system..."
echo "-----"

sudo paru -Syu
echo "$OK System update completed successfully"

echo "$INFO Adding plugins..."
echo "-----"

# CachyOS ships its own repositories and tooling; the base package lists
# above are enough.

if ! is_installed docker; then
    echo "$INFO Docker..."
    sudo paru -S --noconfirm --needed docker
    echo "$OK Docker installed"
fi

if ! is_installed nerd-fonts; then
    echo "$INFO Nerd Fonts..."
    sudo paru -S --noconfirm --needed nerd-fonts
    echo "$OK Nerd Fonts installed"
fi

echo "$INFO Adding repositories to the system..."
echo "-----"

# CachyOS ships its own repositories through pacman; the Fedora/RPM/COPR
# specific repository steps no longer apply to this system.

echo "$INFO Flatpak..."
flatpak remote-add --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo
flatpak remote-add --user --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
echo "$OK Repositories added successfully"

echo "$INFO Adding firmware to the system..."
echo "-----"

# Refresh firmware metadata only if older than 1 day
if [[ ! -f /var/cache/fwupd/metadata ]] || [[ $(find /var/cache/fwupd -name "metadata" -mtime +0 2>/dev/null) ]]; then
    fwupdmgr refresh || echo "$WARN Could not refresh firmware metadata"
fi

fwupdmgr get-devices
fwupdmgr get-updates || echo "$INFO No firmware updates available"
fwupdmgr update || echo "$WARN Firmware update requires manual intervention"

echo "$INFO Adding codecs to the system..."
echo "-----"

if ! is_installed ffmpeg; then
    echo "$INFO Installing ffmpeg..."
    sudo paru -S --noconfirm --needed ffmpeg
    echo "$OK ffmpeg installed"
fi

for pkg in libva-intel-driver openh264 gstreamer1-plugin-openh264 mozilla-openh264; do
    if ! is_installed "$pkg"; then
        sudo paru -S --noconfirm --needed "$pkg" || echo "$WARN Could not install $pkg"
    fi
done

echo "$OK Codecs configured successfully"

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
        if ! systemctl is-enabled --quiet "$service" 2>/dev/null; then
            echo "$INFO Enabling $ORANGE$service$RESET..."
            sudo systemctl enable "$service"
            echo "$OK $ORANGE$service$RESET enabled"
        else
            echo "$NOTE $ORANGE$service$RESET is already enabled"
        fi
    done
    echo "-----"
fi

if [ ${#SERVICES_USER[@]} -gt 0 ]; then
    echo "$INFO Configuring user services..."
    echo "-----"
    for service in "${SERVICES_USER[@]}"; do
        if ! systemctl --user is-enabled --quiet "$service" 2>/dev/null; then
            echo "$INFO Enabling $ORANGE$service$RESET for the user..."
            systemctl --user enable "$service"
            echo "$OK $ORANGE$service$RESET enabled for the user"
        else
            echo "$NOTE $ORANGE$service$RESET is already enabled for the user"
        fi
    done
    echo "-----"
fi

echo "$INFO Configuring Dotfiles..."
echo "-----"

# Source the dotfiles functions
source "$SCRIPT_DIR/scripts/dotfiles/default.sh"

# Install all dotfiles
install_all_dotfiles
echo "$OK Dotfiles configured successfully"

echo "$INFO Configuring system settings..."
echo "-----"

## The current system runs niri + alacritty/kitty instead of GNOME, so the
## desktop appearance lives in the stow-managed packages (niri, kitty, ...).

echo "$OK System settings step handled by stow-managed packages"

echo "$OK System setup completed successfully."
echo "-----"
exit 0
