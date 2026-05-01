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
PACKAGE_MANAGER=dnf

# Check if an RPM package is installed
is_installed() {
    rpm -q "$1" &>/dev/null
}

# Check if a DNF group is installed
is_group_installed() {
    # dnf5 group info shows "Installed: yes/no" with spacing
    local info
    info=$(dnf group info "$1" 2>/dev/null)
    if echo "$info" | grep -q "Installed.*yes"; then
        return 0
    fi
    return 1
}

# Check if a COPR repository is enabled
is_copr_enabled() {
    dnf copr list enabled 2>/dev/null | grep -qw "$1"
}

# Check if a repo file exists in /etc/yum.repos.d/
is_repo_file_present() {
    [ -f "/etc/yum.repos.d/$1" ]
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
        sudo $PACKAGE_MANAGER install -y "${to_install[@]}"
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

echo "$INFO Appending missing DNF config options if not present..."
echo "-----"
options=("fastestmirror=true" "max_parallel_downloads=5" "deltarpm=true")
for option in "${options[@]}"; do
    # Check uncommented lines for exact option match
    if ! grep -v '^\s*#' /etc/dnf/dnf.conf | grep -qFx "$option"; then
        echo "$option" | sudo tee -a /etc/dnf/dnf.conf
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

sudo dnf update -y
echo "$OK System updated successfully"

echo "$INFO Adding plugins..."
echo "-----"

if ! is_installed dnf-plugins-core; then
    sudo dnf install -y dnf-plugins-core
    echo "$OK dnf-plugins-core installed"
fi

if ! is_group_installed core; then
    sudo dnf group upgrade -y core
    sudo dnf group install -y core
    echo "$OK Core group installed"
fi

echo "$INFO Adding repositories to the system..."
echo "-----"

# Fedora Workstation repositories
if ! is_installed fedora-workstation-repositories; then
    echo "$INFO Fedora Workstation..."
    sudo dnf install -y fedora-workstation-repositories
fi

# RPM Fusion
if ! is_installed rpmfusion-free-release || ! is_installed rpmfusion-nonfree-release; then
    echo "$INFO RPM Fusion..."
    sudo dnf install -y https://mirrors.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm https://mirrors.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm
fi

# Terra repository
if ! is_installed terra-release; then
    echo "$INFO Terra..."
    sudo dnf install -y --nogpgcheck --repofrompath 'terra,https://repos.fyralabs.com/terra$releasever' terra-release
fi

# Brave browser repository
if ! is_repo_file_present brave-browser.repo; then
    echo "$INFO Brave browser..."
    sudo dnf config-manager addrepo -y --from-repofile=https://brave-browser-rpm-release.s3.brave.com/brave-browser.repo
fi

# Librewolf repository
if ! is_repo_file_present librewolf.repo; then
    echo "$INFO Librewolf..."
    sudo dnf config-manager addrepo -y --from-repofile=https://repo.librewolf.net/librewolf.repo
fi

# VSCodium repository
if ! is_repo_file_present vscodium.repo; then
    echo "$INFO VSCodium..."
    sudo tee /etc/yum.repos.d/vscodium.repo << 'EOF'
[gitlab.com_paulcarroty_vscodium_repo]
name=gitlab.com_paulcarroty_vscodium_repo
baseurl=https://paulcarroty.gitlab.io/vscodium-deb-rpm-repo/rpms/
enabled=1
gpgcheck=1
repo_gpgcheck=1
gpgkey=https://gitlab.com/paulcarroty/vscodium-deb-rpm-repo/raw/master/pub.gpg
metadata_expire=1h
EOF
fi

# Antigravity repository
if ! is_repo_file_present antigravity.repo; then
    echo "$INFO Antigravity..."
    sudo tee /etc/yum.repos.d/antigravity.repo << 'EOF'
[antigravity-rpm]
name=Antigravity RPM Repository
baseurl=https://us-central1-yum.pkg.dev/projects/antigravity-auto-updater-dev/antigravity-rpm
enabled=1
gpgcheck=0
EOF
fi

# Nerd Fonts COPR
if ! is_copr_enabled aquacash5/nerd-fonts; then
    echo "$INFO Nerd Fonts..."
    sudo dnf copr enable -y aquacash5/nerd-fonts
fi

# Docker repository
if ! is_repo_file_present docker-ce.repo; then
    echo "$INFO Docker..."
    sudo dnf config-manager addrepo -y --from-repofile https://download.docker.com/linux/fedora/docker-ce.repo
fi

# Flatpak
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

if ! is_group_installed multimedia; then
    sudo dnf group install -y multimedia
    echo "$OK Multimedia group installed"
fi

if is_installed ffmpeg-free && ! is_installed ffmpeg; then
    sudo dnf swap -y 'ffmpeg-free' 'ffmpeg' --allowerasing
    echo "$OK Swapped to ffmpeg"
fi

if ! is_group_installed sound-and-video; then
    sudo dnf group install -y sound-and-video
fi

if is_installed libva-intel-media-driver && ! is_installed intel-media-driver; then
    sudo dnf swap -y libva-intel-media-driver intel-media-driver --allowerasing
fi

for pkg in libva-intel-driver openh264 gstreamer1-plugin-openh264 mozilla-openh264; do
    if ! is_installed "$pkg"; then
        sudo dnf install -y "$pkg"
    fi
done

sudo dnf config-manager setopt fedora-cisco-openh264.enabled=1
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

echo "$INFO Configuring GNOME settings..."
echo "-----"

## Themes
if ! is_installed gnome-tweaks; then
    echo "$INFO Installing gnome-tweaks..."
    sudo dnf install -y gnome-tweaks
    echo "$OK gnome-tweaks installed"
fi

echo "$INFO Setting flatpak filesystem access..."
if ! flatpak override --user --show 2>/dev/null | grep -q "xdg-config/gtk-4.0"; then
    flatpak override --user --filesystem=xdg-config/gtk-4.0
    echo "$OK Flatpak filesystem configured for user"
fi

## Extensions
echo "$INFO Installing Extension Manager..."
if ! flatpak info --system flathub com.mattjakeman.ExtensionManager &>/dev/null; then
    sudo flatpak install --system -y flathub com.mattjakeman.ExtensionManager
    echo "$OK Extension Manager installed (system-wide)"
fi

## GNOME Configurations
echo "$INFO Applying GNOME configurations..."

## Display scaling
gsettings set org.gnome.desktop.interface text-scaling-factor 1.0
echo "$OK Display scaling set to 1.0"

## Keyboard Layout
gsettings set org.gnome.desktop.input-sources show-all-sources true
gsettings set org.gnome.desktop.input-sources sources "[('xkb', 'eu')]"
echo "$OK Keyboard layout configured (EU)"

## Time format
gsettings set org.gnome.desktop.interface clock-format '24h'
echo "$OK 24h time format enabled"

## Window buttons
gsettings set org.gnome.desktop.wm.preferences button-layout ':minimize,maximize,close'
echo "$OK Window buttons configured"

## Dark theme
gsettings set org.gnome.desktop.interface color-scheme 'prefer-dark'
echo "$OK Dark theme enabled"

## Animations
gsettings set org.gnome.desktop.interface enable-animations false
echo "$OK Animations disabled"

## Lock screen
gsettings set org.gnome.desktop.session idle-delay 900
echo "$OK Lock screen delay set to 15 minutes"

## Nautilus
gsettings set org.gnome.nautilus.preferences default-sort-order type
echo "$OK Nautilus sort order set to type"

## Enable extensions
if gnome-extensions list | grep -q "appindicatorsupport@rgcjonas.gmail.com"; then
    gnome-extensions enable appindicatorsupport@rgcjonas.gmail.com
    echo "$OK AppIndicator extension enabled"
else
    echo "$NOTE AppIndicator extension not found"
fi

## Keyboard shortcuts
echo "$INFO Configuring keyboard shortcuts..."

# Terminal shortcut
gsettings set org.gnome.settings-daemon.plugins.media-keys custom-keybindings "['/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/']"

gsettings set org.gnome.settings-daemon.plugins.media-keys.custom-keybinding:/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/ name 'Terminal'

gsettings set org.gnome.settings-daemon.plugins.media-keys.custom-keybinding:/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/ command 'ptyxis'

gsettings set org.gnome.settings-daemon.plugins.media-keys.custom-keybinding:/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/ binding '<Ctrl><Alt>t'

echo "$OK Terminal shortcut Ctrl+Alt+T configured"

echo "$OK System setup completed successfully."
echo "-----"
exit 0
