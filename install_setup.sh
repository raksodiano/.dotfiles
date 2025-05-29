#!/bin/bash

# Clean screen
clear

# Exit on any error
set -e

# Source the logos
if [ ! -f "./scripts/logos.sh" ]; then
  echo "Error: logos.sh not found!"
  exit 1
fi

# Source the utils
if [ ! -f "./scripts/utils.sh" ]; then
  echo "Error: utils.sh not found!"
  exit 1
fi

# Source the package list
if [ ! -f "./scripts/packages.conf" ]; then
  echo "Error: packages.conf not found!"
  exit 1
fi

# Source the services list
if [ ! -f "./scripts/services.conf" ]; then
  echo "Error: services.conf not found!"
  exit 1
fi

source ./scripts/logos.sh
source ./scripts/utils.sh
source ./scripts/packages.conf
source ./scripts/services.conf
source ./scripts/dotfiles/main.sh

# Hide native cursor
hide_cursor

animate_block "$INIT_LOGO"
blinking_cursor
sleep 1

animate "Starting system setup..."
blinking_cursor
sleep 1

animate "Updating the system..."
blinking_cursor
sleep 1

# Show native cursor
show_cursor

sudo pacman -Syu --noconfirm

# Hide native cursor
hide_cursor

animate "Installing fonts..."
install_packages "${FONTS[@]}"

animate "Installing system utilities..."
install_packages "${SYSTEM_UTILS[@]}"

animate "Installing development tools..."
install_packages "${DEV_TOOLS[@]}"

animate "Installing system maintenance tools..."
install_packages "${MAINTENANCE[@]}"

animate "Installing desktop environment..."
install_packages "${DESKTOP[@]}"

animate "Installing office..."
install_packages "${OFFICE[@]}"

animate "Installing media packages..."
install_packages "${MEDIA[@]}"

animate "Installing codecs packages..."
install_packages "${CODES[@]}"

# Enable services
animate "Configuring services..."
for service in "${SERVICES[@]}"; do
  if ! systemctl is-enabled "$service" &>/dev/null; then
    animate "Enabling $service..."
    sudo systemctl enable "$service"
  else
    animate "$service is already enabled"
  fi
done

systemctl --user enable syncthing.service

echo

# Enable dotfiles
animate "Configuring dotfiles..."
install_htop
install_fastfetch
install_emacs

show_cursor
