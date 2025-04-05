#!/bin/sh

source ./scripts/utils.sh

# Function to install firewall
install_ufw() {
  install_package_if_missing "ufw"

	sudo systemctl enable ufw
  sudo systemctl start ufw

  echo -e "Firewall ${GREEN}ufw${RESET} installation completed successfully!"
}

# Function to install tlp
install_tlp() {
  install_package_if_missing "tlp"
  install_package_if_missing "tlp-rdw"

	sudo systemctl enable tlp
  sudo systemctl start tlp

  echo -e "${GREEN}tlp${RESET} installation completed successfully!"
}

# Function to install podman
install_podman() {
  install_package_if_missing "podman"
  install_package_if_missing "podman-compose"
  install_package_if_missing "podman-docker"
  install_package_if_missing "fuse-overlayfs"

  echo -e "${GREEN}podman${RESET} installation completed successfully!"
}

# Function to install flameshot
install_flameshot() {
  install_package_if_missing "flameshot"

  echo -e "${GREEN}Flameshot${RESET} installation completed successfully!"
}

# Function to install telegram
install_telegram() {
  install_package_if_missing "telegram-desktop"

  echo -e "${GREEN}Telegram${RESET} installation completed successfully!"
}

# Function to install codecs
install_codecs() {
  local packages=(
    "ffmpeg"
    "a52dec"
		"faac"
		"faad2"
		"flac"
		"jasper"
		"lame"
		"libdca"
		"libdv"
		"libmad"
		"libmpeg2"
		"libtheora"
		"libvorbis"
		"libxv"
		"opus"
		"wavpack"
		"x264"
		"xvidcore"
  )

  install_from_array "${packages[@]}"

  echo -e "${GREEN}codecs${RESET} installation completed successfully!"
}

# Function to install all
install_all() {
  install_ufw
  install_tlp
  install_podman
  install_flameshot
  install_telegram
  install_codecs
}

# Menu function
show_menu_packages() {
  clear
  echo -e "--------------------------------------"
  echo -e "      ${Blue}ArchLinux${RESET} Setup Menu          "
  echo -e "--------------------------------------"
  echo -e "1. Install all ${GREEN}packages${RESET}"
  echo -e "2. Install ufw (firewall)"
  echo -e "3. Install tlp (Optimize Linux Laptop)"
  echo -e "4. Install podman"
  echo -e "5. Install flameshot"
  echo -e "6. Install telegram"
  echo -e "7. Install ${GREEN}codecs${RESET}"
  echo -e "0. Exit"
  echo -e "--------------------------------------"
  echo -n "Please choose an option [0-7]: "
}

while true; do
  show_menu_packages
  read -r option
  case $option in
    0)
      echo "Exiting..."
      exit 0
      ;;
    1)
      clear
      install_all
      ;;
    2)
      clear
      install_ufw
      ;;
    3)
      clear
      install_tlp
      ;;
    4)
      clear
      install_podman
      ;;
    5)
      clear
      install_flameshot
      ;;
    6)
      install_telegram
      ;;
    7)
      install_codecs
      ;;
    *)
      echo "Invalid option, please choose a valid option [0-7]."
      ;;
  esac
  echo -n "Press [Enter] to continue..."
  read -r
done
