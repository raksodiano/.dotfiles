#!/bin/bash

# ============================================
# DOTFILES INSTALLATION FUNCTIONS
# ============================================

# Generic stow function
install_dotfile() {
  local name="$1"
  local display_name="${2:-$name}"
  
  if [ -d "./$name" ]; then
    echo "$INFO Configuring $ORANGE$display_name$RESET..."
    stow -R "$name"
    echo "$OK $ORANGE$display_name$RESET configured successfully!"
  else
    echo "$WARN $ORANGE$display_name$RESET directory not found, skipping..."
  fi
}

# ============================================
# TERMINAL EMULATORS
# ============================================

install_alacritty() {
  install_dotfile "alacritty" "Alacritty"
}

install_kitty() {
  install_dotfile "kitty" "Kitty"
}

# ============================================
# SHELL & PROMPT
# ============================================

install_zsh() {
  # Install oh-my-zsh if not present
  if [ ! -d "$HOME/.oh-my-zsh" ]; then
    echo "$INFO Installing Oh My Zsh..."
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)" "" --unattended
  fi
  
  # Install zsh plugins
  local ZSH_CUSTOM="${ZSH_CUSTOM:-$HOME/.oh-my-zsh/custom}"
  
  # zsh-autosuggestions
  if [ ! -d "$ZSH_CUSTOM/plugins/zsh-autosuggestions" ]; then
    echo "$INFO Installing zsh-autosuggestions plugin..."
    git clone https://github.com/zsh-users/zsh-autosuggestions "$ZSH_CUSTOM/plugins/zsh-autosuggestions"
  else
    echo "$INFO zsh-autosuggestions already installed"
  fi
  
  # zsh-history-substring-search
  if [ ! -d "$ZSH_CUSTOM/plugins/zsh-history-substring-search" ]; then
    echo "$INFO Installing zsh-history-substring-search plugin..."
    git clone https://github.com/zsh-users/zsh-history-substring-search "$ZSH_CUSTOM/plugins/zsh-history-substring-search"
  else
    echo "$INFO zsh-history-substring-search already installed"
  fi
  
  # zsh-syntax-highlighting
  if [ ! -d "$ZSH_CUSTOM/plugins/zsh-syntax-highlighting" ]; then
    echo "$INFO Installing zsh-syntax-highlighting plugin..."
    git clone https://github.com/zsh-users/zsh-syntax-highlighting.git "$ZSH_CUSTOM/plugins/zsh-syntax-highlighting"
  else
    echo "$INFO zsh-syntax-highlighting already installed"
  fi

  install_dotfile "zsh" "Zsh"
  
  # Change default shell to zsh
  if [ "$SHELL" != "/usr/bin/zsh" ]; then
    echo "$INFO Changing default shell to Zsh..."
    chsh -s /usr/bin/zsh
  fi
}

install_starship() {
  install_dotfile "starship" "Starship"
}

install_tmux() {
  install_dotfile "tmux" "Tmux"
}

# ============================================
# EDITORS
# ============================================

install_doom() {
  if [ -d "$HOME/.config/emacs" ]; then
    echo "$INFO Doom already installed, updating settings..."
    stow -R doom
    ~/.config/emacs/bin/doom sync
  else
    echo "$INFO Cloning Doom Emacs..."
    git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs
    
    echo "$INFO Adding Doom settings..."
    stow doom
    
    echo "$INFO Installing Doom..."
    ~/.config/emacs/bin/doom install
  fi
  echo "$OK Doom installation completed!"
}

install_daemon_emacs() {
  install_dotfile "daemon-emacs" "Emacs Daemon"
}

# ============================================
# SYSTEM TOOLS
# ============================================

install_htop() {
  install_dotfile "htop" "htop"
}

install_bat() {
  install_dotfile "bat" "bat"
}

install_fastfetch() {
  install_dotfile "fastfetch" "Fastfetch"
}

install_paru() {
  install_dotfile "paru" "Paru"
}

install_rmpc() {
  install_dotfile "rmpc" "rmpc (MPD client)"
}

# ============================================
# WINDOW MANAGER / COMPOSITOR
# ============================================

install_niri() {
  install_dotfile "niri" "Niri"
}

install_noctalia() {
  install_dotfile "noctalia" "Noctalia Shell"
}

# ============================================
# INSTALL ALL DOTFILES
# ============================================

install_all_dotfiles() {
  echo "$INFO Installing all dotfiles..."
  echo "-----"
  
  # Terminal emulators
  install_alacritty
  install_kitty
  
  # Shell & prompt
  install_zsh
  install_starship
  install_tmux
  
  # System tools
  install_htop
  install_bat
  install_fastfetch
  install_paru
  install_rmpc
  
  # Window manager
  install_niri
  install_noctalia
  
  # Editors
  install_doom
  # install_daemon_emacs

  echo "-----"
  echo "$OK All dotfiles configured!"
}