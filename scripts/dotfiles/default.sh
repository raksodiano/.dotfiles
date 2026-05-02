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
  fi
  
  # zsh-history-substring-search
  if [ ! -d "$ZSH_CUSTOM/plugins/zsh-history-substring-search" ]; then
    echo "$INFO Installing zsh-history-substring-search plugin..."
    git clone https://github.com/zsh-users/zsh-history-substring-search "$ZSH_CUSTOM/plugins/zsh-history-substring-search"
  fi
  
  # zsh-syntax-highlighting
  if [ ! -d "$ZSH_CUSTOM/plugins/zsh-syntax-highlighting" ]; then
    echo "$INFO Installing zsh-syntax-highlighting plugin..."
    git clone https://github.com/zsh-users/zsh-syntax-highlighting.git "$ZSH_CUSTOM/plugins/zsh-syntax-highlighting"
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
  # Install TPM (Tmux Plugin Manager) if not present
  if [ ! -d "$HOME/.tmux/plugins/tpm" ]; then
    echo "$INFO Installing TPM (Tmux Plugin Manager)..."
    git clone https://github.com/tmux-plugins/tpm "$HOME/.tmux/plugins/tpm"
  fi

  # Stow tmux configuration
  install_dotfile "tmux" "Tmux"

  # Install all plugins via TPM 
  if command -v tmux &> /dev/null; then
    echo "$INFO Installing Tmux plugins..."
    local TPM_SOCKET="tpm_install_$$"
    # Start temporary tmux server to avoid conflicts with existing sessions
    tmux -L "$TPM_SOCKET" new-session -d -s tpm_install 2>/dev/null
    # Source tmux config to load plugin list and initialize TPM
    tmux -L "$TPM_SOCKET" source-file ~/.config/tmux/tmux.conf 2>/dev/null
    # Run TPM's plugin installation script
    tmux -L "$TPM_SOCKET" run-shell ~/.tmux/plugins/tpm/scripts/install_plugins.sh 2>/dev/null
    # Clean up temporary tmux server
    tmux -L "$TPM_SOCKET" kill-server 2>/dev/null
    echo "$OK Tmux plugins installed and ready to use!"
  else
    echo "$WARN Tmux is not installed, skipping plugin installation."
  fi
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

# ============================================
# SYSTEM TOOLS
# ============================================

install_bat() {
  install_dotfile "bat" "bat"
}

install_fastfetch() {
  install_dotfile "fastfetch" "Fastfetch"
}

# ============================================
# INSTALL ALL DOTFILES
# ============================================

install_all_dotfiles() {
  echo "$INFO Installing all dotfiles..."
  echo "-----"
  
  # Shell & prompt
  install_zsh
  install_starship
  install_tmux
  
  # System tools
  install_bat
  install_fastfetch
  
  # Editors
  install_doom

  echo "-----"
  echo "$OK All dotfiles configured!"
}
