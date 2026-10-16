# Justfile for this dotfiles repository. Run recipes from ~/.dotfiles.
# Usage: just <recipe>  (list everything with `just --list`)
#
# NOTE: lazygit and syncthing are NOT stowed (real ~/.config dirs:
# lazygit keeps local state, syncthing keeps private keys).
# zsh is a manual ~/.zshrc symlink, also kept out of stow-all.

# Re-link every stowed package (safe: stow -R only rewrites symlinks).
stow-all:
    stow -R bat btop emacs fastfetch hyprland kitty lazydocker mpd mpv noctalia paru rmpc starship tmux tridactyl yazi

# Re-link a single package: just stow yazi
stow pkg:
    stow -R {{pkg}}

# Verify the Emacs config loads clean (expects 25/25 modules).
verify-emacs:
    emacs -Q --batch -L ~/.config/emacs -l ~/.config/emacs/early-init.el --eval '(setq my/config-dir "~/.config/emacs/" my/assets-dir (expand-file-name "assets" my/config-dir))' --eval '(load (expand-file-name "init.el" my/config-dir) nil t)' --eval '(my/init-status)'

# Full system upgrade with paru.
update:
    paru -Syu

# Drop regenerable caches owned by the Emacs config.
clean-emacs-caches:
    rm -rf ~/.cache/emacs/eln-cache/* ~/.cache/emacs/url/cache/*
