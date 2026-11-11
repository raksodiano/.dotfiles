#!/usr/bin/env bash
# ============================================================================
# Paquetes de Arch Linux para este repositorio de dotfiles
#
#   PACMAN_PKGS : paquetes de los repositorios oficiales (pacman)
#   AUR_PKGS    : paquetes de AUR (paru)
#
# Uso:
#   ./scripts/arch-packages.sh --dry-run   # solo muestra la lista
#   ./scripts/arch-packages.sh             # instala todo
#
# Notas:
#   - paru se compila automáticamente si no está instalado.
#   - TPM y Doom se instalan por git desde install.sh; no se repiten aquí.
#     install.sh; no se repiten aquí para no duplicar.
#   - steam requiere habilitar [multilib] en /etc/pacman.conf.
#   - xdg-desktop-portal-gnome (compartir pantalla) arrastra nautilus; es
#     opcional. Sin él, niri usa xdg-desktop-portal-gtk.
#   - Paquetes pesados: texlive-meta, calibre, nixd (arrastra nix y llvm)
#     y antigravity. Coméntalos si no los necesitas.
#   - Equivalencias Fedora -> Arch usadas:
#       fd-find -> fd, python3 -> python, python3-pip -> python-pip,
#       poppler-utils -> poppler, libvterm-devel -> libvterm,
#       7zip-standalone -> 7zip, texlive-scheme-full -> texlive-meta,
#       pandoc -> pandoc-cli, brave-browser -> brave-bin,
#       codium -> vscodium-bin, librewolf-bin -> librewolf (extra),
#       ht-alegreya-fonts -> ttf-alegreya (AUR).
# ============================================================================
set -euo pipefail

PACMAN_PKGS=(
  # --- Base, compilación y utilidades -------------------------------------
  base-devel
  git
  stow
  curl
  wget
  openssh
  unzip
  zip
  7zip
  unrar
  bzip2
  libarchive
  man-db

  # --- Red, audio, energía y Wayland --------------------------------------
  networkmanager
  bluez
  bluez-utils
  upower
  power-profiles-daemon
  polkit
  pipewire
  pipewire-pulse
  pipewire-alsa
  wireplumber
  xdg-desktop-portal-gtk
  xdg-utils
  xdg-user-dirs
  evtest             # bongocat plugin (keyboard reactivity)
  tailscale             # plugin Tailscale (daemon + CLI)
  shared-mime-info
  desktop-file-utils
  hicolor-icon-theme
  fontconfig

  # --- Niri + Noctalia -----------------------------------------------------
  niri
  xwayland-satellite
  noctalia
  greetd
  noctalia-greeter          # alternativa ligera: greetd-tuigreet
  qt6-wayland
  capitaine-cursors          # xcursor-theme de niri (cfg/misc.kdl)
  kitty
  firefox
  librewolf                 # navegador extra de packages.conf

  # --- Utilidades de Wayland ----------------------------------------------
  wl-clipboard
  cliphist
  brightnessctl
  playerctl
  grim
  slurp
  satty
  swappy
  ddcutil
  gammastep
  lm_sensors

  # --- Shell, terminal y monitoreo ----------------------------------------
  starship
  atuin
  tmux
  brush
  bat
  eza
  zoxide
  fzf
  fd
  ripgrep
  btop
  fastfetch
  figlet
  sl

  # --- Archivos, correo y sincronización ----------------------------------
  pass
  pwgen
  isync
  msmtp
  msmtp-mta
  neomutt
  profanity
  syncthing
  task                   # taskwarrior (plugin Task Warrior)

  # --- Emacs / Doom --------------------------------------------------------
  emacs
  libvterm
  tree-sitter
  tree-sitter-cli
  git-delta
  make
  gcc
  cmake
  clang                   # incluye clangd
  lldb
  gdb
  sbcl
  python
  python-pip
  python-pipx             # instalación aislada de herramientas Python
  python-debugpy
  rust                    # incluye cargo y rustfmt
  go
  delve
  nodejs
  npm
  poppler
  poppler-glib
  pandoc-cli
  graphviz
  gnuplot
  hugo
  terraform
  texlive-meta            # esquema completo (pesado)
  calibre                 # calibredb (pesado)
  shellcheck
  shfmt
  prettier
  python-black
  ruff
  enchant
  hunspell
  hunspell-en_us
  hunspell-es_co
  hunspell-es_es
  aspell
  aspell-en
  aspell-es

  # --- Servidores LSP ------------------------------------------------------
  gopls
  pyright
  typescript
  typescript-language-server
  rust-analyzer
  marksman                # LSP de Markdown
  texlab                  # LSP de LaTeX
  dockerfile-language-server
  bash-language-server
  yaml-language-server

  # --- Contenedores --------------------------------------------------------
  docker
  docker-compose
  containerd
  docker-buildx

  # --- Aplicaciones --------------------------------------------------------
  telegram-desktop
  vlc
  mpv
  mpd
  mpc
  rmpc
  yt-dlp
  yazi
  ffmpegthumbnailer
  imagemagick
  jq
  chafa
  resvg
  zathura
  zathura-pdf-poppler
  lazygit
  lazydocker
  thunderbird            # plugin Thunderbird Companion (native messaging)

  # --- Códecs y aceleración Intel -----------------------------------------
  ffmpeg
  libva-intel-driver
  intel-media-driver
  libva-utils
  openh264
  gst-plugins-good
  gst-plugins-bad
  gst-plugins-ugly
  gst-libav

  # --- Tipografías ---------------------------------------------------------
  noto-fonts
  noto-fonts-emoji
  ttf-nerd-fonts-symbols
  ttf-nerd-fonts-symbols-mono
  ttf-3270-nerd
  otf-geist-mono-nerd     # fuente de kitty
  ttf-hack-nerd
  ttf-jetbrains-mono-nerd
  ttf-firacode-nerd

  # --- Opcionales de packages.conf que no usa la configuración actual -----
  # flameshot             # capturas X11; niri ya tiene screenshot + satty
  # bleachbit             # limpieza del sistema
  # steam                 # requiere [multilib] en /etc/pacman.conf
)

AUR_PKGS=(
  # paru (se compila antes si no existe)
  paru

  # Shell completions
  carapace-bin
  # Navegadores / editores de packages.conf
  brave-bin
  vscodium-bin
  antigravity

  # Correo (mu4e)
  mu

  # Tipografías Alegreya (packages.conf: ht-alegreya-fonts)
  ttf-alegreya
  ttf-alegreya-sans

  # Emacs: procesos en segundo plano
  dtach                   # requerido por detached.el

  # LSP / formato para los módulos de Doom
  nixd
  terraform-ls
  # nixfmt               # formateo de Nix (build Haskell pesado)

  # CLIs opcionales para agent-shell (ACP):
  # claude-code
  # gemini-cli-live-bin
  # codex                # vía npm: npm install -g @openai/codex

  # Alternativas gestionadas por install.sh (no necesarias si se usa el script):
  # oh-my-zsh-git
  # tmux-plugin-manager
)

usage() {
  cat <<'EOF'
Uso: ./scripts/arch-packages.sh [opción]

Opciones:
  -n, --dry-run   Muestra la lista de paquetes sin instalar nada.
  -h, --help      Muestra esta ayuda.

Sin opciones instala los paquetes oficiales con pacman y los de AUR con paru.
EOF
}

print_packages() {
  local title="$1"
  shift
  printf '\n%s (%d):\n' "$title" "$#"
  printf '  %s\n' "$@"
}

DRY_RUN=false
case "${1:-}" in
  -n|--dry-run) DRY_RUN=true ;;
  -h|--help)    usage; exit 0 ;;
  "")           ;;
  *)
    echo "Opción desconocida: $1" >&2
    usage >&2
    exit 1
    ;;
esac

if $DRY_RUN; then
  print_packages "Paquetes oficiales (pacman)" "${PACMAN_PKGS[@]}"
  print_packages "Paquetes AUR (paru)" "${AUR_PKGS[@]}"
  exit 0
fi

if ! command -v pacman >/dev/null 2>&1; then
  echo "Error: pacman no encontrado. Este script es para Arch Linux." >&2
  exit 1
fi

if [[ $EUID -eq 0 ]]; then
  echo "Error: no ejecutes este script como root; paru no debe usarse con sudo." >&2
  exit 1
fi

# paru no puede instalarse a sí mismo, así que se compila una sola vez.
if ! command -v paru >/dev/null 2>&1; then
  echo "==> paru no está instalado. Compilando desde AUR..."
  sudo pacman -S --needed base-devel git

  tmpdir="$(mktemp -d)"
  trap 'rm -rf "$tmpdir"' EXIT

  git clone --depth 1 https://aur.archlinux.org/paru.git "$tmpdir/paru"
  (cd "$tmpdir/paru" && makepkg -si)
fi

echo "==> Instalando paquetes oficiales con pacman..."
sudo pacman -S --needed "${PACMAN_PKGS[@]}"

echo "==> Instalando paquetes AUR con paru..."
paru -S --needed "${AUR_PKGS[@]}"

cat <<'EOF'

Listo. Servicios recomendados (actívalos manualmente):

  sudo systemctl enable --now NetworkManager
  sudo systemctl enable --now bluetooth
  sudo systemctl enable --now docker
  sudo systemctl enable greetd

  systemctl --user enable --now pipewire pipewire-pulse wireplumber

  # Opcionales:
  systemctl --user enable --now syncthing
  systemctl --user enable --now mpd
  systemctl --user enable --now emacs
  sudo systemctl enable --now tailscaled   # plugin Tailscale

Opcional:
  - Habilita [multilib] en /etc/pacman.conf y luego: sudo pacman -S steam
  - Para compartir pantalla: sudo pacman -S xdg-desktop-portal-gnome
  - Después ejecuta ./install.sh para desplegar los dotfiles con stow.
EOF
