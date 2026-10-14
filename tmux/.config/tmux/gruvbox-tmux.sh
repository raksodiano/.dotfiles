#!/usr/bin/env bash
# Recolorea el tema Tokyo Night -> Gruvbox Material Hard
# Se ejecuta DESPUES de tpm, sobre los formatos que el plugin ya定义io.
# Solo sustituye codigos de color: los widgets dinamicos se conservan.

set -u

declare -A MAP=(
  ["#1A1B26"]="#1d2021"   # background -> surface
  ["#2A2F41"]="#282828"   # bblack    -> surface_variant
  ["#a9b1d6"]="#ebdbb2"   # foreground-> on_surface
  ["#7aa2f7"]="#a9b665"   # blue      -> primary
  ["#73daca"]="#89b482"   # cyan      -> tertiary
  ["#41a6b5"]="#00aa84"   # cyan2     -> tertiary oscuro
  ["#e0af68"]="#d8a657"   # yellow    -> secondary
)

recolor() {
  local opt="$1" current sed_args=() from to
  current="$(tmux show -gv "$opt" 2>/dev/null)" || return 0
  [ -n "$current" ] || return 0

  for from in "${!MAP[@]}"; do
    to="${MAP[$from]}"
    sed_args+=(-e "s/${from}/${to}/gI")
  done

  printf -v current '%s' "$current"
  local updated
  updated="$(printf '%s' "$current" | sed "${sed_args[@]}")"
  [ "$updated" != "$current" ] && tmux set -g "$opt" "$updated"
  return 0
}

for opt in status-left status-right window-status-current-format window-status-format; do
  recolor "$opt"
done

tmux set -g status-style        "bg=#1d2021,fg=#ebdbb2"
tmux set -g message-style       "bg=#a9b665,fg=#111826"
tmux set -g mode-style          "bg=#a9b665,fg=#111826"
tmux set -g pane-border-style   "fg=#282828"
tmux set -g pane-active-border-style "fg=#a9b665"
tmux set -g window-status-separator ""