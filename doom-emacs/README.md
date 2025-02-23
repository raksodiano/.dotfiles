### 1. Dependencias
```bash
sudo zypper install git emacs gnutls-devel libxml2-devel graphviz stow
```

### 2. Entorno Virtual Python para LSPs
```bash
# Python
python3 -m venv ~/.emacs-lsp-venv
source ~/.emacs-lsp-venv/bin/activate
pip install python-lsp-server pyright pylint black
deactivate

# JavaScript/TypeScript
npm install -g typescript-language-server vscode-langservers-extracted

# C/C++ (clangd)
sudo zypper install clang-tools-extra  # OpenSUSE

# Shell Script
npm install -g bash-language-server

```
