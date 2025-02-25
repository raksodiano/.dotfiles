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

### 3. Carpetas para uso de notas

``` bash
mkdir -p ~/Org/notes/{task,note,hugo,diario}
```
