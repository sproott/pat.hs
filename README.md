# 📚 pat.hs

CLI utility for saving often used directories under a short name

## 📋 Usage

`paths list`

- Lists all of your bookmarks

`paths save KEY`

- Saves the current directory under the given `KEY`

`paths get KEY`

- Prints the path associated with the given `KEY`

`paths remove KEY`

- Removes the `KEY` and its associated value from the [bookmarks file](#-bookmarks-file)

`paths rename KEY NEW_KEY`

- Renames the bookmark associated with `KEY` to `NEW_KEY`

`paths go KEY[/PATH]`

- Changes the working directory to the one associated with `KEY`
- Also goes into subdirectories specified with `PATH`

## 💾 Installation

- Available for Arch Linux only
- For other distributions, see the [building](#-building) section
- Install `paths-bookmarks-git` using your favorite AUR helper

## Post-installation

### Bash

Add the following line to your .bashrc:

```sh
source /usr/share/paths/paths.sh
```

### Fish

Add the following line to your config.fish:

```sh
source /usr/share/paths/paths.fish
```

## 👷 Building

Haskell Stack needs to be installed and on the path to install manually.

```sh
git clone https://github.com/sproott/pat.hs.git
cd pat.hs

mkdir bin
stack --local-bin-path "bin/" install

sudo install -Dm755 "bin/paths" "/usr/share/paths/paths"
sudo install -Dm755 "paths.sh" "/usr/share/paths/paths.sh"
sudo install -Dm755 "paths.fish" "/usr/share/paths/paths.fish"

sudo install -Dm644 "doc/completions/bashcompletion.sh" "/usr/share/bash-completion/completions/paths"
sudo install -Dm644 "doc/completions/fishcompletion.fish" "/usr/share/fish/completions/paths.fish"
```

## 🔧 Bookmarks file

- pat.hs stores the bookmarks in a `.bookmarks` file inside `$XDG_DATA_HOME/paths` or inside `$HOME/.local/share/paths`, if `$XDG_DATA_HOME` is not defined
- You can make direct edits to it, but if you break the format, the program will not launch until you fix it again

## 🍲 Recipes

For some usage tips, see [recipes](doc/recipes/)
