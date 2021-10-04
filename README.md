# 📚 pat.hs

CLI utility for saving often used directories under a short name

## 📋 Usage

`paths list` 
  - Lists all of your bookmarks

`paths save KEY` 
  - Saves the current directory under the given `KEY`

`paths get KEY`
  - Prints the path associated with the given `KEY`

`paths delete KEY`
  - Deletes the `KEY` and its associated value from the [config file](#config-file)

`paths go KEY[/PATH]` 
  - Changes the working directory to the one associated with `KEY`
  - Also goes into subdirectories specified with `PATH`

## 💾 Installation

- Available for Arch Linux only
- For other distributions, see the [building](#-building) section
- Install `paths-bookmarks-git` using your favorite AUR helper

## 👷 Building

Haskell Stack needs to be installed and on the path to install manually.


```sh
git clone https://github.com/sproott/pat.hs.git
cd pat.hs

mkdir bin
stack --local-bin-dir "bin/" install 

install -Dm755 "bin/paths-exe" "/usr/share/paths/paths-exe"
install -Dm755 "paths.sh" "/usr/share/paths/paths.sh"

install -Dm644 "doc/completions/bashcompletion.sh" "/usr/share/bash-completion/completions/paths"
```

Then add the following line to your .bashrc:

```sh
source /usr/share/paths/paths.sh
```

## 🔧 Config file

- pat.hs stores the bookmarks in `$HOME/.paths-bookmarks`
- You can make direct edits to it, but if you break the format, the program will not launch until you fix it again
