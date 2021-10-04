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

## 👷 Building


```sh
git clone https://aur.archlinux.org/paths-bookmarks-git.git
cd paths-bookmarks-git
makepkg -si
```

Then add the following line to your .bashrc:

```sh
source /usr/share/paths/paths.sh
```

## 🔧 Config file

- pat.hs stores the bookmarks in `$HOME/.paths-bookmarks`
- You can make direct edits to it, but if you break the format, the program will not launch until you fix it again
