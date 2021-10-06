# 🍲 Recipes

## Alias autocompletion

When you define an alias for a subcommand, bash completion doesn't work for it. To fix it: 

- Install [complete_alias](https://github.com/cykerway/complete-alias)
- Add the following to .bashrc: 

```sh
source /usr/share/bash-complete-alias/complete_alias
```

- Then define the alias and enable completion for it, for example:
  
```sh
alias go="paths go"
complete -F _complete_alias go
```
