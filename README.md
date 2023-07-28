# .dotfiles
This is the dotfiles I usually use for my configuration.

Clone in home directory and run:

```bash
stow .
```

Beware stow . creates direct links to the parent folder not directly to $HOME.
Also, it will only create links if the files are not already in that directory.
