
# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/leon/mambaforge/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/leon/mambaforge/etc/profile.d/conda.sh" ]; then
        . "/home/leon/mambaforge/etc/profile.d/conda.sh"
    else
        export PATH="/home/leon/mambaforge/bin:$PATH"
    fi
fi
unset __conda_setup

if [ -f "/home/leon/mambaforge/etc/profile.d/mamba.sh" ]; then
    . "/home/leon/mambaforge/etc/profile.d/mamba.sh"
fi
# <<< conda initialize <<<

export LD_LIBRARY_PATH=$HOME/mambaforge/lib:$LD_LIBRARY_PATH

export XDG_RUNTIME_DIR="/tmp/runtime-leon"
export INFOPATH=$INFOPATH:/usr/share/info:/usr/local/share/info:/home/leon/mambaforge/share/info

# pyenv
export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"


# zmq
export LD_LIBRARY_PATH=$HOME/local/zeromq/lib:$LD_LIBRARY_PATH
export PKG_CONFIG_PATH=$HOME/local/zeromq/lib/pkgconfig:$PKG_CONFIG_PATH

# local
export PATH="$HOME/local/bin:$PATH"
export PATH="$HOME/local/libexec/:$PATH"

export LD_LIBRARY_PATH="$HOME/local/usr/lib:$LD_LIBRARY_PATH"
export LD_LIBRARY_PATH="$HOME/local/usr/lib64:$LD_LIBRARY_PATH"

export MANPATH="$HOME/local/share/man:$MANPATH"
export CPLUS_INCLUDE_PATH=$CPLUS_INCLUDE_PATH:"$HOME/local/include/"

PATH="/home/leon/local/perl5/bin${PATH:+:${PATH}}"; export PATH;
PERL5LIB="/home/leon/local/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="/home/leon/local/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"/home/leon/local/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/home/leon/local/perl5"; export PERL_MM_OPT;

# We're in Emacs, yo
export VISUAL=emacsclient
export EDITOR="$VISUAL"

# Load .bashrc to get login environment
# [ -f ~/.bashrc ] && . ~/.bashrc
