
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

export XDG_RUNTIME_DIR="/tmp/runtime-leon"
export INFOPATH=$INFOPATH:/usr/share/info:/usr/local/share/info:/home/leon/mambaforge/share/info

# pyenv
export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"

# zmq
export LD_LIBRARY_PATH=$HOME/zeromq/lib:$LD_LIBRARY_PATH
export PKG_CONFIG_PATH=$HOME/zeromq/lib/pkgconfig:$PKG_CONFIG_PATH

# local
export PATH="$HOME/local/bin:$PATH"
export LD_LIBRARY_PATH="$HOME/local/usr/lib:$LD_LIBRARY_PATH"
export MANPATH="$HOME/local/share/man:$MANPATH"

# pass
export PASSWORD_STORE_KEY='9AF6C15DD00119096AB69F304FCB14B913D7DA3C'

PATH="/home/leon/perl5/bin${PATH:+:${PATH}}"; export PATH;
PERL5LIB="/home/leon/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="/home/leon/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"/home/leon/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/home/leon/perl5"; export PERL_MM_OPT;
