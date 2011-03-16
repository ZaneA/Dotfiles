#
# Zanes .zshrc
#

# History
HISTFILE=~/.history
HISTSIZE=1000
SAVEHIST=1000
eval `dircolors -b`

setopt append_history inc_append_history extended_history hist_find_no_dups hist_ignore_all_dups
setopt hist_reduce_blanks hist_ignore_space hist_no_store hist_save_no_dups

# Git Prompt
# Partially from OH-MY-ZSH

PROMPT='%F{white}%m%B%F{black}:%f%b%c%B%F{black}/%f%b $(git_prompt_info)%F{blue}%#%f '

git_prompt_info() {
        ref=$(git symbolic-ref HEAD 2> /dev/null) || return
        echo "%F{blue}${ref#refs/heads/}$(parse_git_dirty)%f "
}

parse_git_dirty () {
        if [[ -n $(git status -s 2> /dev/null) ]]; then
                echo "%F{red}✗%f"
        else
                echo ""
        fi
}

# Command in window title
case $TERM in
    rxvt*|xterm*)
        #preexec () { print -Pn "\e]0;$1\a" }
        precmd () { print -Pn "\e]0;%n@%m: %~\a" }
        preexec () { print -Pn "\e]0;%n@%m: $1\a" }
    ;;
esac

# Options
setopt appendhistory autocd notify hist_ignore_all_dups nohup automenu alwayslastprompt listtypes print_exit_value prompt_subst extendedglob
autoload -U compinit
compinit

# allow approximate
zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*:match:*' original only
zstyle ':completion:*:approximate:*' max-errors 1 numeric

# tab completion for PID :D
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*' force-list always

zstyle ':completion:*:kill:*:processes' command "ps x"

# cd not select parent dir
zstyle ':completion:*:cd:*' ignore-parents parent pwd


# Set up PATH
export PATH=~/bin/:~/Documents/bin/:/var/lib/gems/1.8/bin/:$PATH

export BROWSER=firefox
export EDITOR=ec

# Handy aliases
alias ls='ls --color -FCvXh --group-directories-first' l='ls' sl='ls' ll='ls -l' la='ls -a' lla='ll -a'
alias less='less -r'
alias mkdir='mkdir -p'

alias ec='emacsclient -nc'

alias gcc='colorgcc'

alias csi='rlwrap csi'

# Not so handy aliases
alias fuckin='sudo' fucking='sudo'

source ~/Documents/dotfiles/live-command-coloring.sh
#source ~/Documents/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

#export GTK_MODULES=rgba:globalmenu-plugin
export GTK_MODULES=rgba
#export DE=gnome

typeset -g -A key
bindkey '^?' backward-delete-char
bindkey '^[[1~' beginning-of-line
bindkey '^[[5~' up-line-or-history
bindkey '^[[3~' delete-char
bindkey '^[[4~' end-of-line
bindkey '^[[6~' down-line-or-history
bindkey '^[[A' up-line-or-search
bindkey '^[[D' backward-char
bindkey '^[[B' down-line-or-search
bindkey '^[[C' forward-char 
bindkey "^[[7~" beginning-of-line
bindkey "^[[8~" end-of-line
# completion in the middle of a line
bindkey '^i' expand-or-complete-prefix

set -5
stty stop ''
