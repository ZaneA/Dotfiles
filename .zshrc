#
# Zanes .zshrc
#

# History
HISTFILE=~/.history
HISTSIZE=1000
SAVEHIST=1000

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
setopt appendhistory autocd notify hist_ignore_all_dups nohup automenu alwayslastprompt listtypes print_exit_value prompt_subst
autoload -U compinit
compinit

# Set up PATH
PATH=~/bin/:~/Documents/bin/:/var/lib/gems/1.8/bin/:$PATH

BROWSER=firefox

# Handy aliases
alias ls='ls --color=auto -FCvXh --group-directories-first' l='ls' sl='ls' ll='ls -l' la='ls -a' lla='ll -a'
alias ec='emacsclient -nc'

# Not so handy aliases
alias fuckin='sudo' fucking='sudo'

bindkey "^[[7~" beginning-of-line
bindkey "^[[8~" end-of-line

stty stop ''
