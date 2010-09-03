#
# Zanes .zshrc
#

# History
HISTFILE=~/.history
HISTSIZE=10000
SAVEHIST=10000

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

# Options
setopt appendhistory autocd notify hist_ignore_all_dups nohup automenu alwayslastprompt listtypes print_exit_value prompt_subst

# Set up PATH
PATH=$PATH:~/bin/:~/Documents/bin/

# Handy aliases
alias ls='ls --color=auto -FC' l='ls' sl='ls' ll='ls -l' la='ls -a' lla='ll -a'

# Not so handy aliases
alias fuckin='sudo' fucking='sudo'

stty stop ''
