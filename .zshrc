#
# Zanes .zshrc
#

# History
HISTFILE=~/.history
HISTSIZE=9999
SAVEHIST=9999
LISTMAX=9999
eval `dircolors -b`

setopt append_history inc_append_history extended_history hist_find_no_dups hist_ignore_all_dups
setopt hist_reduce_blanks hist_ignore_space hist_no_store hist_save_no_dups numeric_glob_sort
setopt chase_dots auto_list auto_pushd pushd_silent multios rm_star_wait no_clobber dotglob
setopt appendhistory autocd notify nohup automenu alwayslastprompt listtypes print_exit_value
setopt prompt_subst extendedglob

# Git Prompt
# Partially from OH-MY-ZSH

PROMPT='%F{white}%m (%D{%I:%M%P}) %f%b%c%B%F{black}/%f%b $(git_prompt_info)%F{blue}%#%f '

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
autoload -U compinit
compinit

# Fix kill-backward-word
export WORDCHARS=''

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
export PATH=~/bin:~/Documents/bin:~/Downloads/android-ndk-r6b:$PATH

export BROWSER=firefox
export EDITOR='gvim -f'
export PAGER='less'

# Handy aliases
alias ls='ls --color -FCvh --group-directories-first' l='ls' sl='ls' ll='ls -l'
alias less='less -r'
alias mkdir='mkdir -p'

# Other aliases
alias ec='emacsclient -nc'
alias docco='rocco'
alias gcc='colorgcc'
alias csi='rlwrap csi'
alias mysql='mysql --auto-rehash --auto-vertical-output --sigint-ignore'

# Be verbose
for c in cp rm chmod chown rename; do
  alias $c="$c -v"
done

expand-or-complete-with-dots() {
  echo -n "\e[31m......\e[0m"
  zle expand-or-complete
  zle redisplay
}
zle -N expand-or-complete-with-dots
bindkey "^I" expand-or-complete-with-dots

source ~/.profile
source ~/Documents/dotfiles/live-command-coloring.sh
source ~/Documents/dotfiles/z/z.sh

# Fix keyboard handling
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
bindkey "^[OH" beginning-of-line
bindkey "^[OF" end-of-line
# completion in the middle of a line
bindkey '^i' expand-or-complete-prefix

# No freezing tty on ^s
set -5
stty stop ''
