# Setup fzf
# ---------
if [[ ! "$PATH" == */home/baban/.fzf/bin* ]]; then
  export PATH="${PATH:+${PATH}:}/home/baban/.fzf/bin"
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "/home/baban/.fzf/shell/completion.bash" 2> /dev/null

# Key bindings
# ------------
source "/home/baban/.fzf/shell/key-bindings.bash"
