# Customise bash prompt:
## Function to check if previous command fails/succeeds
function __stat() { 
    if [ $? -eq 0 ]; then 
        echo -en "\e[32m[\u2714]\e[m " 
    else 
        echo -en "\e[31m[\u2718]\e[m " 
    fi 
}

## source script to have git branch in prompt
source ~/.git-prompt.sh

## Options for git-prompt
GIT_PS1_SHOWDIRTYSTATE=true
GIT_PS1_SHOWUPSTREAM="auto"
GIT_PS1_SHOWCOLORHINTS=true
GIT_PS1_STATESEPARATOR=" "

## Bash prompt
PS1=$'\u250F'	# Elbow
PS1+='[\T]'	# Time
PS1+=' '	# Space 
PS1+='\u@\h'	# User@hostname
PS1+=' ' 	# Space
PS1+='\[\e[31m\]\w\[\e[m\]'	# current dir
PS1+=' '	# Space
PS1+='\[\e[96m\]$(__git_ps1 "[%s]")\[\e[m\]'	# git branch
PS1+=' '	# Space
PS1+='$(__stat)'	# Previous command success
PS1+='\n'	# New line
PS1+=$'\u2517'	# Elbow
PS1+='\$'	# $
PS1+=' '	# Space

# Simple
# PS1='\$'

# Time
# PS1='\T \$'

# Coloured directory
# PS1='\[\e[31m\]\w\[\e[m\] \T \$'

# Coloured and bold directory
# PS1='\[\e[96;1m\]\w\[\e[m\] \T \$'

# Unicode umbrella
# PS1=$'\u2602'

# Making use of brackets for fortmatting
# PS1='[\T] {\u:\h} -- \w'

# Messy one liner with new line code
# PS1='┏[\T] \u@\h \[\e[31m\]\w\[\e[m\]\n┗$ '

# Using a function for command success
# PS1='$(__stat)'

# Sourcing an external shell script for git branch
# PS1='$(__git_ps1 "[%s]")'
