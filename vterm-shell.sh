#!/usr/bin/env zsh
# emacs shell-side
if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
   if command -v realpath >/dev/null 2>&1; then
       vterm_resolve_path() {
           realpath "${1:-.}"
       }
   elif command -v python3 >/dev/null 2>&1; then
       vterm_resolve_path() {
           python3 - "${1:-.}" <<'PY'
import os, sys
print(os.path.realpath(sys.argv[1]))
PY
       }
   elif command -v python >/dev/null 2>&1; then
       vterm_resolve_path() {
           python - "${1:-.}" <<'PY'
import os, sys
print(os.path.realpath(sys.argv[1]))
PY
       }
   else
       vterm_resolve_path() {
           local target="${1:-.}"
           print -r -- "${target:A}"
       }
   fi

   vterm_printf() {
       if [ -n "$TMUX" ] \
              && { [ "${TERM%%-*}" = "tmux" ] \
		       || [ "${TERM%%-*}" = "screen" ]; }; then
           # Tell tmux to pass the escape sequences through
           printf "\ePtmux;\e\e]%s\007\e\\" "$1"
       elif [ "${TERM%%-*}" = "screen" ]; then
           # GNU screen (screen, screen-256color, screen-256color-bce)
           printf "\eP\e]%s\007\e\\" "$1"
       else
           printf "\e]%s\e\\" "$1"
       fi
   }

   vterm_prompt_end() {
       vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
   }
   setopt PROMPT_SUBST
   PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'

   vterm_cmd() {
       local vterm_elisp
       vterm_elisp=""
       while [ $# -gt 0 ]; do
           vterm_elisp="$vterm_elisp""$(printf '"%s" ' "$(printf "%s" "$1" | sed -e 's|\\|\\\\|g' -e 's|"|\\"|g')")"
           shift
       done
       vterm_printf "51;E$vterm_elisp"
   }

   find_file() {
       vterm_cmd find-file "$(vterm_resolve_path "${@:-.}")"
   }
fi
