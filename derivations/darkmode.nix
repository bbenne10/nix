{ writeScriptBin }: writeScriptBin "darkmode"
  ''
    if [ $(uname -s) = "Darwin" ]; then
      osascript_stem='tell application "System Events" to tell appearance preferences to'
      usage="usage: $0\nusage: $0 [dark|light]"
    
      if [ $# == 0 ]; then
          if [ "$(osascript -e "$osascript_stem return dark mode")" == "true" ]
          then
              echo dark
          else
              echo light
          fi
      elif [ $# == 1 ]; then
          case "$1" in
              dark)
                  osascript -e "$osascript_stem set dark mode to true";;
              light)
                  osascript -e "$osascript_stem set dark mode to false";;
              *)
                  echo $usage 1>&2
                  exit 1
          esac
      else
          echo $usage 1>&2
          exit 1
      fi
    else
      echo "Don't know how to toggle darkmode on $(uname -s)! Teach me!"
      exit 1
    fi
  ''
