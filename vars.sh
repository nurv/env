export CLICOLOR=1
export LC_CTYPE=pt_PT.UTF-8
export LC_ALL=pt_PT.UTF-8
export LSCOLORS="exfxcxdxbxegedabagacad"
export LS_COLORS='di=34;40:ln=35;40:so=32;40:pi=33;40:ex=31;40:bd=34;46:cd=34;43:su=0;41:sg=0;46:tw=0;42:ow=0;43:'
export GREP_COLOR='1;33'
export DYLD_LIBRARY_PATH=$DYLD_LIBRARY_PATH:/usr/local/mysql/lib/
export JAVA_OPTS="-server -Xms256m -Xmx1024m -XX:PermSize=384m"
export GPGKEY=B621B2CE
export MAVEN_OPTS="$JAVA_OPTS -Dorg.apache.jasper.compiler.Parser.STRICT_QUOTE_ESCAPING=false"

#editor
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='nano'
else
  if [[ $TERM == "xterm" || $TERM == "xterm-color" || $TERM == "rxvt" ]]; then
    alias EDITOR='nano'
  else
    alias EDITOR='nano'
  fi
fi
