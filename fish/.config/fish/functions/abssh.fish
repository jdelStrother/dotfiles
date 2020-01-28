function abssh
  if test "$argv[1]" = "--web"
    set argv $argv[2..-1]
    set hosts py02 py04 web01 web02 web01b web02b app1-1 app1-2 app1-3 app1-4 app1-5
  else if test "$argv[1]" = "--dj"
    set argv $argv[2..-1]
    set hosts dj1 dj2 dj1-1 dj1-2
  else if test "$argv[1]" = "--balancer"
    set argv $argv[2..-1]
    set hosts balancer1 balancer2
  else if test "$argv[1]" = "--redis"
    set argv $argv[2..-1]
    set hosts redis1-1 redis1-2 redis1-3 ubuntu@app1-1 ubuntu@app1-2 ubuntu@app1-3
  else if string match -- '--*' "$argv[1]"
    echo "unrecognized host option" 1>&2
    return 1
  else
    set hosts py02 py04 web01 web02 balancer1 balancer2 dj1 dj2 dj1-1 dj1-2 app1-1 app1-2 app1-3 app1-4 app1-5 redis1-1 redis1-2 redis1-3
  end
  pssh -H "$hosts" $argv
end
