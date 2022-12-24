function __get_rake_completions -d "Get rake completions"
  set tool rake
  if test -f bin/rake
    set tool bin/rake
  end
  $tool -T 2>&1 | sed -e "s/^rake \([a-z:_0-9!\-]*\).*#\(.*\)/\1	\2/"
end
complete -c rake --no-files -a "(__get_rake_completions)"
