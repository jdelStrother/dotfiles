function aws-login
  # url-encode the login url and use https://addons.mozilla.org/en-GB/firefox/addon/open-url-in-container/
  # to open it in a specific container
  set -l loginurl (aws-vault login --duration=8h --stdout "$argv[1]" | jq -sRr @uri)
  if test -z $loginurl
    return 1
  end

  set url $(printf 'ext+container:name=%s&url=%s' "aws-$argv[1]" "$loginurl")
  open -a Firefox.app "$url"
end
