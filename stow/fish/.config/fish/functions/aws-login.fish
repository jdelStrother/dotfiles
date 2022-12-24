function aws-login
  # by default aws-vault generates session names with timestamps,
  # which is annoying for Quicksight which then sees every login as a fresh user
  export AWS_ROLE_SESSION_NAME=jdelStrother

  # url-encode the login url and use https://addons.mozilla.org/en-GB/firefox/addon/open-url-in-container/
  # to open it in a specific container
  set -l loginurl (aws-vault login --duration=8h --stdout "$argv[1]" | jq -sRr @uri)
  if test -z $loginurl
    return 1
  end

  set url $(printf 'ext+container:name=%s&url=%s' "aws-$argv[1]" "$loginurl")
  open -a Firefox.app "$url"
end
