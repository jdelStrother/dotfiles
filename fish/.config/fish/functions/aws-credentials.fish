function aws-mfa
  # security add-generic-password -a jdelStrother -s "AWS OTP" -w
  set otpcode (security find-generic-password -a jdelStrother -s 'AWS OTP' -w); or exit $status
  oathtool --totp --base32 {$otpcode}
end

function aws-credentials
  set -e AWS_ACCESS_KEY_ID
  set -e AWS_SECRET_ACCESS_KEY
  set -e AWS_SESSION_TOKEN
  set temporary_credentials (aws sts get-session-token --token-code (aws-mfa) --serial-number arn:aws:iam::466056351294:mfa/jdelStrother)
  if test -n "$temporary_credentials"
    set -x -g AWS_ACCESS_KEY_ID (echo $temporary_credentials | jq -re '.Credentials.AccessKeyId')
    set -x -g AWS_SECRET_ACCESS_KEY (echo $temporary_credentials | jq -re '.Credentials.SecretAccessKey')
    set -x -g AWS_SESSION_TOKEN (echo $temporary_credentials | jq -re '.Credentials.SessionToken')
  end
end


