function abjenkins-whitelist-ip
  set groupid (aws ec2 describe-security-groups --filters 'Name=group-name,Values="amazon-ecs-cli-setup-jenkins-EcsSecurityGroup-1V2KTFR4JKWJ0"' --query 'SecurityGroups[0].GroupId' --output text)
  set cidr (curl --silent ifconfig.me)/32
  echo aws ec2 authorize-security-group-ingress --group-id $groupid --protocol tcp --port 443 --cidr $cidr
  aws ec2 authorize-security-group-ingress --group-id $groupid --protocol tcp --port 443 --cidr $cidr
  echo "IP Authorized! You should run this when you're done:"
  echo aws ec2 revoke-security-group-ingress --group-id $groupid --protocol tcp --port 443 --cidr $cidr
end
