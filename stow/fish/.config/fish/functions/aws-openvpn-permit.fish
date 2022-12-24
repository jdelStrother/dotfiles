function aws-openvpn-permit
  set groupid (aws ec2 describe-security-groups --filters 'Name=group-name,Values="OpenVPN Access Server"' --query 'SecurityGroups[0].GroupId' --output text)
  set myip (curl --silent ifconfig.me)
  aws ec2 authorize-security-group-ingress --group-id "$groupid" --protocol tcp --port 22 --cidr "$myip/32"
  echo "You should run this when done:"
  echo aws ec2 revoke-security-group-ingress --group-id "$groupid" --protocol tcp --port 22 --cidr "$myip/32"
end
