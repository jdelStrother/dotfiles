# Defined in - @ line 0
function flow --description 'alias flow=node_modules/.bin/flow'
  if test -e node_modules/.bin/flow
    node_modules/.bin/flow  $argv;
  else
    env flow $argv;
  end
end
