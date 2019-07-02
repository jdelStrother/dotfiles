function webpack-analyze
  if test -z "$argv[1]"
    echo "filename missing"
    return 1
  end

  env NODE_ENV=production node_modules/.bin/webpack --bail --config webpack.production.config.js  --profile --json > "/tmp/$argv[1]"
  and webpack-bundle-analyzer "/tmp/$argv[1]" ~/Developer/web/app/assets/javascripts/packages
end
