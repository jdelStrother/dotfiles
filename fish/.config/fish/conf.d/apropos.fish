# Apparently this should be unnecessary in fish > 3.1.2
# https://gist.github.com/folke/cded312c972ff3164dd405aad423b00f

set db ~/.whatis.db

function apropos_update
    echo "Updating apropos / whatis database at $db"
    man --path | tr ":" " " | xargs /usr/libexec/makewhatis -o $db
end

function apropos
    [ -f $db ] || apropos_update
    /usr/bin/grep -i "$argv" $db
end

function whatis
    [ -f $db ] || apropos_update
    /usr/bin/grep -i "\b$argv\b" $db
end
