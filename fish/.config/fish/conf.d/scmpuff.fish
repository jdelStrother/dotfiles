set -x SCMPUFF_GIT_CMD (which git)

if type -q hub
    set -x SCMPUFF_GIT_CMD "hub"
end


if not type -q scmpuff
    exit 1
end

functions -e git
alias gs=scmpuff_status

function git
    type -q $SCMPUFF_GIT_CMD; or set -x SCMPUFF_GIT_CMD (which git)

    if test (count $argv) -eq 0
        eval $SCMPUFF_GIT_CMD
        set -l s $status
        return $s
    end

    switch $argv[1]
    case commit blame log rebase merge
        scmpuff exec -- "$SCMPUFF_GIT_CMD" $argv
    case checkout diff rm reset
        scmpuff exec --relative -- "$SCMPUFF_GIT_CMD" $argv
    case add
        scmpuff exec -- "$SCMPUFF_GIT_CMD" $argv
        scmpuff_status
    case '*'
        eval command "$SCMPUFF_GIT_CMD" (string escape -- $argv)
    end
end
