#!/usr/bin/env fish

functions --copy __bobthefish_prompt_git __bobthefish_prompt_just_git

function __bobthefish_describe_jj
    set -l jj_status (jj log --ignore-working-copy --no-graph --color never -r @ -T '
      separate(
          " ",
          bookmarks.join(", "),
          change_id.shortest(),
          commit_id.shortest(),
          if(conflict, "(conflict)"),
          if(empty, "(empty)"),
          if(divergent, "(divergent)"),
          if(hidden, "(hidden)"),
      )
  ')
    echo $branch_glyph $jj_status
end

function __bobthefish_prompt_git --description 'Display the actual jj/git state' --no-scope-shadowing --argument git_root_dir real_pwd

    if [ \! -d "$git_root_dir/.jj" ]
        __bobthefish_prompt_just_git $git_root_dir $real_pwd
        return
    end


    set -l dirty ''
    set -l staged ''
    set -l stashed ''
    set -l ahead ''
    set -l new ''

    set -l flags "$dirty$staged$stashed$ahead$new"

    [ "$flags" ]
    and set flags " $flags"

    set -l flag_colors $color_repo

    __bobthefish_path_segment $git_root_dir

    __bobthefish_start_segment $flag_colors
    echo -ns $flags ' '
    echo -ns (__bobthefish_describe_jj) ' '

    if [ "$theme_git_worktree_support" != yes ]
        set -l project_pwd (__bobthefish_project_pwd $git_root_dir $real_pwd)
        if [ "$project_pwd" ]
            if [ -w "$real_pwd" ]
                __bobthefish_start_segment $color_path
            else
                __bobthefish_start_segment $color_path_nowrite
            end

            echo -ns $project_pwd ' '
        end
        return
    end

    set -l project_pwd (command git rev-parse --show-prefix 2>/dev/null | string trim --right --chars=/)
    set -l work_dir (command git rev-parse --show-toplevel 2>/dev/null)

    # only show work dir if it's a parent…
    if [ "$work_dir" ]
        switch $real_pwd/
            case $work_dir/\*
                string match "$git_root_dir*" $work_dir >/dev/null
                and set work_dir (string sub -s (math 1 + (string length $git_root_dir)) $work_dir)
            case \*
                set -e work_dir
        end
    end

    if [ "$project_pwd" -o "$work_dir" ]
        set -l colors $color_path
        if not [ -w "$real_pwd" ]
            set colors $color_path_nowrite
        end

        __bobthefish_start_segment $colors

        # handle work_dir != project dir
        if [ "$work_dir" ]
            set -l work_parent (__bobthefish_dirname $work_dir)
            if [ "$work_parent" ]
                echo -n "$work_parent/"
            end

            set_color normal
            set_color -b $color_repo_work_tree
            echo -n (__bobthefish_basename $work_dir)

            set_color normal
            set_color -b $colors
            [ "$project_pwd" ]
            and echo -n /
        end

        echo -ns $project_pwd ' '
    else
        set project_pwd $real_pwd

        string match "$git_root_dir*" $project_pwd >/dev/null
        and set project_pwd (string sub -s (math 1 + (string length $git_root_dir)) $project_pwd)

        set project_pwd (string trim --left --chars=/ -- $project_pwd)

        if [ "$project_pwd" ]
            set -l colors $color_path
            if not [ -w "$real_pwd" ]
                set colors $color_path_nowrite
            end

            __bobthefish_start_segment $colors

            echo -ns $project_pwd ' '
        end
    end
end
