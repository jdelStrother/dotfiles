" vim: set foldmethod=marker

set nocompatible
" Load Vundle & Plugins ------------------------------------------------------------------------{{{1
filetype off " force reloading of filetype
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()
runtime macros/matchit.vim

" let Vundle manage Vundle
Bundle 'gmarik/vundle'

" repos on github
Bundle 'AndrewRadev/linediff.vim'
Bundle 'airblade/vim-rooter'
Bundle 'kana/vim-textobj-user'
Bundle 'msanders/cocoa.vim'
Bundle 'nelstrom/vim-textobj-rubyblock'
Bundle 'sjl/gundo.vim'
Bundle 'sjl/threesome.vim'
Bundle 'sjl/badwolf'
Bundle 'tpope/vim-commentary'
Bundle 'tpope/vim-cucumber'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-haml'
Bundle 'tpope/vim-rails'
Bundle 'tpope/vim-repeat'
Bundle 'tpope/vim-surround'
Bundle 'altercation/vim-colors-solarized'
Bundle 'Lokaltog/vim-powerline'
Bundle 'ciaranm/securemodelines'
Bundle 'tpope/vim-endwise'
Bundle 'maxbrunsfeld/vim-yankstack'
Bundle 'kien/ctrlp.vim'
Bundle 'vim-ruby/vim-ruby'
" repos on Vim-Scripts
Bundle 'bufexplorer.zip'
Bundle 'EasyMotion'
Bundle 'taglist.vim'
Bundle 'vim-coffee-script'
Bundle 'Mark--Karkat'
" }}}
" Backups --------------------------------------------------------------------------------------{{{1
" purposefully not using .vim/tmp because our .vim dir is in Dropbox
set backupdir=~/.tmp/vim/backup,~/tmp,/var/tmp,/tmp
set directory=~/.tmp/vim/swap,~/tmp,/var/tmp,/tmp

set backup
set swapfile

if exists('+undofile')
  set undofile
  set undodir=~/.tmp/vim/undo
end


" Basic Options --------------------------------------------------------------------------------{{{1
set showcmd		" display incomplete commands
set modelines=5
set scrolloff=3 " show 3 lines of context around scrolling cursor
set ttyfast
set showcmd		" display incomplete commands
" allow buffers to go to the background without forcing you to save them first
" set hidden
" auto-write buffers before switching away
set autowriteall


" Show trailing spaces
set listchars=tab:▸\ ,eol:¬,trail:·
" don't show unnecessary 'press enter to continue' prompts'
" set shortmess=atI
" autoread changed files on switching back to vim
set autoread

" set foldmethod=syntax
" set foldlevelstart=1 " don't auto-fold on opening files
" set foldminlines=0 " When folding, hide 1-line methods just like everything else

" allow backspacing over everything in insert mode
set backspace=indent,eol,start
set history=1000	" keep 1000 lines of command line history
set undolevels=1000
set ruler		" show the cursor position all the time

" show completions on tab
set wildmenu
set wildignore+=files/**,public/files/**,*.log

" Don't try to highlight lines longer than 800 characters.
set synmaxcol=800

" we autogenerate ctags into the .git dir
" http://tbaggery.com/2011/08/08/effortless-ctags-with-git.html
set tags+=.git/tags

" Appearance -----------------------------------------------------------------------------------{{{1
set background=dark
try
  colorscheme badwolf
catch
  echo "Couldn't load badwolf"
endtry
"
" Switch syntax highlighting on, when the terminal has colors
" Also switch on highlighting the last used search pattern.
if &t_Co > 2 || has("gui_running")
  syntax on
  set hlsearch
endif

" Solarized's default is to add an underline to folded lines, which is excessive
hi Folded term=bold cterm=bold


" Tabs & Spaces --------------------------------------------------------------------------------{{{1
set shiftwidth=2
set shiftround
set softtabstop=2
set tabstop=2
set expandtab

" Searching & Movement -------------------------------------------------------------------------{{{1
" ignore case in searching, unless there's a capital letter in the search phrase
set ignorecase
set smartcase
" assume the /g flag on :s substitutions to replace all matches in a line
set gdefault
" do incremental searching
set incsearch

set virtualedit+=block

" Plugin Settings ------------------------------------------------------------------------------{{{1

let g:EasyMotion_leader_key = '\'
let g:fuzzy_ignore = "*.log,tmp/*,files/*,public/files/*"
let g:fuzzy_matching_limit = 70
let g:Powerline_symbols = 'fancy'

let g:yankstack_map_keys = 0
nmap <C-p> <Plug>yankstack_substitute_older_paste
xmap <C-p> <Plug>yankstack_substitute_older_paste
imap <C-p> <Plug>yankstack_substitute_older_paste
nmap <C-P> <Plug>yankstack_substitute_newer_paste
xmap <C-P> <Plug>yankstack_substitute_newer_paste
imap <C-P> <Plug>yankstack_substitute_newer_paste

" Don't let the Mark plugin map <leader>r
map <leader>M <Plug>MarkClear
map <Plug>IgnoreMarkRegex <Plug>MarkRegex
map <Plug>IgnoreMarkSearchAnyNext <Plug>MarkSearchAnyNext

let Tlist_Ctags_Cmd="/usr/local/bin/ctags"

if exists("*fugitive#statusline")
  set statusline=%<%f\ %h%m%r%{fugitive#statusline()}%=%-14.(%l,%c%V%)\ %P
endif
set laststatus=2 "always show status line

" Lose the context-sensitive coloring, it's really slow
" let ruby_no_expensive = 1

" hit p to open vertical preview windows in netrw
let g:netrw_preview   = 1
let g:netrw_winsize   = 30


" CtrlP ----------------------------------------------------------------------------------------{{{1
let g:ctrlp_map = '<c-t>'
let g:ctrlp_max_height = 20
" the multiple uses of find in the fallback are to ensure that everything in the current
" directory get listed before descending into deeper directories
" That way, even if we run this in our home directory and get truncated
" output, at least the files at the top level will show up.
let g:ctrlp_user_command = {
  \ 'types': {
    \ 1: ['.git', 'cd %s && git ls-files . -co --exclude-standard | grep -v -E "\.(gif|png|jpe?g|svg|psd|ai|gem)\$" '],
    \ },
  \ 'fallback': 'cd %s; (find -L . -type f -maxdepth 1 && find -E -L . -type f -mindepth 1 ! -regex "(^|.*/)(\.bundler|\.rvm|\.Trash)(/.*|$)") | head -1000'
  \ }
  "\ 'fallback': 'cd %s; (find . -type f -follow -maxdepth 1 && find . -type f -follow -mindepth 1) | grep -v -E "(^|/)(\.bundler|\.rvm|\.Trash)" | head -5000'
let g:ctrlp_match_func = { 'match': 'MatchFunc3' }
function! MatchFunc(items, str, limit, mmode, ispath, crfile, regex)
  let script = "
        \  # we'll get invoked with the arguments '<LIMIT> <MANIFEST_PATH> <SEARCH TERMS>'                                       \n
        \  search_limit = ARGV[0].to_i                                                                                           \n
        \  cache_file = ARGV[1]                                                                                                  \n
        \  search_term = ARGV[2..-1].join(\" \")                                                                                 \n
        \                                                                                                                        \n
        \  if search_term.length==0                                                                                              \n
        \    puts File.read(cache_file)                                                                                          \n
        \    exit(0)                                                                                                             \n
        \  end                                                                                                                   \n
        \                                                                                                                        \n
        \                                                                                                                        \n
        \  # take each individual character and regex-escape it.  Spaces will match both literal space and a directory slash.    \n
        \  search_letters = search_term.split('').map{|l| Regexp.escape(l.gsub(/ /,'[ /]'))}                                     \n
        \  greedy_matcher = Regexp.new(search_letters.join('.*'))                                                                \n
        \  matcher =        Regexp.new(search_letters.join('.*?'))                                                               \n
        \  lookahead_matcher = Regexp.new(\"#{search_letters[0]}.*(?=#{search_letters[1..-1].join('.*')})\")                     \n
        \                                                                                                                        \n
        \  # Greedy matching ('.*') is much faster than non-greedy ('.*?')                                                       \n
        \  # Pre-filter the paths to find ones that match the overall regex using the greedy match:                              \n
        \  matching_paths = File.read(cache_file).split(\"\\n\").select{|p,m| p=~greedy_matcher}                               \n
        \  # matching_paths = File.read(cache_file).scan(Regexp.new(\"^.*#{search_letters.join('.*')}.*$\"))                       \n
        \                                                                                                                        \n
        \  # Determine a 'score' for each path that shows how suitable each is, then print the best scoring paths.               \n
        \  # A lower score is better.                                                                                            \n
        \  puts matching_paths.map{|path|                                                                                    \n
        \    # using the non-greedy match, sort the matching paths by their minimum match range.                                 \n
        \    # eg, if we search for 'ac' in abcd and abdc, the former should score higher because the                            \n
        \    # match only spans 3 characters rather than 4.                                                                      \n
        \    length = path.enum_for(:scan, lookahead_matcher).map{ ($&+$')[matcher].length}.min \n
        \    start_match,end_match = *matcher.match(path).offset(0)                                                              \n
        \    score = length # end_match - start_match                                                                                     \n
        \    # Paths where the match intersects the filename are better (eg when searching for 'foo',                            \n
        \    # 'dir/foo.c' should score higher than 'dir/foo/bar.c')                                                             \n
        \    end_of_match_is_in_file_component = File.dirname(path).length < end_match                                           \n
        \    score -= 5 if end_of_match_is_in_file_component                                                                     \n
        \                                                                                                                        \n
        \    [\"#{path} (#{length})\", score]                                                                                                        \n
        \  }.sort_by{|p,s| s}.map{|p,score| \"#{p}\"}[0..search_limit]                                               \n
        \"
  try
    " Create a cache file
    let cachefile = ctrlp#utils#cachedir().'/custom.cache'
    call writefile(a:items, cachefile)
    let cmd = '/Users/jon/.rvm/rubies/ruby-1.9.3-p194/bin/ruby -- - '.a:limit.' '.cachefile.' '.a:str
    let result = split(system(cmd, script), "\n")
    return result
  catch
    return v:exception
  endtry
endfunction


function! MatchFunc2(items, str, limit, mmode, ispath, crfile, regex)

  keepa edit foobar
  set nobuflisted 
  set buftype=nofile 
  set bufhidden=hide 
  call append(0,items)

"ruby << RUBY
"
"  # we'll get invoked with the arguments '<LIMIT> <MANIFEST_PATH> <SEARCH TERMS>'
"  search_term = "mod/user"
"
"  #   buffer = VIM::Buffer.current
"  #   file_list = 1.upto(buffer.length-1).map{|l| buffer[l]}
"  #   while buffer.length>1
"  #     buffer.delete(1)
"  #   end
"  # 
"  #   puts __LINE__
"  #   # if search_term.length==0
"  #   #   puts File.read(cache_file)
"  #   #   exit(0)
"  #   # end
"  # 
"  # 
"  #   # take each individual character and regex-escape it.  spaces will match both literal space and a directory slash.
"  #   search_letters = search_term.split('').map{|l| regexp.escape(l.gsub(/ /,'[ /]'))}
"  #   greedy_matcher = Regexp.new(search_letters.join('.*'))
"  #   matcher =        Regexp.new(search_letters.join('.*?'))
"  # 
"  #   # greedy matching ('.*') is much faster than non-greedy ('.*?')
"  #   # pre-filter the paths to find ones that match the overall regex using the greedy match:
"  #   # matching_paths = File.read(cache_file).split("\n").select{|p,m| p=~greedy_matcher}
"  #   matching_paths = file_list.scan(Regexp.new("^.*#{search_letters.join('.*')}.*$"))
"  # 
"  #   puts __LINE__
"  #   # determine a 'score' for each path that shows how suitable each is, then print the best scoring paths.
"  #   # a lower score is better.
"  #   results = matching_paths.sort_by{|path|
"  #     # using the non-greedy match, sort the matching paths by their minimum match range.
"  #     # eg, if we search for 'ac' in abcd and abdc, the former should score higher because the
"  #     # match only spans 3 characters rather than 4.
"  #     start_match,end_match = *matcher.match(path).offset(0)
"  #     score = end_match - start_match
"  #     # paths where the match intersects the filename are better (eg when searching for 'foo',
"  #     # 'dir/foo.c' should score higher than 'dir/foo/bar.c')
"  #     end_of_match_is_in_file_component = File.dirname(path).length < end_match
"  #     score -= 5 if end_of_match_is_in_file_component
"  # 
"  #     score
"  #   }.map{|p,m| p}[0..search_limit]
"  # 
"  #   puts __LINE__
"  #   results.each do |result|
"  #     buffer.append(buffer.length-1, result)
"  #   end
"RUBY
"  
 let l:result = getline(1, line('$'))
 echo l:result
 " bd!
 silent execute "normal \<C-^>"
 return l:result
endfunction

function! MatchFunc3(items, str, limit, mmode, ispath, crfile, regex)
  let cachefile = ctrlp#utils#cachedir().'/custom.cache'
  if !exists("g:itemCache") || g:itemCache!=a:items
    let g:itemCache = a:items
    call writefile(a:items, cachefile)
  endif
  let ruby = '/Users/jon/.rvm/rubies/ruby-1.9.3-p194/bin/ruby'
  let script = '/Users/jon/.bin/ctrlp_matcher.rb'
  let cmd = ruby.' '.script.' '.a:limit.' '.cachefile.' '.a:str
  let result = split(system(cmd), "\n")
  return result
endfunction

" Mappings & Commands --------------------------------------------------------------------------{{{1
let mapleader=" "
let maplocalleader="\\"

vmap <Leader>b :<C-U>!git blame <C-R>=expand("%:p") <CR> \| sed -n <C-R>=line("'<") <CR>,<C-R>=line("'>") <CR>p <CR>
" edit a file in the same directory as the current file
map <Leader>e :e <C-R>=expand("%:p:h") . '/'<CR>
map <leader>d :execute 'Explore ' . getcwd()<CR>
map <leader>D :Explore<CR>
map <leader>u :GundoToggle<CR>
" fold everything except the current block
nnoremap <leader>z zMzv

map <C-s> :write<CR>
imap <C-s> <Esc><C-s>
" map <C-t> Xp
imap <C-t> <C-o>X<C-o>p

imap jj <Esc>

" Open a Quickfix window for the last search
nnoremap <silent> <leader>/ :execute 'vimgrep /'.@/.'/g %'<CR>:copen<CR>

" :w!! for sudo-save
cmap w!! w !sudo tee % >/dev/null

command! Q q " Bind :Q to :q

" @h to convert {:x=>1} to {x:1}
let @h='/:xepldf>'


" I keep accidentally middle clicking on my trackpad, which pastes shit
nnoremap <MiddleMouse> <Nop>
nnoremap <2-MiddleMouse> <Nop>
nnoremap <3-MiddleMouse> <Nop>
nnoremap <4-MiddleMouse> <Nop>

inoremap <MiddleMouse> <Nop>
inoremap <2-MiddleMouse> <Nop>
inoremap <3-MiddleMouse> <Nop>
inoremap <4-MiddleMouse> <Nop>

nnoremap <leader><space> za

" Convenient command to see the difference between the current buffer and the
" file it was loaded from, thus the changes you made.
command! DiffOrig vert new | set bt=nofile | r # | 0d_ | diffthis | wincmd p | diffthis

" Turn off search highlight temporarily
nmap <silent> <leader>n :silent :nohlsearch<CR>

" Toggle display of invisibles
nmap <silent> <leader>s :set nolist!<CR>

" Environments ---------------------------------------------------------------------------------{{{1
if has("gui_running")
  " https://gist.github.com/1627888
  set guifont=Menlo\ Regular\ for\ Powerline:h11

  " I can't get 'hi link EasyMotionShade  Comment' to work :(
  hi EasyMotionShade term=bold ctermfg=11 guifg=#5c7176

  " No toolbar
  set guioptions-=T
  " get rid of left/right scrollbars
  set guioptions-=L
  set guioptions-=r

  " Use a line-drawing char for pretty verticl splits
  set fillchars+=vert:│

  " Different cursors for different modes.
  set guicursor=n-c:block-Cursor-blinkon0
  set guicursor+=v:block-vCursor-blinkon0
  set guicursor+=i-ci:ver20-Cursor

  " Make macvim take up the entire screen in fullscreen mode
  if exists('+fuoptions')
    set fuoptions=maxvert,maxhorz
  endif
else
  " terminal-only stuff
  if exists('$TMUX')
    " this causes the corrupt-screen problems if you hit 'dd' before entering
    " insert mode
    let &t_SI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=1\x7\<Esc>\\"
    let &t_EI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=0\x7\<Esc>\\"
  else
    let &t_SI = "\<Esc>]50;CursorShape=1\x7"
    let &t_EI = "\<Esc>]50;CursorShape=0\x7"
  endif
end

" In many terminal emulators the mouse works just fine, thus enable it.
if has('mouse')
  set mouse=a
endif


" AutoCmd --------------------------------------------------------------------------------------{{{1
if has("autocmd")

  " Enable file type detection.
  " Use the default filetype settings, so that mail gets 'tw' set to 72,
  " 'cindent' is on in C files, etc.
  " Also load indent files, to automatically do language-dependent indenting.
  filetype plugin indent on

  " Put these in an autocmd group, so that we can delete them easily.
  augroup vimrcEx
  au!

  " For all text files set 'textwidth' to 78 characters.
  autocmd FileType text setlocal textwidth=78

  " Consider '-' as part of a word in css
  autocmd FileType sass,css,scss setlocal iskeyword+=-

  " When editing a file, always jump to the last known cursor position.
  " Don't do it when the position is invalid or when inside an event handler
  " (happens when dropping a file on gvim).
  " Also don't do it when the mark is in the first line, that is the default
  " position when opening a file.
  autocmd BufReadPost *
    \ if expand('%.')!='.git/index' && line("'\"") > 1 && line("'\"") <= line("$") |
    \   exe "normal! g`\"" |
    \ endif

  autocmd BufNewFile,BufRead *.rbapi set filetype=ruby
  autocmd BufNewFile,BufRead Gemfile set filetype=ruby

  " Open :help documents in a nice, big vertical split instead of a horizontal one:
  au FileType help wincmd L

  " highlight the cursor's line in the current window:
  autocmd WinEnter * setlocal cursorline
  autocmd WinLeave * setlocal nocursorline

  autocmd FileType ruby iab rw attr_accessor

  augroup END

else

  set autoindent  " always set autoindenting on

endif " has("autocmd")


" SpinTest -------------------------------------------------------------------------------------{{{1
function! s:first_readable_file(files) abort
  let files = type(a:files) == type([]) ? copy(a:files) : split(a:files,"\n")
  for file in files
    if filereadable(rails#app().path(file))
      return file
    endif
  endfor
  return ''
endfunction
function! SpinTest(file, linenumber)
  let linenumber = a:linenumber
  if a:file =~# '\(\<test_.*\|\(_test\|_spec\)\)\.rb$'
    let test_file = a:file
  else
    let test_file = s:first_readable_file(rails#buffer(a:file).related())
    let linenumber=""
  endif
  if test_file != ""
    let g:SpinLastFile = test_file
  else
    " fall back to the previously used file
    let test_file = g:SpinLastFile
  endif
  let command = "!spin push " . shellescape(test_file)
  if linenumber!=""
    let command = command . ":" . a:linenumber
  endif
  silent exe command
endfunction
nnoremap <silent> <leader>r :call SpinTest(expand('%'), "")<CR>
nnoremap <silent> <leader>R :call SpinTest(expand('%'), line('.'))<CR>


" hacking --------------------------------------------------------------------------------------{{{1
function! RubyTest()
  let l:list = ['x', 'z', 'a','b','c']
  silent hide enew
  set nobuflisted 
  set buftype=nofile 
  set bufhidden=hide 
  call append(0,l:list)
ruby << RUBY
  buffer = VIM::Buffer.current
  lines = 1.upto(buffer.length-1).map{|l| buffer[l]}
  while buffer.length>1
    buffer.delete(1)
  end
  lines.sort.each do |line|
    buffer.append(buffer.length-1, line)
  end
RUBY

  let l:result = getline(1, line('$'))
  " bd!
  silent execute "normal \<C-^>"
  echo l:result
endfunction
