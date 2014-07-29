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
Bundle 'vim-ruby/vim-ruby'
Bundle 'tpope/vim-dispatch'
Bundle 'Shougo/unite.vim'
Bundle 'Shougo/vimproc.vim'
Bundle 'Shougo/vimfiler.vim'
Bundle 'Shougo/unite-outline'
Bundle 'Shougo/unite-help'
Bundle 'Shougo/unite-session'
Bundle 'tsukkee/unite-tag'
Bundle 'Shougo/neomru.vim'

" repos on Vim-Scripts
Bundle 'EasyMotion'
Bundle 'taglist.vim'
Bundle 'vim-coffee-script'
Bundle 'Mark--Karkat'
Bundle 'nginx.vim'
Bundle 'LargeFile'
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


" Unite.vim ----------------------------------------------------------------------------------------{{{1
let g:unite_source_tag_max_fname_length = 45
" Use the fuzzy matcher for everything
call unite#filters#matcher_default#use(['matcher_fuzzy'])
" Use the rank sorter for everything
" call unite#filters#sorter_default#use(['sorter_rank'])

" Set up some custom ignores
call unite#custom_source('file_rec,file_rec/async,file_mru,file,buffer,grep',
      \ 'ignore_pattern', join([
      \ '\.git/',
      \ 'app/assets/images',
      \ 'tmp/',
      \ '.sass-cache',
      \ ], '\|'))
let g:unite_source_history_yank_enable = 1

if executable('ag')
  let g:unite_source_rec_async_command = 'ag --follow --nocolor --nogroup -g ""'
  let g:unite_source_grep_command='ag'
  let g:unite_source_grep_default_opts='--nocolor --line-numbers --nogroup -S -C0'
  let g:unite_source_grep_recursive_opt=''
elseif executable('ack')
  let g:unite_source_grep_command='ack'
  let g:unite_source_grep_default_opts='--no-heading --no-color'
  let g:unite_source_grep_recursive_opt=''
endif

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

nnoremap <Leader>ff :<C-u>Unite -no-split -buffer-name=files -auto-resize -start-insert buffer file_mru file_rec/async<cr>
nnoremap <Leader>ft :<C-u>Unite -no-split -buffer-name=tags -auto-resize -start-insert tag<cr>
nnoremap <Leader>fb :<C-u>Unite -no-split -buffer-name=buffers -quick-match buffer<cr>
nnoremap <Leader>fo :<C-u>Unite -no-split -buffer-name=outline -start-insert outline<cr>
nnoremap <Leader>fy :<C-u>Unite history/yank<cr>
nnoremap <Leader>fg :<C-u>Unite grep:.<cr>
nnoremap <Leader>fd :<C-u>VimFilerBufferDir<cr>

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
  autocmd FileType unite call s:configure_unite()
	function! s:configure_unite()
	  " Overwrite settings.
	  imap <silent><buffer><expr> <C-s> unite#do_action('split')
	endfunction

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
