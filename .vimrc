set nocompatible
colorscheme vividchalk
set background=dark

set backupdir=~/.vim/tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set directory=~/.vim/tmp,~/.tmp,~/tmp,/var/tmp,/tmp

set lines=50
set columns=130
" set number

" show 3 lines of context around scrolling cursor
set scrolloff=3

set softtabstop=2

let mapleader=" "

let g:fuzzy_ignore = "*.log,tmp/*"
let g:fuzzy_matching_limit = 70

map <leader>t :FuzzyFinderTextMate<CR>
map <leader>b :FuzzyFinderBuffer<CR>
map <leader>r :ruby finder.rescan!<CR>:FuzzyFinderRemoveCache<CR>:exe ":echo 'rescan complete'"<CR>

map <leader>d :execute 'NERDTreeToggle ' . getcwd()<CR>
map <leader>D :NERDTree %:h<CR>

map <C-s> :write<CR>
imap <C-s> <Esc><C-s>

" allow buffers to go to the background without forcing you to save them first
set hidden

" show completions on tab
set wildmenu

" Turn off search highlight temporarily
nmap <silent> <leader>n :silent :nohlsearch<CR>

" Show trailing spaces
set listchars=tab:>-,trail:·,eol:$
nmap <silent> <leader>s :set nolist!<CR>

" don't show unnecessary 'press enter to continue' prompts'
set shortmess=atI

" set sessionoptions=blank,buffers,curdir,folds,help,resize,tabpages,winsize
" map <c-q> :mksession! ~/.vim/.session <cr>
" map <c-s> :source ~/.vim/.session <cr>

" allow backspacing over everything in insert mode
set backspace=indent,eol,start

if has("vms")
  set nobackup		" do not keep a backup file, use versions instead
else
  set backup		" keep a backup file
endif
set history=1000	" keep 1000 lines of command line history
set ruler		" show the cursor position all the time
set showcmd		" display incomplete commands
set incsearch		" do incremental searching

" CTRL-U in insert mode deletes a lot.  Use CTRL-G u to first break undo,
" so that you can undo CTRL-U after inserting a line break.
" inoremap <C-U> <C-G>u<C-U>

" In many terminal emulators the mouse works just fine, thus enable it.
if has('mouse')
  set mouse=a
endif

" Switch syntax highlighting on, when the terminal has colors
" Also switch on highlighting the last used search pattern.
if &t_Co > 2 || has("gui_running")
  syntax on
  set hlsearch
endif

" Only do this part when compiled with support for autocommands.
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

  " When editing a file, always jump to the last known cursor position.
  " Don't do it when the position is invalid or when inside an event handler
  " (happens when dropping a file on gvim).
  " Also don't do it when the mark is in the first line, that is the default
  " position when opening a file.
  autocmd BufReadPost *
    \ if line("'\"") > 1 && line("'\"") <= line("$") |
    \   exe "normal! g`\"" |
    \ endif

  augroup END

else

  set autoindent		" always set autoindenting on

endif " has("autocmd")

" Convenient command to see the difference between the current buffer and the
" file it was loaded from, thus the changes you made.
" Only define it when not defined already.
if !exists(":DiffOrig")
  command DiffOrig vert new | set bt=nofile | r # | 0d_ | diffthis
		  \ | wincmd p | diffthis
endif
