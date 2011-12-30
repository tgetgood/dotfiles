set mouse=a hlsearch bs=2 sw=2 ts=2 expandtab autoindent smartindent
syntax on 
set bg=dark

" Switch windows more fluidly in normal or insert mode. 
" I never used the defaults for these keys anyway (if there are any).
nmap <c-h> <c-w>h
nmap <c-j> <c-w>j
nmap <c-k> <c-w>k
nmap <c-l> <c-w>l

imap <c-h> <c-o><c-w>h
imap <c-j> <c-o><c-w>j
imap <c-k> <c-o><c-w>k
imap <c-l> <c-o><c-w>l

"Better runtime paths
call pathogen#infect()

filetype plugin on
set ofu=syntaxcomplete#Complete

set nu wildmenu

runtime! macros/matchit.vim

"Drupal Specific settings
if has("autocmd")
  " Drupal *.module files.
  augroup module
    autocmd BufRead *.module set filetype=php
    autocmd BufRead *.inc set filetype=php
    autocmd BufRead *.install set filetype=php
    autocmd BufRead *.engine set filetype=php
  augroup END
endif

"some rails stuff
filetype plugin indent on
augroup myfiletypes
  autocmd!
  autocmd FileType ruby,eruby,yaml set ai sw=2 sts=2 et
augroup END

command SW execute 'w !sudo tee % >/dev/null' | e! %
command DCC execute '! drush cache clear'


"make gd work for drupal
map gd :exe '/\s*function\s*'.expand("<cword>")<CR>
map <F11> :set hlsearch!<CR>
set grepprg=ack\ -a
map gr :grep <cword> site/sites/all

"-s0 allows nc to respond to ^D (EOF); see `man nc`
map <F5> :silent !echo "BrowserReload(); repl.quit();^D" \| nc -s0 192.168.2.122 4242;<CR><C-C><C-L>
set tags=./tags;/


" Rebuild the cScope database and reconnect to the server.
function ReScope()
  cs kill -1

  " Assume the top level of the project contains the .git folder
  let dir = finddir('.git', '.;/var/shared/sites;/var/www;/home')
  " if (dir == '.' || dir == '/home' || dir == '/var/www' || dir = '/var/shared/sites')
  "   return
  " endif
  execute "cd " . dir 
  cd ..

  "TODO: extend this to find source files in other languages 
  execute "!cscope -b -q -u -i<(find " . getcwd() . " -path '.git/*' -prune , -path '.svn/*' -prune ,  \\( -name *.module -o -name *.inc -o -name *.php -o -name *.install -o -name *.engine -o -name *.test -o -name *.theme -o -name *.js -o -name *.rb -o -name *.rhtml -o -name *.py -o -name *.yml -o -name Rakefile \\))"
  
  execute "cs add  " . getcwd() . "/cscope.out"
endfunction

" if has("cscope")
"   call ReScope()
" endif

comm! -nargs=0 RS call ReScope()

"inspired by http://cscope.sourceforge.net/cscope_maps.vim
map <C-\>p :tab split<CR>:exec("tag ".expand("<cword>"))<CR>

"Taken from http://www.thingy-ma-jig.co.uk/comment/7067#comment-7067
function! DrupalImplementsComment(fname)
  let hook = substitute(a:fname,"^[0-9a-zA-Z]\\+_","","")
  set paste
 
  exe "normal! O/**\<CR>"
  \          . " * Implements hook_" . hook . "().\<CR>"
  \          . " */\<Esc>"
 
  set nopaste
endfunction
nmap <C-\>o :cs find c <C-R>=expand("<cword>")<CR><CR>	

" Rope setup
let ropevim_vim_completion=1
let ropevim_extend_complete=1

