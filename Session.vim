let SessionLoad = 1
let s:so_save = &g:so | let s:siso_save = &g:siso | setg so=0 siso=0 | setl so=-1 siso=-1
let v:this_session=expand("<sfile>:p")
silent only
silent tabonly
cd ~/stuff/development/recreation-alert
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
let s:shortmess_save = &shortmess
if &shortmess =~ 'A'
  set shortmess=aoOA
else
  set shortmess=aoO
endif
badd +31 test/campsite1.json
badd +2 test/Recreation/ClientSpec.hs
badd +6733 term://~/stuff/development/recreation-alert//80013:/Users/home/.nix-profile/bin/zsh
badd +10 test/campsite_invalid_1.json
badd +1 src/Recreation/Client.hs
badd +22 src/Recreation/Types.hs
badd +3 src/MyLib.hs
badd +120 recreation-alert.cabal
badd +32 app/Env.hs
badd +14 src/Recreation/Class.hs
badd +17 app/Main.hs
badd +33 app/CLI.hs
badd +1 src/Recreation/Predicate.hs
badd +90 term://~/stuff/development/recreation-alert//79650:/Users/home/.nix-profile/bin/zsh
badd +70 src/Pushbullet/Notifier.hs
badd +3 ~/.config/recreation-alert.json
badd +26 .gitignore
argglobal
%argdel
edit app/Env.hs
let s:save_splitbelow = &splitbelow
let s:save_splitright = &splitright
set splitbelow splitright
wincmd _ | wincmd |
vsplit
1wincmd h
wincmd w
let &splitbelow = s:save_splitbelow
let &splitright = s:save_splitright
wincmd t
let s:save_winminheight = &winminheight
let s:save_winminwidth = &winminwidth
set winminheight=0
set winheight=1
set winminwidth=0
set winwidth=1
exe 'vert 1resize ' . ((&columns * 79 + 79) / 158)
exe 'vert 2resize ' . ((&columns * 78 + 79) / 158)
argglobal
balt ~/.config/recreation-alert.json
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal nofen
silent! normal! zE
let &fdl = &fdl
let s:l = 30 - ((15 * winheight(0) + 16) / 33)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 30
normal! 0
wincmd w
argglobal
if bufexists(fnamemodify("term://~/stuff/development/recreation-alert//80013:/Users/home/.nix-profile/bin/zsh", ":p")) | buffer term://~/stuff/development/recreation-alert//80013:/Users/home/.nix-profile/bin/zsh | else | edit term://~/stuff/development/recreation-alert//80013:/Users/home/.nix-profile/bin/zsh | endif
if &buftype ==# 'terminal'
  silent file term://~/stuff/development/recreation-alert//80013:/Users/home/.nix-profile/bin/zsh
endif
balt recreation-alert.cabal
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal nofen
let s:l = 8791 - ((32 * winheight(0) + 16) / 33)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 8791
normal! 02|
wincmd w
2wincmd w
exe 'vert 1resize ' . ((&columns * 79 + 79) / 158)
exe 'vert 2resize ' . ((&columns * 78 + 79) / 158)
tabnext 1
if exists('s:wipebuf') && len(win_findbuf(s:wipebuf)) == 0 && getbufvar(s:wipebuf, '&buftype') isnot# 'terminal'
  silent exe 'bwipe ' . s:wipebuf
endif
unlet! s:wipebuf
set winheight=1 winwidth=20
let &shortmess = s:shortmess_save
let &winminheight = s:save_winminheight
let &winminwidth = s:save_winminwidth
let s:sx = expand("<sfile>:p:r")."x.vim"
if filereadable(s:sx)
  exe "source " . fnameescape(s:sx)
endif
let &g:so = s:so_save | let &g:siso = s:siso_save
set hlsearch
nohlsearch
let g:this_session = v:this_session
let g:this_obsession = v:this_session
doautoall SessionLoadPost
unlet SessionLoad
" vim: set ft=vim :
