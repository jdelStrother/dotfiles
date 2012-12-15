let s:rxBlank = '\v^\s*$'
" let s:rxMethodStart = '\v^\s*def'
let s:rxMethodStart = '\v^\s*(def|it|before\(:each\)|task) '
let s:rxContainerStart = '\v^\s*(class|module|describe|namespace) '
let s:rxDeclarations = '\v^\s*((before|after|around)_filter|(validates\w*)|rescue_from|helper_method) '

fun! RubyFold(lineNum)
  let lineContent = getline(a:lineNum)
  if lineContent =~ s:rxMethodStart
    return '>1'

  elseif lineContent =~ s:rxDeclarations
    " when in a class or module, the first line of a declaration (eg
    " before_filter :foo) is the start of a fold.  We don't try to fold single
    " lines of declarations.  Declarations preceeded & followed by other
    " declarations can just use '=' folding
    let prevLine = getline(PreviousNonBlankLine(a:lineNum))
    let nextLine = getline(NextNonBlankLine(a:lineNum))
    let isSingleLine = (prevLine =~ s:rxDeclarations && nextLine =~ s:rxDeclarations)
    let isInnerLine = (prevLine !~ s:rxDeclarations && nextLine !~ s:rxDeclarations)
    if isSingleLine || isInnerLine
      return '='
    elseif getline(PreviousScopeLine(a:lineNum)) =~ s:rxContainerStart
      if prevLine !~ s:rxDeclarations
        return '>1'
      else
        return '<1'
      endif
    endif

  elseif lineContent =~ '\v^\s*end\s*$'
    " try to find the corresponding start of this 'end' line
    let scopeLineNum = PreviousScopeLine(a:lineNum-1)
    if getline(scopeLineNum) =~ s:rxMethodStart && IndentLevel(scopeLineNum)==IndentLevel(a:lineNum)
      return 's1'
    endif
  end

  return '='
endfunction

function! RubyFoldText(foldstart, foldend, dashes)
  let lineContent = getline(a:foldstart)
  if lineContent =~ s:rxMethodStart
    let text = lineContent
  else
    let text = substitute(join(getline(a:foldstart, a:foldend), ','), '\v\s+', ' ', 'g')
    " reapply the indent from the first line:
    let text = substitute(text, '\v^\s+', matchstr(lineContent, '\v^\s+'), '')
  endif

  return text
  "substitute(text, '\v^.', , '')
endfunction

function! IndentLevel(lnum)
  return indent(a:lnum) / &shiftwidth
endfunction
function! PreviousNonBlankLine(lnum)
  let current=a:lnum-1
  while current>=0
    if getline(current) =~ '\v\S'
      return current
    endif
    let current -= 1
  endwhile
  return -2
endfunction
function! NextNonBlankLine(lnum)
  let current=a:lnum+1
  let numlines = line('$')
  while current<=numlines
    if getline(current) =~ '\v\S'
      return current
    endif
    let current += 1
  endwhile
  return -2
endfunction

function! PreviousScopeLine(lnum)
  let lineIndent=IndentLevel(a:lnum)
  let current = a:lnum-1
  let depth=1
  while current>=0
    if IndentLevel(current)<lineIndent
      let lineContent=getline(current)
      if lineContent =~ '\v^\s*(class|module|def|describe|it) '
        return current
      endif
    endif
    let current -= 1
  endwhile
  return -2
endfunction

" function! NextScopeLine(lnum)
"   let numlines = line('$')
"   let current = a:lnum+1
"   while current<=numlines
"     if getline(current) =~? '\v^\s*(end)'
"       return current
"     endif
"     let current += 1
"   endwhile
"   return -2
" endfunction

setlocal foldmethod=expr
setlocal foldexpr=RubyFold(v:lnum)
" setlocal foldcolumn=4
setlocal foldtext=RubyFoldText(v:foldstart,v:foldend,v:folddashes)

" vim: ft=vim
