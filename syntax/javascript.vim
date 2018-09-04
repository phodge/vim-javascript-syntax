syn case match
syn spell default

" orphans
" TODO: generator functions in any context
" TODO: switch()
" TODO: operators/comparisons
" TODO: builtins

" TODO: review all features and make sure stuff is wrapped in checks for the ES version
let s:bufname = bufname('')
let s:defaults = {
      \ 'javascript_es5':        1,
      \ 'javascript_es6':        1,
      \ 'javascript_es2017':     1,
      \ 'javascript_jsx':        (s:bufname =~ '\.[tj]sx\>'),
      \ 'javascript_typescript': (s:bufname =~ '\.tsx\?$'),
      \ }
for [s:name, s:default] in items(s:defaults)
  if !exists('b:'.s:name)
    let b:[s:name] = s:default
  endif
endfor


" {{{ clusters

  " jsClExpr - for any expression
  " jsClAfterValue
  " jsClImportExport - import / export statements - these aren't allowed at
  "                    the top level

" }}}

" {{{ highlight groups

  " syntax errors
  hi! link jsSyntaxError IncSearch

" }}}

" {{{ errors - lowest precedence

  " out-of-place semicolon
  syn match jsErrorSemicolon contained /;/
  hi! link jsErrorSemicolon jsSyntaxError

  " out-of-place comma
  syn match jsErrorComma contained /,/
  hi! link jsErrorComma jsSyntaxError

  " unexpected closing brace, close bracket, close parens
  syn match jsErrorCloseBrace  contained /}/
  syn match jsErrorCloseSquare /]/
  syn match jsErrorCloseParen  /)/
  syn cluster jsClTop add=jsErrorCloseSquare,jsErrorCloseParen
  hi! link jsErrorCloseBrace  jsSyntaxError
  hi! link jsErrorCloseSquare jsSyntaxError
  hi! link jsErrorCloseParen  jsSyntaxError

  " unexpected assignment
  syn match jsErrorAssign contained /==\@!/
  hi! link jsErrorAssign jsSyntaxError

  " keywords that are used out of place
  syn keyword jsKeywordError in instanceof
  syn match jsKeywordError /\<\%(import\|export\)\>/
  syn cluster jsClTop add=jsKeywordError
  hi! link jsKeywordError jsSyntaxError

" }}}

" {{{ collect import/export names from the file!

  hi! link jsUserIdentifier Identifier

  function! <SID>CollectGlobalNames()
    let l:wordlist = {}

    " bufnum, lnum, col, off
    let l:oldpos = getpos('.')
    try
      call cursor(1, 1)

      " look for top-level import statements
      let l:start = 0
      while l:start <= line('$')
        call cursor(l:start, 1)
        let [l:line, l:col] = searchpos('^\s*import ', 'cnW', 0, 10)
        if l:line == 0
          break
        endif

        " set cursor to next line so we don't search endlessly
        let l:start = l:line + 1

        " grab everything after the word 'import'
        let l:imports = matchstr(getline(l:line), '^\s*import\s\+\zs.\{-}\ze\s\+from\>')

        " RegisterFirstWord() will add the first identifier, or <name> as identifier, or
        " * as <identifier> from l:imports
        while strlen(l:imports)
          let l:imports = <SID>RegisterFirstWord(l:imports, l:wordlist)
        endwhile
      endwhile
    finally
      call setpos('.', l:oldpos)
    endtry

    " set up syntax keyword rules for the wordlist
    execute 'syn keyword jsUserIdentifier contained ' . join(keys(l:wordlist))
  endfunction

  function! <SID>RegisterFirstWord(input, wordlist)
    let l:input = a:input

    " strip a leading comma or opening parenthesis off the start
    let l:prefix = matchstr(l:input, '^\s*[,{]\s*')
    if strlen(l:prefix)
      let l:input = strpart(l:input, strlen(l:prefix))
    endif

    " strip '* as ' or '<identifier> as' off the start of the import string
    let l:prefix = matchstr(l:input, '^\s*\%(\%(\*\s*\|\w\+\s\+\)as\s\+\)\=')
    if strlen(l:prefix)
      let l:input = strpart(l:input, strlen(l:prefix))
    endif

    " match the word at the start of l:input
    let l:word = matchstr(l:input, '^\w\+')
    if strlen(l:word)
      let a:wordlist[l:word] = 1
      return strpart(l:input, strlen(l:word))
    endif
    return ''
  endfunction

  augroup JavascriptSyntax
  augroup end
  autocmd! JavascriptSyntax BufWritePost <buffer> call <SID>CollectGlobalNames()
  call <SID>CollectGlobalNames()

" }}}

" {{{ reserved words

  " TODO: finish implementing all of these
  syn keyword jsReservedWord 
      \ case
      \ catch
      \ with
      \ switch yield
      \ debugger
      \ default
  syn match jsReservedWord /\<\%(try\|finally\)\>/
  syn cluster jsClTop add=jsReservedWord
  hi! link jsReservedWord IncSearch

" }}}

" {{{ comments

  syn region jsComment start=!//! end=/$/ oneline keepend extend
  syn region jsComment start=!/\*! end=!\*/! keepend extend
  syn cluster jsClTop add=jsComment
  hi! link jsComment Comment

" }}}

" {{{ expressions - normal dictionaries (first so they have lowest priority)

  syn region jsDictRegion matchgroup=jsDict start=/{/ end=/}/
        \ keepend extend contains=jsDictKey,jsDictKeySpecial,jsDictInlineFunc,jsDictComma,jsComment,jsErrorSemicolon,jsComment,jsGetter
  syn cluster jsClExpr add=jsDictRegion
  syn cluster jsClTop add=jsDictRegion
  hi! link jsDict Number

  syn match jsDictKey /\<[$A-Za-z0-9_]\+\_s*:/ contained nextgroup=@jsClExpr skipwhite skipnl
  syn match jsDictKey /\(['"]\)\%(\\.\|[^\\]\)\{-}\1:/ contained nextgroup=@jsClExpr skipwhite skipnl
  hi! link jsDictKey jsDict

  syn match jsDictKeySpecial contained /\(['"]\=\)__proto__\1\_s*:/ nextgroup=@jsClExpr skipwhite skipnl
  hi! link jsDictKeySpecial SpecialChar


  if b:javascript_es6
    syn region jsDictKey contained matchgroup=jsDict start=/\[/ end=/\]:/ keepend extend
          \ contains=@jsClExpr,jsErrorCloseBrace,jsErrorCloseParen,jsErrorSemicolon,jsErrorComma
          \ nextgroup=@jsClExpr skipwhite skipnl
  endif
 
  syn match jsDictInlineFunc /\<\h\w*\ze\_s*(/ contained nextgroup=jsFullFuncArgs skipwhite skipnl
  hi! link jsDictInlineFunc jsDictKey

  syn match jsDictComma contained /,/
  hi! link jsDictComma jsDict

  " ES5 getters/setters inside objects
  syn cluster jsClGetterError add=jsErrorCloseBrace,jsErrorComma,jsErrorAssign,jsErrorCloseSquare,jsErrorCloseParen
  syn cluster jsClGetterError add=jsGetterArgsError
  syn match jsGetter /\<get\>\%(\_s*:\)\@!/ contained nextgroup=jsGetterName,@jsClGetterError skipwhite skipnl
  syn match jsGetter /\<set\>\%(\_s*:\)\@!/ contained nextgroup=jsSetterName,@jsClGetterError skipwhite skipnl
  hi! link jsGetter Keyword

  syn match jsGetterName contained /\<[$A-Za-z_][$A-Za-z0-9_]*\>/ nextgroup=jsGetterArgs,@jsClGetterError skipwhite skipnl
  syn match jsSetterName contained /\<[$A-Za-z_][$A-Za-z0-9_]*\>/ nextgroup=jsSetterArgs,@jsClGetterError skipwhite skipnl
  if b:javascript_es6
    syn region jsGetterName contained matchgroup=jsGetter start=/\[/ end=/\]/ keepend extend nextgroup=jsGetterArgs,@jsClGetterError skipwhite skipnl
          \ contains=@jsClExpr,jsErrorSemicolon,jsErrorComma,jsErrorCloseBrace,jsErrorCloseParen
    syn region jsSetterName contained matchgroup=jsGetter start=/\[/ end=/\]/ keepend extend nextgroup=jsSetterArgs,@jsClGetterError skipwhite skipnl
          \ contains=@jsClExpr,jsErrorSemicolon,jsErrorComma,jsErrorCloseBrace,jsErrorCloseParen
  endif

  syn match jsGetterArgsError contained /(.\{-})/
  hi! link jsGetterArgsError jsSyntaxError

  syn match jsGetterArgs contained /(\_s*)/ extend nextgroup=jsGetterBody,@jsClGetterError skipwhite skipnl
  hi! link jsGetterArgs jsGetter
  syn match jsSetterArgs contained /([$A-Za-z_][$A-Za-z0-9_]*)/
        \ nextgroup=jsGetterBody,@jsClGetterError skipwhite skipnl
        \ contains=jsSetterArgParens
  syn match jsSetterArgParens contained /[()]/
  hi! link jsSetterArgParens jsGetter

  syn region jsGetterBody contained matchgroup=jsGetter start=/{/ end=/}/ keepend extend
        \ contains=@jsClTop,@jsClImportExport


" }}}

" {{{ lists, list-assignment

  hi! link jsList Typedef

  syn region jsListRegion matchgroup=jsList start=/\[/ end=/\]/ keepend extend
        \ contains=@jsClExpr,jsListComma,jsComment,jsErrorSemicolon
        \ nextgroup=@jsClAfterValue skipwhite skipnl
  syn cluster jsClExpr add=jsListRegion
  syn cluster jsClTop add=jsClTop
  syn match jsListComma /,/ contained
  hi! link jsListComma jsList


" }}}

" {{{ expressions - null/true/false/numbers

  syn keyword jsLiteralValue undefined null true false
        \ nextgroup=@jsClAfterValue skipwhite skipnl
  syn cluster jsClExpr add=jsLiteralValue
  syn cluster jsClTop add=jsClTop
  hi! link jsLiteralValue Typedef

  " decimal
  syn match jsNumber /\<\%(0\|[1-9]\d*\)\%(\.\d\+\)\=\>/ nextgroup=@jsClAfterValue skipwhite skipnl
  " octal
  syn match jsNumberOctal /\<0[0-7]\+\>/ nextgroup=@jsClAfterValue skipwhite skipnl
  if b:javascript_es6
    " binary
    syn match jsNumberBinary /\<0b[01]\+\>/ nextgroup=@jsClAfterValue skipwhite skipnl
  endif
  syn cluster jsClExpr add=jsNumber,jsNumberOctal,jsNumberBinary
  syn cluster jsClTop add=jsNumber,jsNumberOctal,jsNumberBinary
  hi! link jsNumber Statement
  hi! link jsNumberOctal Typedef
  hi! link jsNumberBinary SpecialChar


" }}}

" {{{ expressions - unary operators

  syn keyword jsOperator new typeof delete void nextgroup=@jsClExpr skipwhite skipnl
  syn match jsOperator /[!~]/ nextgroup=@jsClExpr skipwhite skipnl
  syn match jsOperator /\%(++\=\|--\=\)/ nextgroup=@jsClExpr skipwhite skipnl
  syn cluster jsClExpr add=jsOperator
  syn cluster jsClTop add=jsOperator
  hi! link jsOperator SpecialChar

  syn match jsPostIncrement contained /\%(++\|--\)/ nextgroup=@jsClExpr skipwhite skipnl
  syn cluster jsClAfterValue add=jsPostIncrement
  hi! link jsPostIncrement jsOperator


" }}}

" {{{ ternary operator

  syn region jsTernaryOperator matchgroup=Operator contained start=/?/ end=/:/ keepend extend
        \ contains=@jsClExpr,jsErrorSemicolon,jsErrorCloseBrace
        \ nextgroup=@jsClExpr skipwhite skipnl
  syn cluster jsClAfterValue add=jsTernaryOperator

" }}}

" {{{ expressions - dict-expansion: {var1, var2}

  " NOTE: jsDictAssign has the contained flag because it's not allowed to sit
  " by itself - it has to come after a 'var' option
  syn match jsDictAssign contained /{\_s*\w\+\_s*\%(,\_s*\w\+\_s*\)*}/ extend
        \ nextgroup=jsAssign skipwhite skipnl contains=jsIdentifier
  hi! link jsDictAssign jsVar

  " NOTE: jsListAssign is a top-level thing
  syn region jsListAssignRegion matchgroup=jsVar start=/\[/ end=/\]/ keepend extend
        \ nextgroup=jsAssign skipwhite skipnl
        \ contains=jsVarComma,jsSplat,jsIdentifier
  syn match jsSplat contained /\.\.\./
  syn cluster jsClTop add=jsListAssignRegion
  hi! link jsSplat jsVar

" }}}

" {{{ expressions

  syn region jsString start=/"/ end=/"/ skip=/\\./ keepend extend contains=@jsClInsideString nextgroup=@jsClAfterValue skipwhite skipnl
  syn region jsString start=/'/ end=/'/ skip=/\\./ keepend extend contains=@jsClInsideString nextgroup=@jsClAfterValue skipwhite skipnl
  syn cluster jsClExpr add=jsString
  syn cluster jsClTop add=jsString
  hi! link jsString String

  " new `` interpolated strings
  syn region jsSuperString start=/`/ end=/`/ skip=/\\./ contains=jsSuperStringExpr keepend extend
  hi! link jsSuperString jsString
  syn cluster jsClExpr add=jsSuperString
  syn cluster jsClTop add=jsSuperString
  syn region jsSuperStringExpr matchgroup=jsSuperStringDelim start=/\${/ end=/}/ keepend extend
        \ contains=@jsClExpr,jsErrorCloseSquare,jsErrorCloseParen,jsErrorSemicolon
  hi! link jsSuperStringDelim Special

  syn match jsIdentifier /\%(\<[A-Za-z_]\|\$\)[$A-Za-z0-9_]*\%(\$\|\>\)/ nextgroup=@jsClAfterValue,jsAssign skipwhite skipnl contains=jsUserIdentifier
  syn cluster jsClExpr add=jsIdentifier
  syn cluster jsClTop add=jsIdentifier

  " special identifiers
  syn keyword jsSpecialIdentifier super this nextgroup=@jsClAfterValue,jsAssign skipwhite skipnl
  hi! link jsSpecialIdentifier SpecialChar
  syn cluster jsClExpr add=jsSpecialIdentifier
  syn cluster jsClTop add=jsSpecialIdentifier

  syn region jsCall contained matchgroup=jsParens start=/(/ end=/)/ keepend extend
        \ contains=@jsClExpr,jsErrorCloseBrace,jsErrorCloseSquare,jsErrorSemicolon
        \ nextgroup=@jsClAfterValue skipwhite skipnl
  syn region jsPropAccess contained matchgroup=jsParens start=/\[/ end=/\]/ keepend extend
        \ contains=@jsClExpr,jsErrorCloseBrace,jsErrorCloseParen,jsErrorSemicolon
        \ nextgroup=@jsClAfterValue,jsAssign skipwhite skipnl
  hi! link jsParens Special
  syn cluster jsClAfterValue add=jsCall
  syn cluster jsClAfterValue add=jsCall,jsPropAccess

  " parenthesized regions
  syn region jsParenExpr matchgroup=jsParens start=/(/ end=/)/ keepend extend contains=@jsClExpr
        \ nextgroup=@jsClAfterValue skipwhite skipnl
        \ matchgroup=Error end=/[;]/
  syn cluster jsClExpr add=jsParenExpr
  syn cluster jsClTop add=jsParenExpr

" }}}

" {{{ operators

  syn match jsDot /\./ contained nextgroup=jsPropertyName,jsMethodName skipwhite skipnl
  hi! link jsDot Operator
  syn cluster jsClAfterValue add=jsDot

  syn match jsAssign /[-+/*&|^%]\?=[>=]\@!/ contained nextgroup=@jsClExpr skipwhite skipnl
  hi! link jsAssign Operator

  syn match jsCommaOperator /,/
  hi! link jsCommaOperator Operator

  syn match jsLogicalOperator /\%(&&\|||\|[*&^|]=\@!\|++\@!\|--\@!\|\/[*/]\@!\)/ contained
        \ nextgroup=@jsClExpr skipwhite skipnl
  syn match jsComparisonOperator /\%([!=]==\=\|[<>]=\=\)/ contained
        \ nextgroup=@jsClExpr skipwhite skipnl
  syn keyword jsComparisonOperator contained instanceof nextgroup=@jsClExpr skipwhite skipnl
  syn cluster jsClAfterValue add=jsLogicalOperator,jsComparisonOperator
  hi! link jsLogicalOperator Operator
  hi! link jsComparisonOperator Keyword

" }}}

" {{{ property names / method calls

  " property names
  syn match jsPropertyName /\h\w*/ contained nextgroup=@jsClAfterValue,jsAssign skipwhite skipnl

  " method calls
  syn match jsMethodName /\h\w*\ze\_s*(/ contained nextgroup=jsCall skipwhite skipnl

" }}}

" {{{ statements

  hi! link jsVar Macro

  syn region jsVarDecl matchgroup=jsVar start=/\<\%(var\|let\|const\)\>/ end=/;/
        \ keepend extend contains=@jsClExpr,jsVarComma,jsDictAssign,jsListAssignRegion
  syn match jsVarComma /,/ contained
  hi! link jsVarComma jsVar

  syn region jsReturnStatement matchgroup=jsStatement start=/\<\%(return\|throw\)\>/ end=/;/ end=/\ze}/
        \ keepend extend contains=@jsClExpr,jsErrorCloseParen,jsErrorCloseSquare
  hi! link jsStatement Statement

  syn region jsBreakStatement matchgroup=jsStatement start=/\<break\|continue\>/ end=/;/
        \ keepend extend
  hi! link jsBreakStatement Keyword

  syn cluster jsClTop add=jsVarDecl,jsReturnStatement,jsBreakStatement

" }}}

" {{{ for loops and conditionals

  syn match jsOrphanFlowControl /\<\%(if\|for\|while\|do\)\>/
  syn cluster jsClTop add=jsOrphanFlowControl
  hi! link jsOrphanFlowControl jsSyntaxError

  " simple braces for 'else' and 'for' blocks
  syn region jsFlowBraces matchgroup=jsConditional start=/{/ end=/}/ keepend extend contained contains=@jsClTop,@jsClImportExport
  " braces for 'if' clauses that will allow an 'else' afterward
  syn region jsConditionalBraces matchgroup=jsConditional start=/{/ end=/}/ keepend extend contained contains=@jsClTop,@jsClImportExport
        \ nextgroup=jsConditionalElse skipwhite skipnl
  " braces for a 'do' clause that will allow a 'while' afterward
  syn region jsDoWhileBraces matchgroup=jsConditional start=/{/ end=/}/ keepend extend contained contains=@jsClTop,@jsClImportExport
        \ nextgroup=jsDoWhile skipwhite skipnl

  " if ()
  syn region jsConditionalRegion matchgroup=jsConditional start=/\<if\_s*(/ end=/)/
        \ keepend extend contains=@jsClExpr,jsErrorSemicolon,jsErrorCloseBrace,jsErrorCloseSquare
        \ nextgroup=jsConditionalBraces skipwhite skipnl
  " else
  syn keyword jsConditionalElse contained else nextgroup=jsFlowBraces skipwhite skipnl
  hi! link jsConditionalElse jsConditional

  " while ()
  syn region jsFlowRegion matchgroup=jsConditional start=/\<while\_s*(/ end=/)/
        \ keepend extend contains=@jsClExpr,jsErrorSemicolon,jsErrorCloseBrace,jsErrorCloseSquare
        \ nextgroup=jsFlowBraces skipwhite skipnl
  " while () after a do {} clause
  syn region jsDoWhile matchgroup=jsConditional start=/\<while\_s*(/ end=/);/ contained
        \ keepend extend contains=@jsClExpr,jsErrorSemicolon,jsErrorCloseBrace,jsErrorCloseSquare
        \ nextgroup=jsFlowBraces skipwhite skipnl
  " do {} clause
  syn keyword jsDo do nextgroup=jsDoWhileBraces skipwhite skipnl
  hi! link jsDo jsConditional

  " for ()
  syn region jsFlowRegion matchgroup=jsConditional start=/\<for\_s*(/ end=/)/
        \ keepend extend contains=@jsClExpr,jsForSemicolon,jsForIn,jsForOf,jsForVar,jsErrorCloseBrace,jsErrorCloseSquare
        \ nextgroup=jsFlowBraces skipwhite skipnl
  syn match jsForSemicolon contained /;/
  hi! link jsForSemicolon jsConditional
  syn keyword jsForVar contained var let const
  hi! link jsForVar jsVar
  syn keyword jsForIn contained in
  hi! link jsForIn jsDict
  syn keyword jsForOf contained of
  hi! link jsForOf jsList

  hi! link jsFlowSimple jsConditional
  hi! link jsConditional Function


  syn region jsFlowPost matchgroup=jsConditional start=/\<while\_s*(/ end=/);/ keepend extend contained
  hi! link jsFlowPost jsConditional

  syn cluster jsClTop add=jsConditionalRegion,jsFlowRegion,jsDo

" }}}

" {{{ try/catch/finally

  hi! link jsTry Statement

  syn region jsTryBlock matchgroup=jsTry start=/\<try\_s*{/ end=/}/ keepend extend
        \ contains=@jsClTop,@jsClImportExport
        \ nextgroup=jsCatchStatement,jsFinallyStatement skipwhite skipnl

  syn region jsCatchStatement contained matchgroup=jsTry start=/\<catch\_s*(/ end=/)/ keepend extend
        \ contains=jsErrorCloseBrace,jsErrorSemicolon,jsErrorComma,jsErrorAssign
        \ nextgroup=jsCatchBlock skipwhite skipnl

  syn keyword jsFinallyStatement contained finally nextgroup=jsFinallyBlock skipwhite skipnl
  hi! link jsFinallyStatement jsTry

  syn region jsCatchBlock contained matchgroup=jsTry start=/{/ end=/}/ keepend extend
        \ contains=@jsClTop,@jsClImportExport
        \ nextgroup=jsFinallyStatement skipwhite skipnl
  syn region jsFinallyBlock contained matchgroup=jsTry start=/{/ end=/}/ keepend extend
        \ contains=@jsClTop,@jsClImportExport

  syn cluster jsClTop add=jsTryBlock

" }}}

" {{{ imports/exports

  syn cluster jsClImportExport add=jsImportRegion,jsExport

  hi! link jsImport Macro
  hi! link jsImportString Comment

  syn region jsExportRegion matchgroup=jsImport start=/\<export\_s\+\%(default\_s\+\)\=/ end=/;/
      \ contains=jsImportMembers keepend extend

  syn region jsImportRegion matchgroup=jsImport start=/\<import\>/ end=/;/
      \ contains=jsImportMembers,jsImportString,jsUserIdentifier,jsImportStar,jsImportFromAs,jsImportComma

  syn keyword jsImportFromAs contained from as
  hi! link jsImportFromAs jsImport

  syn match jsImportStar contained /\*/
  hi! link jsImportStar jsImportString

  syn region jsImportString contained start=/"/ end=/"/ keepend extend

  syn region jsImportMembers contained matchgroup=jsImport start=/{/ end=/}/ keepend extend
        \ contains=jsImportIdentifier,jsUserIdentifier,jsImportComma,jsImportFromAs
  syn match jsImportComma contained /,/
  hi! link jsImportComma jsImport

  syn match jsImportIdentifier contained /[$A-Za-z_][$A-Za-z_0-9]*/
  hi! link jsImportIdentifier jsImportString


  " import ImportClause 'from' <string>
  " import <string>

  " ImportClause:
  "     <identifier>
  "     [ <identifier> , ] '*' 'as' <identifier>
  "     [ <identifier> , ] NamedImports
  " NamedImports:
  "     '{\s*}'
  "     '{' ImportsList '}'
  "     '{' ImportsList ',' '}'
  " ImportsList:
  "     ImportSpecifier
  "     ImportsList , ImportSpecifier
  "     See 15.2.2
  " ImportSpecifier:
  "     <identifier>
  "     <identifier> 'as' <identifier>
  
  syn cluster jsClTop add=jsClImportExport

" }}}

" {{{ classes

  syn match jsClassIntro /\<class\_s\+\w\+\>/ nextgroup=jsClassBody,jsClassExtends skipwhite skipnl
        \ contains=jsClass,jsUserIdentifier
  syn keyword jsClass contained class extends
  hi! link jsClass Typedef
  syn cluster jsClExpr add=jsClassIntro

  syn match jsClassExtends /\<extends\_s\+\%([$A-Za-z0-9_.]\+\)/ contained
        \ nextgroup=jsClassBody skipwhite skipnl contains=jsUserIdentifier,jsDot,jsClass

  syn region jsClassBody matchgroup=jsClass start=/{/ end=/}/ contained
        \ contains=jsClassMethod,jsMethodGenerator,jsClassStatic,jsClassProperty,jsComment,jsGetter

  if b:javascript_es6
    syn match jsMethodGenerator contained /\*\ze\_s*[$A-Za-z_]/
    hi! link jsMethodGenerator Statement
  endif
  syn match jsClassMethod /[$A-Za-z_][$A-Za-z_0-9]*\ze\_s*(/ contained contains=jsClassConstructor
        \ nextgroup=jsFullFuncArgs skipwhite skipnl
  syn keyword jsClassConstructor contained constructor
  hi! link jsClassConstructor jsClass

  syn keyword jsClassStatic contained static nextgroup=jsClassProperty skipwhite skipnl
  syn region jsClassProperty contained start=/\<[$A-Za-z_][$A-Za-z0-9_]*\ze\_s*=/ keepend extend matchgroup=jsClass end=/;/
        \ contains=jsAssign
  hi! link jsClassStatic jsClass

  syn cluster jsClTop add=jsClassIntro


" }}}

" {{{ function declarations (top-level and lambdas)

  hi! link jsFullFunc Macro
  hi! link jsAnonFunc Include

  syn match jsFullFunc /\<function\ze\_s\+[$A-Za-z0-9_]/ nextgroup=jsFullFuncName skipwhite skipnl
  syn match jsAnonFunc /\<function\ze\_s*(/ contained nextgroup=jsAnonFuncArgs,jsAnonFuncName skipwhite skipnl
  syn cluster jsClExpr add=jsAnonFunc

  syn match jsFullFuncName contained /\<[$A-Za-z_][$A-Za-z0-9_]*\>/ contains=jsUserIdentifier
        \ nextgroup=jsFullFuncArgs skipwhite skipnl
  syn match jsAnonFuncName contained /\<[$A-Za-z_][$A-Za-z0-9_]*\>/ contains=jsUserIdentifier
        \ nextgroup=jsAnonFuncArgs skipwhite skipnl

  " full/anonymous function arg list
  syn region jsFullFuncArgs contained matchgroup=jsFullFunc start=/(/ end=/)/ contained
        \ keepend extend contains=jsFullFuncArgComma
        \ nextgroup=jsFullFuncBody skipwhite skipnl
        \ matchgroup=Error end=/,\_s*)/
  syn region jsAnonFuncArgs contained matchgroup=jsAnonFunc start=/(/ end=/)/ contained
        \ keepend extend contains=jsAnonFuncArgComma
        \ nextgroup=jsAnonFuncBody skipwhite skipnl
        \ matchgroup=Error end=/,\_s*)/

  " both types of function braces
  syn region jsFullFuncBody matchgroup=jsFullFuncBrace start=/{/ end=/}/ keepend extend contained
        \ contains=@jsClTop,@jsClImportExport
  hi! link jsFullFuncBrace jsFullFunc
  syn region jsAnonFuncBody matchgroup=jsAnonFuncBrace start=/{/ end=/}/ keepend extend contained contains=@jsClTop,@jsClImportExport
  syn region jsAnonFuncBody matchgroup=jsAnonFuncBrace start=/(/ end=/)/ keepend extend contained contains=@jsClExpr,jsErrorCloseBrace,jsErrorCloseSquare,jsErrorSemicolon
  hi! link jsAnonFuncBrace jsAnonFunc

  syn match jsAnonFuncArgComma /,/ contained
  hi! link jsAnonFuncArgComma jsAnonFunc

  syn match jsFuncFatArrow /\%(\<[$A-Za-z_][$A-Za-z_0-9]*\|(\%([$A-Za-z_][$A-Za-z_0-9]*\|\[\w\+\_s*\%(,\_s*\w\+\_s*\)*\]\)\_s*\%(,\_s*[$A-Za-z_][$A-Za-z_0-9]*\)*)\|()\)\_s*=>/ contains=jsAnonFuncArgComma extend
        \ nextgroup=jsAnonFuncBody skipwhite skipnl
  syn cluster jsClExpr add=jsFuncFatArrow
  hi! link jsFuncFatArrow Include

  "syn match jsAnonFunc
  
  syn cluster jsClTop add=jsFullFunc,jsAnonFunc,jsFuncFatArrow

" }}}

" async/await {{{

  if b:javascript_es2017
    " async functions
    syn keyword jsAsync async nextgroup=jsFuncFatArrow,jsFullFunc,jsAnonFunc skipwhite skipnl
    syn cluster jsClExpr add=jsAsync
    hi! link jsAsync Statement

    " await expressions
    syn keyword jsAwait await nextgroup=@jsClExpr skipwhite skipnl contained
    syn cluster jsClExpr add=jsAwait
    hi! link jsAwait jsAsync

    syn cluster jsClTop add=jsAwait,jsAsync
  endif

" }}}

" {{{

  " if we're embedded in a PHP script, also match PHP regions in some places
  if get(g:, 'main_syntax', '') == 'php'
    " NOTE: normally you'd syntax-include the syntax you're nesting, but that
    " would cause infinite recursion here
    syn cluster jsClExpr add=phpRegion
    syn cluster jsClInsideString add=phpRegion
  endif

" }}}

syn sync fromstart

let b:current_syntax = "javascript"

" vim: foldmethod=marker
