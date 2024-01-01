#lang brag
# Adapted from the offical grammer spec
; https://mlochbaum.github.io/BQN/spec/grammar.html

#(...)? and [...] are equivalent

# @ and / are used to hide nodes
; that aren't needed after parsing
; https://docs.racket-lang.org/brag/#%28part._cuts-and-splices%29

program : [line] (/"⋄" [line])*
@line   : stmt | import
import  : [@lhsComp def] bqn-req
stmt    : nothing | expr ;| [lhs-elt] "⇐"
@expr   : subExpr
        | FuncExpr
        | 1M-Expr
        | 2M-Expr

def : "←" | "⇐"
@assign: def | "↩"

bqn-req : /"•Require" (RKT-STRING | SUB-CUSTOM)

2Mod : [atom /"."] 2MOD-CUSTOM | 2mod-literal | 2M-Block
     | /"(" (@2Mod    | 2M-Expr)  /")"
1Mod : [atom /"."] 1MOD-CUSTOM | 1mod-literal | 1M-Block
     | /"(" (@1Mod    | 1M-Expr)  /")"
Func : [atom /"."] FUNC-CUSTOM | func-literal | FuncBlock
     | /"(" (Func     | FuncExpr) /")"
@atom : [atom /"."] SUB-CUSTOM  | sub-literal | subBlock
     | /"(" (@subject | subExpr)  /")" | a-list | a-merge

ns : [atom /"."]

a-list   : /"⟨" /["⋄"] [(expr /"⋄")* expr /["⋄"]] /"⟩"
a-merge  : /"[" /["⋄"]  (expr /"⋄")* expr /["⋄"]  /"]"

any      : atom | Func | 1Mod | 2Mod
strand   : @any (/"‿" @any)+
@subject : atom | strand

2M-Expr  : @2Mod | 2MOD-CUSTOM assign 2M-Expr
1M-Expr  : @1Mod | 1MOD-CUSTOM assign 1M-Expr

Derv     : @Func |  Operand @1Mod | Operand @2Mod (subject | Func)
@Operand : subject | Func | Derv
Fork     : Derv  | (Operand | nothing) Derv Fork
Train    : @Fork | Derv Fork
FuncExpr : Train | FUNC-CUSTOM assign FuncExpr

arg      : [subject | nothing] Derv subExpr 
nothing  : [subject | nothing] Derv nothing | NOTHING
subExpr  :  subject | arg | lhs assign subExpr | lhs Derv "↩" [subExpr]

@name      : 2MOD-CUSTOM | 1MOD-CUSTOM | FUNC-CUSTOM | SUB-CUSTOM
@lhs-sub   : NOTHING | lhsList | lhsNS | lhsArray | sub-literal
@lhs-any   : name | lhs-sub | /"(" lhs-elt /")"
@lhs-atom  : lhs-any | /"(" lhsStrand /")"
@lhs-elt   : lhs-any | lhsStrand
/lhs-pair  : lhs /"⇐" name
@lhs-entry : lhs-pair | lhs-elt
lhsStrand  : lhs-atom (/"‿" lhs-atom)+
lhsNS      : /"⟨" /["⋄"] [(lhs-entry /"⋄")* lhs-entry /["⋄"]] /"⟩"
lhsList    : /"⟨" /["⋄"] [(lhs-elt   /"⋄")* lhs-elt   /["⋄"]] /"⟩"
lhsArray   : /"[" /["⋄"] [(lhs-elt   /"⋄")* lhs-elt   /["⋄"]] /"]"
lhsComp   : lhs-sub | lhsStrand
@lhs       : SUB-CUSTOM | @lhsComp | /"(" lhs /")"

@headW  : lhs | "𝕨"
headX   : lhs | "𝕩"
HeadF   : lhs | FUNC-CUSTOM | "𝕗" | "𝔽"
HeadG   : lhs | FUNC-CUSTOM | "𝕘" | "𝔾"
FuncLab : FUNC-CUSTOM | "𝕊"
1ModLab : 1MOD-CUSTOM | "_𝕣"
2ModLab : 2MOD-CUSTOM | "_𝕣_"

unbound: ()
1ModHeadBase: HeadF 1ModLab
2ModHeadBase: HeadF 2ModLab HeadG

/1ModImmHead : @1ModHeadBase | unbound 1ModLab
/2ModImmHead : @2ModHeadBase | unbound 2ModLab unbound

no-mod      : ()
undo        : /"⁼"
@maybe-undo : no-mod | undo 
swap-undo   : /"˜"  /"⁼"
@maybe-w    : headW | unbound

/FuncHead : unbound FuncLab (maybe-undo | swap-undo) unbound 
          | maybe-w FuncLab  maybe-undo              headX
          | maybe-w FuncLab               swap-undo  headX
          | lhsComp

/1ModDelayHead :         1ModLab
               | maybe-w 1ModHeadBase maybe-undo headX
               | headW   1ModHeadBase swap-undo  headX

/2ModDelayHead :         2ModLab
               | maybe-w 2ModHeadBase maybe-undo headX
               | maybe-w 2ModHeadBase swap-undo  headX


body-close: (/";" | /MONAD | /DYAD)
ifElse  : expr /["⋄"] /"?" branch /body-close body
ifBreak : expr /["⋄"] /"?" branch /body-close [body]
branch  : /["⋄"] (stmt /"⋄")* (stmt | ifElse) /["⋄"]
body    : /["⋄"] (stmt /"⋄")* (stmt | ifBreak) /["⋄"]

head-block : ()
else-head  : ()

monad-head : ()
dyad-head  : ()

/Monad  : monad-head branch (/MONAD | ";")
/Dyad   : dyad-head  branch (/DYAD  | ";")

/Imm-Headless : else-head branch /";"
@Headless : Monad [Dyad]
          | Dyad

/FuncBody  : /["⋄"] FuncHead /["⋄"] /":" body /";"

FuncBlock  : /"{" head-block FuncBody+ [Headless] (/FUNC-BLOCK | /"}")
           | /"{" Headless /FUNC-BLOCK

/1M-Imm-Body  : /["⋄"] 1ModImmHead /["⋄"] /":" body /";"
1M-Imm-Block  : /"{" head-block 1M-Imm-Body+ [Imm-Headless] (/1M-BLOCK | /"}")
              | /"{" Imm-Headless /1M-BLOCK


/1M-Del-Body : /["⋄"] 1ModDelayHead /["⋄"] /":" body /";"

1M-Del-Block : /"{" head-block 1M-Del-Body+ [Headless] (/1M-BLOCK | /"}")
             | /"{" Headless /1M-BLOCK

1M-Block : 1M-Imm-Block | 1M-Del-Block


/2M-Imm-Body  : /["⋄"] 2ModImmHead /["⋄"] /":" body /";"
2M-Imm-Block  : /"{" head-block 2M-Imm-Body+ [Imm-Headless] (/2M-BLOCK | /"}")
              | /"{" Imm-Headless /2M-BLOCK


/2M-Del-Body : /["⋄"] 2ModDelayHead /["⋄"] /":" body /";"

2M-Del-Block : /"{" head-block 2M-Del-Body+ [Headless] (/2M-BLOCK | /"}")
             | /"{" Headless /2M-BLOCK

2M-Block : 2M-Imm-Block | 2M-Del-Block

subHead : /["⋄"] SUB-CUSTOM /["⋄"] /":"
subBlock  : /"{" [/subHead] body (/"}" | /SUB-BLOCK)

@2mod-literal : 2MOD-LITERAL | "_𝕣_"

@1mod-literal : 1MOD-LITERAL | "_𝕣" 

@func-literal : FUNC-LITERAL
              | "𝕎" | "𝕊" | "𝕏" | "𝔽" | "𝔾"

@sub-literal  : SUB-LITERAL | special-sub
              | CHARACTER | STRING | RKT-STRING
              | INTEGER | REAL | NUMBER | real | number

@special-sub : "𝕨" | "𝕤" | "𝕩" | "𝕗" | "𝕣" | "𝕘"
number : real /"i" real
real   : REAL | ["¯"] ("∞" | "π" [/"e" INTEGER])