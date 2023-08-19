#lang brag
# Mostly adaptated from the offical grammer spec
; https://mlochbaum.github.io/BQN/spec/grammar.html

#(...)? and [...] are equivalent

# @ and / are used to hide nodes
; that aren't needed after parsing
; https://docs.racket-lang.org/brag/#%28part._cuts-and-splices%29

program : [line] (/"⋄" [line])*
@line   : stmt | import
import  : [lhs-comp def] bqn-req
stmt    : expr ;| [lhs-elt] "⇐"
@expr   : subExpr
        | FuncExpr
        | 1M-Expr
        | 2M-Expr

@def : "←" | "⇐"
@assign: def | "↩"

bqn-req : /"•Require" (RKT-STRING | SUB-CUSTOM)

2Mod : [atom /"."] 2MOD-CUSTOM | 2mod-literal | 2M-block
     | /"(" (@2Mod    | 2M-Expr)  /")"  
1Mod : [atom /"."] 1MOD-CUSTOM | 1mod-literal | 1M-block
     | /"(" (@1Mod    | 1M-Expr)  /")"  
Func : [atom /"."] FUNC-CUSTOM | func-literal | FuncBlock
     | /"(" (Func     | FuncExpr) /")"  
atom : [atom /"."] SUB-CUSTOM  | sub-literal  | subBlock
     | /"(" (@subject | subExpr)  /")" | a-list | a-merge 

a-list   : /"⟨" /["⋄"] [(expr /"⋄")* expr /["⋄"]] /"⟩"
a-merge  : /"[" /["⋄"]  (expr /"⋄")* expr /["⋄"]  /"]"

any      : atom | Func | 1Mod | 2Mod
strand   : @any (/"‿" @any)+
@subject : atom | strand

2M-Expr  : 2Mod | 2MOD-CUSTOM assign 2M-Expr
1M-Expr  : 1Mod | 1MOD-CUSTOM assign 1M-Expr

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
/lhs-entry : @lhs-elt | lhs /"⇐" name
lhsStrand  : lhs-atom (/"‿" lhs-atom)+
lhsNS      : /"⟨" /["⋄"] [(lhs-entry /"⋄")* lhs-entry /["⋄"]] /"⟩"
lhsList    : /"⟨" /["⋄"] [(lhs-elt   /"⋄")* lhs-elt   /["⋄"]] /"⟩"
lhsArray   : /"[" /["⋄"] [(lhs-elt   /"⋄")* lhs-elt   /["⋄"]] /"]"
lhsComp    : lhs-sub | lhsStrand
/lhs-comp  : lhsComp
@lhs       : SUB-CUSTOM | lhs-comp | /"(" lhs /")"

headW   : lhs | "𝕨"
headX    : lhs | "𝕩"
HeadF    : lhs | FUNC-CUSTOM | "𝕗" | "𝔽"
HeadG    : lhs | FUNC-CUSTOM | "𝕘" | "𝔾"
FuncLab  : FUNC-CUSTOM | "𝕊"
1ModLab  : 1MOD-CUSTOM | "_𝕣"
2ModLab  : 2MOD-CUSTOM | "_𝕣_"
1ModImm  : 1ModLab | HeadF 1ModLab
2ModImm  : 2ModLab | HeadF 2ModLab HeadG

no-mod      : /""
undo        : /"⁼"
@maybe-undo : no-mod | undo 
swap-undo   : /"˜"  /"⁼" 

FuncHead  :         FuncLab (maybe-undo | swap-undo) 
          | [headW] FuncLab maybe-undo headX
          |  headW  FuncLab swap-undo  headX
          | lhsComp

1ModDelay :         1ModLab
          | [headW] 1ModImm maybe-undo headX
          |  headW  1ModImm swap-undo  headX

2ModDelay :         2ModLab
          | [headW] 2ModImm maybe-undo headX
          |  headW  2ModImm swap-undo  headX


body : /["⋄"] (stmt /"⋄")* stmt /["⋄"]

FuncBody  : /["⋄"] FuncHead /["⋄"] /":" body
FuncBlock : /"{" FuncBody (/";" FuncBody)* [/";" body] /"}"
          | /"{" body /FUNC-BLOCK

1ModBody  : /["⋄"] (1ModImm | 1ModDelay) /["⋄"] /":" body
1M-block  : /"{" 1ModBody (/";" 1ModBody)* [/";" body] /"}"
          | /"{" body  (1M-IMMEDIATE | 1M-DELAYED)

2ModBody  : /["⋄"] (2ModImm | 2ModDelay) /["⋄"] /":" body
2M-block  : /"{" 2ModBody (/";" 2ModBody)* [/";" body] /"}"
          | /"{" body  (2M-IMMEDIATE | 2M-DELAYED)

subBlock  : /"{" /["⋄"] SUB-CUSTOM /["⋄"] /":" body [/";" body] /"}"
          | /"{" body /SUB-BLOCK

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