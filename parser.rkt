#lang brag
# Mostly adaptated from the offical grammer spec
; https://mlochbaum.github.io/BQN/spec/grammar.html

#(...)? and [...] are equivalent

# @ and / are used to hide nodes
; that aren't needed after parsing
; https://docs.racket-lang.org/brag/#%28part._cuts-and-splices%29

program : [line] (/"⋄" [line])*
@line   : stmt | import
import  : [lhsComp def] bqn-req
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
/lhs-entry : lhs-elt | lhs /"⇐" name
lhsStrand  : lhs-atom (/"‿" lhs-atom)+
lhsNS      : /"⟨" /["⋄"] [(lhs-entry /"⋄")* lhs-entry /["⋄"]] /"⟩"
lhsList    : /"⟨" /["⋄"] [(lhs-elt   /"⋄")* lhs-elt   /["⋄"]] /"⟩"
lhsArray   : /"[" /["⋄"] [(lhs-elt   /"⋄")* lhs-elt   /["⋄"]] /"]"
@lhsComp   : lhs-sub | lhsStrand
@lhs       : SUB-CUSTOM | lhsComp | /"(" lhs /")"

@headW  : lhs | "𝕨"
headX   : lhs | "𝕩"
HeadF   : lhs | FUNC-CUSTOM | "𝕗" | "𝔽"
HeadG   : lhs | FUNC-CUSTOM | "𝕘" | "𝔾"
FuncLab : FUNC-CUSTOM | "𝕊"
1ModLab : 1MOD-CUSTOM | "_𝕣"
2ModLab : 2MOD-CUSTOM | "_𝕣_"
/1ModImmHead : 1ModLab | HeadF 1ModLab
/2ModImmHead : 2ModLab | HeadF 2ModLab HeadG

no-mod      : ()
undo        : /"⁼"
@maybe-undo : no-mod | undo 
swap-undo   : /"˜"  /"⁼" 

/FuncHead :         FuncLab (maybe-undo | swap-undo) 
          | [headW] FuncLab maybe-undo headX
          |  headW  FuncLab swap-undo  headX
          | lhsComp

1ModDelayHead :         1ModImmHead
              | [headW] 1ModImmHead maybe-undo headX
              |  headW  1ModImmHead swap-undo  headX

2ModDelayHead :         1ModImmHead
              | [headW] 2ModImmHead maybe-undo headX
              |  headW  2ModImmHead swap-undo  headX

else-head : ()
body : /["⋄"] (stmt /"⋄")* stmt /["⋄"]

/FuncBody : /["⋄"] FuncHead /["⋄"] /":" body
/FuncElse : /";" else-head body
FuncBlock : /"{" FuncBody (/";" FuncBody)* [FuncElse] /"}"
          | /"{" body /FUNC-BLOCK


1ModElse     : /";" else-head body

/1M-Imm-Body : /["⋄"] 1ModImmHead /["⋄"] /":" body
1M-Imm-Block : /"{" 1M-Imm-Body (/";" 1M-Imm-Body)* [1ModElse] /"}"
             | /"{" body /1M-IMMEDIATE

1M-Del-Body  : /["⋄"] 1ModDelayHead /["⋄"] /":" body
1M-Del-Block : /"{" 1M-Del-Body (/";" 1M-Del-Body)* [1ModElse] /"}"
             | /"{" body /1M-DELAYED

@1M-block : 1M-Imm-Block | 1M-Del-Block


2ModElse     : /";" else-head body

/2M-Imm-Body  : /["⋄"] 2ModImmHead /["⋄"] /":" body
2M-Imm-Block : /"{" 2M-Imm-Body (/";" 2M-Imm-Body)* [2ModElse] /"}"
             | /"{" body /2M-IMMEDIATE

2M-Del-Body  : /["⋄"] 2ModDelayHead /["⋄"] /":" body
2M-Del-Block : /"{" 2M-Del-Body (/";" 2M-Del-Body)* [2ModElse] /"}"
             | /"{" body /2M-DELAYED

@2M-block : 2M-Imm-Block | 2M-Del-Block

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