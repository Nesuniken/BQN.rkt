#lang brag
# Mostly adaptated from the offical specification
; https://mlochbaum.github.io/BQN/spec/grammar.html

#(...)? and [...] are equivalent

# @ and / are used to hide nodes
; that aren't needed after parsing
; https://docs.racket-lang.org/brag/#%28part._cuts-and-splices%29

program : [stmt] (/"⋄" [stmt])*
@stmt   : expr | import
expr    : subExpr
        | FuncExpr
        | 1M-Expr
        | 2M-Expr

import : [lhsComp def] bqn-req

@def : "←" | "⇐"
@assign: def | "↩"

bqn-req : /"•Require" (RKT-STRING | SUB-CUSTOM)

2Mod : [atom /"."] 2MOD-CUSTOM | 2MOD-LITERAL | 2M-block
     | /"(" (@2Mod    | 2M-Expr)  /")"  
1Mod : [atom /"."] 1MOD-CUSTOM | 1MOD-LITERAL | 1M-block
     | /"(" (@1Mod    | 1M-Expr)  /")"  
Func : [atom /"."] FUNC-CUSTOM | FUNC-LITERAL | FuncBlock
     | /"(" (Func     | FuncExpr) /")"  
atom : [atom /"."] SUB-CUSTOM  | sub-literal  | subBlock
     | /"(" (@subject | subExpr)  /")" | a-list | a-merge 

a-list   : /"⟨" /["⋄"] [(expr /"⋄")* expr /["⋄"]] /"⟩"
a-merge  : /"[" /["⋄"]  (expr /"⋄")* expr /["⋄"]  /"]"

any      : atom | Func | 1Mod | 2Mod
strand   : @any (/"‿" @any)+
@subject : atom | strand

/2M-Expr : 2Mod | 2MOD-CUSTOM assign 2M-Expr
/1M-Expr : 1Mod | 1MOD-CUSTOM assign 1M-Expr

Derv     : @Func |  Operand @1Mod | Operand @2Mod (subject | Func)
@Operand : subject | Func | Derv
Fork     : Derv  | (Operand | nothing) Derv Fork
Train    : @Fork  | Derv Fork
/FuncExpr : Train | FUNC-CUSTOM assign FuncExpr

arg         : [subject | nothing] Derv subExpr 
nothing     : [subject | nothing] Derv nothing | NOTHING
subExpr     : subject | arg | SUB-CUSTOM assign subExpr | SUB-CUSTOM Derv "↩" [subExpr]

@name     : 2MOD-CUSTOM | 1MOD-CUSTOM | FUNC-CUSTOM | SUB-CUSTOM
@lhs-sub  : NOTHING | lhsList | lhsArray
@lhs-any  : name | lhs-sub | /"(" lhs-elt /")"
lhs-atom  : lhs-any | /"(" lhsStrand /")"
lhs-elt   : lhs-any | lhsStrand
lhs-entry : @lhs-elt | lhs /"⇐" name
lhsStrand : lhs-atom (/"‿" lhs-atom)+
lhsList   : /"⟨" /["⋄"] [(lhs-entry /"⋄")* lhs-entry /["⋄"]] /"⟩"
lhsArray  : /"[" /["⋄"] [(lhs-elt   /"⋄")* lhs-elt   /["⋄"]] /"]"
@lhsComp  : lhs-sub | lhsStrand
/lhs      : SUB-CUSTOM | lhsComp | /"(" lhs /")"

body : /"{" (stmt /"⋄")* stmt /["⋄"]

FuncBlock : body /FUNC-BLOCK
1M-block  : body  (1M-IMMEDIATE | 1M-DELAYED)
2M-block  : body  (2M-IMMEDIATE | 2M-DELAYED)
subBlock  : body /SUB-BLOCK

@sub-literal : SUB-LITERAL
             | CHARACTER | STRING | RKT-STRING
             | INTEGER | REAL | NUMBER | real | number

number : real /"i" real
real   : REAL | ["¯"] ("∞" | "π" [/"e" INTEGER])