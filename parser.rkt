#lang brag
# Mostly adaptated from the offical specification
; https://mlochbaum.github.io/BQN/spec/grammar.html

#(...)? and [...] are equivalent

# @ and / are used to hide nodes
; that aren't needed after parsing
; https://docs.racket-lang.org/brag/#%28part._cuts-and-splices%29

program : /["⋄"] (stmt /"⋄")* stmt /["⋄"]
@stmt   : def | expr
expr    : subExpr
        | FuncExpr
        | 1M-Expr
        | 2M-Expr

@assign: "←" | "⇐"

def : 2MOD-CUSTOM assign 2M-Expr
    | 1MOD-CUSTOM assign 1M-Expr
    | FUNC-CUSTOM assign FuncExpr
    | SUB-CUSTOM  assign subExpr

2Mod : [atom /"."] 2MOD-CUSTOM | 2MOD-LITERAL | 2M-block
     | /"(" (@2Mod    | 2M-Expr)  /")"  
1Mod : [atom /"."] 1MOD-CUSTOM | 1MOD-LITERAL | 1M-block
     | /"(" (@1Mod    | 1M-Expr)  /")"  
Func : [atom /"."] FUNC-CUSTOM | FUNC-LITERAL | FuncBlock
     | /"(" (@Func    | FuncExpr) /")"  
atom : [atom /"."] SUB-CUSTOM  | sub-literal  | subBlock
     | /"(" (@subject | subExpr)  /")" | a-list | a-merge 

a-list   : /"⟨" /["⋄"] [(expr /"⋄")* expr /["⋄"]] /"⟩"
a-merge  : /"[" /["⋄"]  (expr /"⋄")* expr /["⋄"]  /"]"

any      : atom | Func | 1Mod | 2Mod
strand   : @any (/"‿" @any)+
@subject : @atom | strand

2M-Expr : 2Mod | 2MOD-CUSTOM "↩" 2M-Expr
1M-Expr : 1Mod | 1MOD-CUSTOM "↩" 1M-Expr

Derv     : @Func |  Operand @1Mod | Operand @2Mod (subject | Func)
@Operand : subject | Derv
Fork     : Derv  | (Operand | /nothing) Derv Fork
Train    : @Fork  | Derv Fork
FuncExpr : Train | FUNC-CUSTOM "↩" FuncExpr

arg         : [subject | /nothing] Derv subExpr 
nothing     : [subject |  nothing] Derv nothing | NOTHING
subExpr     : @subject | arg | SUB-CUSTOM "↩" subExpr | SUB-CUSTOM Derv "↩" [subExpr]
          
sub-literal : SUB-LITERAL | NUMBER | CHARACTER | STRING

body : /["⋄"] (stmt /"⋄" | expr /["⋄"] "?" /["⋄"])* stmt /["⋄"]

/block : /"{" body (/";" body)*

FuncBlock : @block /FUNC-BLOCK
1M-block  : block (1M-IMMEDIATE | 1M-DELAYED)
2M-block  : block (2M-IMMEDIATE | 2M-DELAYED)
subBlock  : @block /SUB-BLOCK