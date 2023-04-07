#lang brag
# Mostly adaptated from the offical specification
; https://mlochbaum.github.io/BQN/spec/grammar.html

#()? and [] are equivalent

# @ and / are used to hide nodes
; that aren't needed after parsing
; https://docs.racket-lang.org/brag/#%28part._cuts-and-splices%29

program : [expr] (/"⋄" ([expr] | /nothing))*
expr    : subExpr
        | Func
        | 1Mod
        | 2Mod

2Mod : [atom /"."] 2Mod-custom | @2Mod-literal | /"(" (@2Mod    | 2M-Expr)  /")"  
1Mod : [atom /"."] 1Mod-custom | @1Mod-literal | /"(" (@1Mod    | 1M-Expr)  /")"  
Func : [atom /"."] func-custom | @func-literal | /"(" (@Func    | FuncExpr) /")"  
atom : [atom /"."] sub-custom  | @sub-literal  | array  | /"(" (@subject | subExpr)  /")"

array : "⟨" /["⋄"] [(expr /"⋄")* expr /["⋄"]] /"⟩"
      | "[" /["⋄"]  (expr /"⋄")* expr /["⋄"]  /"]"

any     : atom | Func | 1Mod | 2Mod
strand  : @any (/"‿" @any)+
subject : @atom | strand

2M-Expr : 2Mod | @2Mod-custom "↩" 2M-Expr
1M-Expr : 1Mod | @1Mod-custom "↩" 1M-Expr

Derv     : Func |  Operand 1Mod | Operand 2Mod (subject | Func)
@Operand : subject | Derv
Fork     : Derv  | (Operand | /nothing) Derv Fork
Train    : Fork  | Derv Fork
FuncExpr : Train | @func-custom "↩" FuncExpr

arg     : [subject | /nothing] Derv subExpr
nothing : [subject |  nothing] Derv nothing | NOTHING
subExpr : @subject | arg | @sub-custom "↩" subExpr | @sub-custom Derv "↩" subExpr

func-literal : FUNC-PRIM | func-special      | ("•" func-custom)
1Mod-literal : 1MOD-PRIM | "_𝕣"  | "⁼" | "˜" | ("•" 1Mod-custom)
2Mod-literal : 2MOD-PRIM | "_𝕣_"             | ("•" 2Mod-custom)
 sub-literal : INTEGER | real | complex 
             | CHARACTER | STRING
             | "•" sub-custom
             | sub-special

func-special : "𝕎" | "𝕊" | "𝕏" | "𝔽" | "𝔾"
sub-special  : "𝕨" | "𝕤" | "𝕩" | "𝕣" 

mantissa : "π" | INTEGER [/"." INTEGER]
exponent : ["¯"] INTEGER
real     : ["¯"] ("∞" | mantissa [/("E" | "e") exponent])
complex  : real /("I" | "i") real

#Resolves the janky lexing needed for numerics
sub-custom  : SUB-CUSTOM  |  "e" | "i" 
func-custom : FUNC-CUSTOM |  "E" | "I" 
1Mod-custom : /"_" (@sub-custom | @func-custom)
2Mod-custom : @1Mod-custom /"_"