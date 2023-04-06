#lang brag
# Mostly adaptated from the offical specification
; https://mlochbaum.github.io/BQN/spec/grammar.html

#()? and [] are equivalent

# @ and / are used to hide nodes
; that aren't needed after parsing
; https://docs.racket-lang.org/brag/#%28part._cuts-and-splices%29

program : [stmt] (/"⋄" ([stmt] | /nothing))*
@stmt   : expr | def | export
export  : lhs-elt /"⇐"
def     : subDefExpr
        | FuncDefExpr
        | 1M-DefExpr
        | 2M-DefExpr

@expr   : subExpr
        | Func
        | 1Mod
        | 2Mod

2Mod : [atom /"."] 2Mod-custom | @2Mod-literal | 2M-block           | /"(" (@2Mod    | 2M-Expr)  /")"  
1Mod : [atom /"."] 1Mod-custom | @1Mod-literal | 1M-block           | /"(" (@1Mod    | 1M-Expr)  /")"  
Func : [atom /"."] func-custom | @func-literal | FuncBlock          | /"(" (@Func    | FuncExpr) /")"  
atom : [atom /"."] sub-custom  | @sub-literal  | subBlock  | array  | /"(" (@subject | subExpr)  /")"

2M-Def  : 2Mod | /"(" (@2M-Def  | 2M-DefExpr)  /")"
1M-Def  : 1Mod | /"(" (@1M-Def  | 1M-DefExpr)  /")"
FuncDef : Func | /"(" (@FuncDef | FuncDefExpr) /")"
atomDef : atom | /"(" (@subDef  | subDefExpr)  /")" | defArray

array : "⟨" /["⋄"] [(expr /"⋄")* expr /["⋄"]] /"⟩"
      | "[" /["⋄"]  (expr /"⋄")* expr /["⋄"]  /"]"

defArray : "⟨" /["⋄"] [((def | expr) /"⋄")* (def | expr) /["⋄"]] /"⟩"
         | "[" /["⋄"]  ((def | expr) /"⋄")* (def | expr) /["⋄"]  /"]"

any     : atom | Func | 1Mod | 2Mod
strand  : @any (/"‿" @any)+
subject : @atom | strand

anyDef    : atomDef | FuncDef | 1M-Def | 1M-Def
strandDef : @anyDef (/"‿" @anyDef)+
subDef    : @atomDef | strandDef

@assign    : "←" | "⇐" | "↩"
2M-DefExpr : 2M-Def | @2Mod-custom assign (2M-Expr | 2M-DefExpr) 
1M-DefExpr : 1M-Def | @1Mod-custom assign (1M-Expr | 1M-DefExpr)

2M-Expr : 2Mod | @2Mod-custom "↩" 2M-Expr
1M-Expr : 1Mod | @1Mod-custom "↩" 1M-Expr

Derv     : Func |  Operand 1Mod | Operand 2Mod (subject | Func)
@Operand : subject | Derv
Fork     : Derv  | (Operand | /nothing) Derv Fork
Train    : Fork  | Derv Fork
FuncExpr : Train | @func-custom "↩" FuncExpr

DefDerv     : Derv | FuncDef |  DefOperand 1M-Def | DefOperand 2M-Def (subDef | FuncDef)
@DefOperand : Operand  | subDef  | DefDerv
DefFork     : Fork     | DefDerv | (DefOperand | DefNothing) DefDerv DefFork
DefTrain    : DefFork  | DefDerv DefFork
FuncDefExpr : DefTrain | @func-custom assign (FuncExpr | FuncDefExpr)

arg     : [subject | /nothing] Derv subExpr
nothing : [subject |  nothing] Derv nothing | NOTHING
subExpr : @subject | arg | lhs "↩" subExpr | lhs Derv "↩" subExpr

DefArg     : arg     | [subDef | DefNothing] DefDerv subDefExpr
DefNothing : nothing | [subDef | DefNothing] DefDerv DefNothing  | NOTHING
subDefExpr : subDef  |  DefArg | lhs assign subDefExpr | lhs DefDerv "↩" [subDefExpr]

name      : sub-custom | func-custom | 1Mod-custom | 2Mod-custom
@lhs-sub  : NOTHING | lhsList | lhsArray
lhs-any   : name | lhs-sub | /"(" lhs-elt /")"
lhs-atom  : lhs-any | /"(" lhsStrand /")"
lhs-elt   : lhs-any | lhsStrand
lhs-entry : lhs-elt | lhs "⇐" name
lhsStrand : lhs-atom (/"‿" lhs-atom)+
lhsList   : "⟨" /["⋄"] [(lhs-entry /"⋄")* lhs-entry /["⋄"]] "⟩"
lhsArray  : "[" /["⋄"] [(lhs-elt   /"⋄")* lhs-elt   /["⋄"]] "]"
lhsComp   : lhs-sub | lhsStrand
@lhs      : sub-custom | lhsComp | /"(" lhs /")"

headW    : lhs | "𝕨"
headX    : lhs | "𝕩"
HeadF    : lhs | func-custom | "𝕗" | "𝔽"
HeadG    : lhs | func-custom | "𝕘" | "𝔾"
2ModLab  : 2Mod-custom | "_𝕣_"
FuncName : func-custom |  "𝕊"
1ModName : [HeadF] (1Mod-custom | "_𝕣")
2ModName : (2ModLab | HeadF 2ModLab HeadG)

F-head  : FuncName
        | [headW] FuncName      ["⁼"]  headX
        |  headW  FuncName  "˜"  "⁼"   headX
        |         FuncName ["˜"] "⁼"
        | lhsComp

1M-head : 1ModName
        | [headW] 1ModName      ["⁼"] headX
        |  headW  1ModName  "˜"  "⁼"  headX
        | lhsComp

2M-head : 2ModName
        | [headW] 2ModName     ["⁼"] headX
        |  headW  2ModName "˜"  "⁼"  headX
        | lhsComp

body : /["⋄"] (stmt /"⋄" | (def | expr) /["⋄"] "?" /["⋄"])* stmt /["⋄"]

 F-case : (/["⋄"] F-head     /["⋄"] /":") body
1M-case : (/["⋄"] 1M-head    /["⋄"] /":") body
2M-case : (/["⋄"] 2M-head    /["⋄"] /":") body
 S-case : (/["⋄"] sub-custom /["⋄"] /":") body

FuncBlock : /"{"  F-case (/";" ( F-case | body))* /"}"
          | X-block /FUNC-BLOCK

 1M-block : /"{" 1M-case (/";" (1M-case | body))* /"}"
          | X-block   /1M-BLOCK

 2M-block : /"{" 2M-case (/";" (2M-case | body))* /"}"
          | X-block   /2M-BLOCK

 subBlock : /"{"  S-case (/";" ( S-case | body))* /"}"
          | X-block  /SUB-BLOCK

 @X-block : /"{"    body (/";"            body )*

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