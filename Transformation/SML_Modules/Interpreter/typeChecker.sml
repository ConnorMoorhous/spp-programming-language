(* =========================================================================================================== *)
structure TypeChecker =
struct

open Model;
open CONCRETE_REPRESENTATION;

(* =========================================================================================================== *)
(*
    Here is where your typeCheck and typeOf definitions go. The primary challenge here is to translate the parse 
    expression notation we used in M2 to the actual SML tree patterns used in the TL System. See the comments in
    the semantics.sml file for a more detailed discussion on this topic. 
*)

(***    typeOf   ***)

fun typeOf( itree(inode("expression",_), [ logicalOr ] ), m ) =
        typeOf( logicalOr, m )
 
  | typeOf( itree(inode("logicalOr",_), [ logicalOr, itree(inode("or",_), []), logicalAnd ] ), m ) =
    let 
        val t1 = typeOf(logicalOr, m)
        val t2 = typeOf(logicalAnd, m)
    in
        if t1 = t2 andalso t1 = BOOL then BOOL
        else ERROR
    end
  
  | typeOf( itree(inode("logicalOr",_), [ logicalAnd ] ), m ) =
        typeOf( logicalAnd, m )

  | typeOf( itree(inode("logicalAnd",_), [ logicalAnd, itree(inode("and",_), []), equality ] ), m ) =
    let 
        val t1 = typeOf(logicalAnd, m)
        val t2 = typeOf(equality, m)
    in
        if t1 = t2 andalso t1 = BOOL then BOOL
        else ERROR
    end

  | typeOf( itree(inode("logicalAnd",_), [ equality ] ), m ) =
        typeOf( equality, m )

  | typeOf( itree(inode("equality",_), [ equality, itree(inode("equalityOperator",_), [ itree(inode("!=",_), []) ]), relation ] ), m ) =
    let 
        val t1 = typeOf(equality, m)
        val t2 = typeOf(relation, m)
    in
        if t1 = t2 andalso (t1 = INT orelse t1 = BOOL) then BOOL
        else ERROR
    end
   
  | typeOf( itree(inode("equality",_), [ equality, itree(inode("equalityOperator",_), [ itree(inode("==",_), []) ]), relation ] ), m ) =
    let 
        val t1 = typeOf(equality, m)
        val t2 = typeOf(relation, m)
    in
        if t1 = t2 andalso (t1 = INT orelse t1 = BOOL) then BOOL
        else ERROR
    end
  
  | typeOf( itree(inode("equality",_), [ relation ] ), m ) =
        typeOf( relation, m )

  | typeOf( itree(inode("relation",_), [ relation, itree(inode("relationOperator",_), [ itree(inode("<",_), []) ]), additive ] ), m ) =
    let 
        val t1 = typeOf(relation, m)
        val t2 = typeOf(additive, m)
    in
        if t1 = t2 andalso t1 = INT then BOOL
        else ERROR
    end
   
  | typeOf( itree(inode("relation",_), [ relation, itree(inode("relationOperator",_), [ itree(inode(">",_), []) ]), additive ] ), m ) =
    let 
        val t1 = typeOf(relation, m)
        val t2 = typeOf(additive, m)
    in
        if t1 = t2 andalso t1 = INT then BOOL
        else ERROR
    end
    
  | typeOf( itree(inode("relation",_), [ additive ] ), m ) =
        typeOf( additive, m )

  | typeOf( itree(inode("additive",_), [ additive, itree(inode("additiveOperator",_), [ itree(inode("+",_), []) ]), multiplicative ] ), m ) =
    let 
        val t1 = typeOf(additive, m)
        val t2 = typeOf(multiplicative, m)
    in
        if t1 = t2 andalso t1 = INT then INT
        else ERROR
    end

  | typeOf( itree(inode("additive",_), [ additive, itree(inode("additiveOperator",_), [ itree(inode("-",_), []) ]), multiplicative ] ), m ) =
    let 
        val t1 = typeOf(additive, m)
        val t2 = typeOf(multiplicative, m)
    in
        if t1 = t2 andalso t1 = INT then INT
        else ERROR
    end

  | typeOf( itree(inode("additive",_), [ multiplicative ] ), m ) =
        typeOf( multiplicative, m )

  | typeOf( itree(inode("multiplicative",_), [ multiplicative, itree(inode("multiplicativeOperator",_), [ itree(inode("*",_), []) ]), unary ] ), m ) =
    let 
        val t1 = typeOf(multiplicative, m)
        val t2 = typeOf(unary, m)
    in
        if t1 = t2 andalso t1 = INT then INT
        else ERROR
    end

  | typeOf( itree(inode("multiplicative",_), [ multiplicative, itree(inode("multiplicativeOperator",_), [ itree(inode("/",_), []) ]), unary ] ), m ) =
    let 
        val t1 = typeOf(multiplicative, m)
        val t2 = typeOf(unary, m)
    in
        if t1 = t2 andalso t1 = INT then INT
        else ERROR
    end

  | typeOf( itree(inode("multiplicative",_), [ multiplicative, itree(inode("multiplicativeOperator",_), [ itree(inode("%",_), []) ]), unary ] ), m ) =
    let 
        val t1 = typeOf(multiplicative, m)
        val t2 = typeOf(unary, m)
    in
        if t1 = t2 andalso t1 = INT then INT
        else ERROR
    end

  | typeOf( itree(inode("multiplicative",_), [ unary ] ), m ) =
        typeOf( unary, m )

  | typeOf( itree(inode("unary",_), [ itree(inode("-",_), []), unary ] ), m ) =
    let 
        val t = typeOf(unary, m)
    in
        if t = INT then t
        else ERROR
    end

  | typeOf( itree(inode("unary",_), [ itree(inode("!",_), []), unary ] ), m ) =
    let 
        val t = typeOf(unary, m)
    in
        if t = BOOL then t
        else ERROR
    end

  | typeOf( itree(inode("unary",_), [ exponent ] ), m ) =
        typeOf( exponent, m )

  | typeOf( itree(inode("exponent",_), [ factor, itree(inode("^",_), []), exponent ] ), m ) =
    let 
        val t1 = typeOf(factor, m)
        val t2 = typeOf(exponent, m)
    in
        if t1 = t2 andalso t1 = INT then INT
        else ERROR
    end

  | typeOf( itree(inode("exponent",_), [ factor ] ), m ) =
        typeOf( factor, m )

  | typeOf( itree(inode("factor",_), [ itree(inode("(",_), []), expression, itree(inode(")",_), []) ] ), m ) =
        typeOf( expression, m )

  | typeOf( itree(inode("factor",_), [ itree(inode("|",_), []), expression, itree(inode("|",_), []) ] ), m ) =
    let 
        val t = typeOf(expression, m)
    in
        if t = INT then t
        else ERROR
    end
        
  | typeOf( itree(inode("factor",_), [ factorChild ] ), m ) =
        typeOf( factorChild, m ) (* prefix, postfix,  *)

  | typeOf( itree(inode("prefix",_), [ operator, idTree ] ), m ) =
    let 
        val id = getLeaf(idTree)
        val t = getType(accessEnv(id, m))
    in
        if t = INT then t
        else ERROR
    end

  | typeOf( itree(inode("postfix",_), [ idTree, operator ] ), m ) =
    let 
        val id = getLeaf(idTree)
        val t = getType(accessEnv(id, m))
    in
        if t = INT then t
        else ERROR
    end
    
  | typeOf( intTree as itree(inode("integer",_), [ _ ] ), m ) = INT

  | typeOf( boolTree as itree(inode("boolean",_), [ _ ] ), m ) = BOOL
  
  | typeOf( idTree as itree(inode("id",_), [ _ ] ), m ) = 
    let
        val id = getLeaf(idTree)
        val t = getType(accessEnv(id, m))
    in
        t
    end
    
 | typeOf _ = raise Fail("error in typeChecker.typeOf - this should never occur");


(***    typeCheck   ***) 

fun typeCheck( itree(inode("statementList",_), [ statement, statementList ] ), m ) = 
        typeCheck( statementList, typeCheck(statement, m))
        
  | typeCheck( itree(inode("statementList",_), [ epsilon ] ), m ) = m
  
  | typeCheck( itree(inode("statement",_), [ singleStatement, itree(inode(";",_), []) ] ), m ) = 
        typeCheck(singleStatement, m)
        
  | typeCheck( itree(inode("statement",_), [ blockStatement ] ), m ) = 
        typeCheck(blockStatement, m)
        
  | typeCheck( itree(inode("singleStatement",_), [ itree(inode("skip",_), [ epsilon ] ) ]), m ) = m
  
  | typeCheck( itree(inode("singleStatement",_), [ child ] ), m ) = 
        typeCheck(child, m)   (* declaration, assignment, out, prefix, postfix *)
        
  | typeCheck( itree(inode("blockStatement",_), [ child ] ), m ) = 
        typeCheck(child, m)   (* conditional, iterative, block *) 
        
  | typeCheck( itree(inode("declaration",_), [ itree(inode("type",_), [itree(inode("int",_), [])]), idTree ] ), m0 ) =
        let
            val id = getLeaf(idTree)
            val m1 = updateEnv(id, INT, Model.getCounter(m0), m0)
        in
            m1
        end
        
  | typeCheck( itree(inode("declaration",_), [ itree(inode("type",_), [itree(inode("bool",_), [])]), idTree ] ), m0 ) =
        let 
            val id = getLeaf(idTree)
            val m1 = updateEnv(id, BOOL, Model.getCounter(m0), m0)
        in
            m1
        end
        
  | typeCheck( itree(inode("declaration",_), [ itree(inode("type",_), [itree(inode("int",_), [])]), idTree, itree(inode("=",_), []), expression ] ), m0 ) =
        let
            val msg = "Type failed to match during declaration!" 
            val id = getLeaf(idTree)
            val m1 = updateEnv(id, INT, Model.getCounter(m0), m0)
            val t = typeOf(expression, m0)
        in
            if t = INT then m1 else tError msg 
        end
        
  | typeCheck( itree(inode("declaration",_), [ itree(inode("type",_), [itree(inode("bool",_), [])]), idTree, itree(inode("=",_), []), expression ] ), m0 ) =
        let
            val msg = "Type failed to match during declaration!" 
            val id = getLeaf(idTree)
            val m1 = updateEnv(id, BOOL, Model.getCounter(m0), m0)
            val t = typeOf(expression, m0)
        in
            if t = BOOL then m1 else tError msg
        end   
        
  | typeCheck( itree(inode("assignment",_), [ idTree, itree(inode("=",_), []), expression ] ), m ) =
        let
            val msg = "Type failed to match during assignment!" 
            val id = getLeaf(idTree)
            val t1 = typeOf(expression, m)
            val t2 = getType(accessEnv(id, m))
        in
            if t1 = t2 then m else tError msg
        end
        
  | typeCheck( itree(inode("prefix",_), [ operator, idTree ] ), m ) =
        let
            val msg = "Type failed to match during prefix!" 
            val id = getLeaf(idTree)
            val t1 = getType(accessEnv(id, m))
        in
            if t1 = INT then m else tError msg
        end   
  
  | typeCheck( itree(inode("postfix",_), [ idTree, operator] ), m ) =
        let
            val msg = "Type failed to match during postfix!" 
            val id = getLeaf(idTree)
            val t1 = getType(accessEnv(id, m))
        in
            if t1 = INT then m else tError msg
        end   
  
  | typeCheck( itree(inode("out",_), [ itree(inode("print",_), []), itree(inode("(",_), []), expression , itree(inode(")",_), [])] ), m ) =
        let
            val msg = "Type failed to match during print!" 
            val t1 = typeOf(expression, m)
        in
            if t1 <> ERROR then m else tError msg
        end
  
  | typeCheck( itree(inode("conditional",_), [ child ] ), m ) = 
        typeCheck(child, m) (* ifThen, ifThenElse *)
  
  | typeCheck( itree(inode("iterative",_), [ child ] ), m ) = 
        typeCheck(child, m) (* whileLoop, forLoop *)
  
  | typeCheck( itree(inode("block",_), [ itree(inode("{",_), []), statementList, itree(inode("}",_), []) ] ), m0 ) = 
        let
            val m1 = typeCheck(statementList, m0)
        in
            m0
        end
  
  | typeCheck( itree(inode("ifThen",_), [ itree(inode("if",_), []), itree(inode("(",_), []), expression, itree(inode(")",_), []), itree(inode("then",_), []), block ] ), m0) = 
        let
            val msg = "Type failed to match during ifThen!" 
            val t = typeOf(expression, m0)
            val m1 = typeCheck(block, m0)
        in
            if t = BOOL then m0 else tError msg
        end
  
  | typeCheck( itree(inode("ifThenElse",_), [ itree(inode("if",_), []), itree(inode("(",_), []), expression, itree(inode(")",_), []), itree(inode("then",_), []), block1, itree(inode("else",_), []), block2 ] ), m0) = 
        let
            val msg = "Type failed to match during ifThenElse!" 
            val t = typeOf(expression, m0)
            val m1 = typeCheck(block1, m0)
            val m2 = typeCheck(block2, m0)
        in
            if t = BOOL then m0 else tError msg
        end
        
  | typeCheck( pTree as itree(inode("whileLoop",_), [ itree(inode("while",_), []), itree(inode("(",_), []), expression, itree(inode(")",_), []), block ] ), m0) = 
        let
            val msg = "Type failed to match during whileLoop!" 
            val t = typeOf(expression, m0)
            val m1 = typeCheck(block, m0)
        in
            if t = BOOL then m0 else tError msg
        end
  
  | typeCheck( itree(inode("forLoop",_), [ itree(inode("for",_), []), itree(inode("(",_), []), forInitial, itree(inode(";",_), []), expression, itree(inode(";",_), []), forUpdate, itree(inode(")",_), []), block ] ), m0) = 
        let
            val msg = "Type failed to match during forLoop!" 
            val m1 = typeCheck(forInitial, m0)
            val t = typeOf(expression, m1)
            val m2 = typeCheck(forUpdate, m1)
            val m3 = typeCheck(block, m1)
        in
            if t = BOOL then m0 else tError msg
        end
        
  | typeCheck( itree(inode("forInitial",_), [ itree(inode("int",_), []), idTree, itree(inode("=",_), []), expression ]), m0) =
        let
            val msg = "Type failed to match during forInitial!" 
            val id = getLeaf(idTree)
            val m1 = updateEnv(id, INT, Model.getCounter(m0), m0)
            val t = typeOf(expression, m0)
        in
            if t = INT then m1 else tError msg
        end
        
  | typeCheck( itree(inode("forUpdate",_), [ child ] ), m ) = 
        typeCheck(child, m) (* prefix & postfix *)
        
  | typeCheck( itree(inode(x_root,_), children),_) = raise General.Fail("\n\nIn typeCheck root = " ^ x_root ^ "\n\n")
  
  | typeCheck _ = raise Fail("Error in typeChecker.typeCheck - this should never occur");

(* =========================================================================================================== *)  
end (* struct *)
(* =========================================================================================================== *)








