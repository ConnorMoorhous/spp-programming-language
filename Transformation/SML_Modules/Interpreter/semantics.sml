(* =========================================================================================================== *)
structure Semantics =
struct


(* This makes contents of the Model structure directly accessible (i.e., the prefix "Model." is not needed. *)            
open Model; 
            
(* This makes the internal representation of parse trees directly accessible. *)            
open CONCRETE_REPRESENTATION;

(***    E'   ***)

fun E'( itree(inode("expression",_), [ logicalOr ] ), m ) =
        E'( logicalOr, m )
 
  | E'( itree(inode("logicalOr",_), [ logicalOr, itree(inode("or",_), []), logicalAnd ] ), m0 ) =
    let 
        val (v1, m1) = E'(logicalOr, m0)
    in
        if toBool(v1) then (v1, m1)
        else E'(logicalAnd, m1)

    end
  
  | E'( itree(inode("logicalOr",_), [ logicalAnd ] ), m ) =
        E'( logicalAnd, m )

  | E'( itree(inode("logicalAnd",_), [ logicalAnd, itree(inode("and",_), []), equality ] ), m0 ) =
    let 
        val (v1, m1) = E'(logicalAnd, m0)
    in
        if toBool(v1) then E'(equality, m1)
        else (v1, m1)
    end

  | E'( itree(inode("logicalAnd",_), [ equality ] ), m ) =
        E'( equality, m )

  | E'( itree(inode("equality",_), [ equality, itree(inode("equalityOperator",_), [ itree(inode("!=",_), []) ]), relation ] ), m0 ) =
    let
        val (v1, m1) = E'(equality, m0)
        val (v2, m2) = E'(relation, m1)
    in
        (Boolean (v1 <> v2), m2)
    end
   
  | E'( itree(inode("equality",_), [ equality, itree(inode("equalityOperator",_), [ itree(inode("==",_), []) ]), relation ] ), m0 ) =
    let
        val (v1, m1) = E'(equality, m0)
        val (v2, m2) = E'(relation, m1)
    in
        (Boolean (v1 = v2), m2)
    end
  
  | E'( itree(inode("equality",_), [ relation ] ), m ) =
        E'( relation, m )

  | E'( itree(inode("relation",_), [ relation, itree(inode("relationOperator",_), [ itree(inode("<",_), []) ]), additive ] ), m0 ) =
    let
        val (v1, m1) = E'(relation, m0)
        val (v2, m2) = E'(additive, m1)
    in
        (Boolean (toInt(v1) < toInt(v2)), m2)
    end
   
  | E'( itree(inode("relation",_), [ relation, itree(inode("relationOperator",_), [ itree(inode(">",_), []) ]), additive ] ), m0 ) =
    let
        val (v1, m1) = E'(relation, m0)
        val (v2, m2) = E'(additive, m1)
    in
        (Boolean (toInt(v1) > toInt(v2)), m2)
    end
    
  | E'( itree(inode("relation",_), [ additive ] ), m ) =
        E'( additive, m )

  | E'( itree(inode("additive",_), [ additive, itree(inode("additiveOperator",_), [ itree(inode("+",_), []) ]), multiplicative ] ), m0 ) =
    let
        val (v1, m1) = E'(additive, m0)
        val (v2, m2) = E'(multiplicative, m1)
    in
        (Integer (toInt(v1) + toInt(v2)), m2)
    end

  | E'( itree(inode("additive",_), [ additive, itree(inode("additiveOperator",_), [ itree(inode("-",_), []) ]), multiplicative ] ), m0 ) =
    let
        val (v1, m1) = E'(additive, m0)
        val (v2, m2) = E'(multiplicative, m1)
    in
        (Integer (toInt(v1) - toInt(v2)), m2)
    end

  | E'( itree(inode("additive",_), [ multiplicative ] ), m ) =
        E'( multiplicative, m )

  | E'( itree(inode("multiplicative",_), [ multiplicative, itree(inode("multiplicativeOperator",_), [ itree(inode("*",_), []) ]), unary ] ), m0 ) =
    let
        val (v1, m1) = E'(multiplicative, m0)
        val (v2, m2) = E'(unary, m1)
    in
        (Integer (toInt(v1) * toInt(v2)), m2)
    end

  | E'( itree(inode("multiplicative",_), [ multiplicative, itree(inode("multiplicativeOperator",_), [ itree(inode("/",_), []) ]), unary ] ), m0 ) =
    let
        val (v1, m1) = E'(multiplicative, m0)
        val (v2, m2) = E'(unary, m1)
    in
        (Integer (toInt(v1) div toInt(v2)), m2)
    end

  | E'( itree(inode("multiplicative",_), [ multiplicative, itree(inode("multiplicativeOperator",_), [ itree(inode("%",_), []) ]), unary ] ), m0 ) =
    let
        val (v1, m1) = E'(multiplicative, m0)
        val (v2, m2) = E'(unary, m1)
    in
        (Integer (toInt(v1) mod toInt(v2)), m2)
    end

  | E'( itree(inode("multiplicative",_), [ unary ] ), m ) =
        E'( unary, m )

  | E'( itree(inode("unary",_), [ itree(inode("-",_), []), unary ] ), m0 ) =
    let
        val (v0, m1) = E'(unary, m0)
        val v1 = toInt(v0)
    in
        (Integer (~v1), m1)
    end

  | E'( itree(inode("unary",_), [ itree(inode("!",_), []), unary ] ), m0 ) =
    let
        val (v0, m1) = E'(unary, m0)
        val v1 = toBool(v0)
    in
        (Boolean (not v1), m1)
    end

  | E'( itree(inode("unary",_), [ exponent ] ), m ) =
        E'( exponent, m )

  | E'( itree(inode("exponent",_), [ factor, itree(inode("^",_), []), exponent ] ), m0 ) =
    let
        fun exp(b, 0) = 1
            | exp(b, e) = b*exp(b, e-1)
        val (v1, m1) = E'(factor, m0)
        val (v2, m2) = E'(exponent, m1)
        val v3 = exp(toInt(v1), toInt(v2))
    in
        (Integer v3, m1)
    end

  | E'( itree(inode("exponent",_), [ factor ] ), m ) =
        E'( factor, m )

  | E'( itree(inode("factor",_), [ itree(inode("(",_), []), expression, itree(inode(")",_), []) ] ), m ) =
        E'( expression, m )

  | E'( itree(inode("factor",_), [ itree(inode("|",_), []), expression, itree(inode("|",_), []) ] ), m0 ) =
    let
        val (v0, m1) = E'(expression, m0)
        val v1 = toInt(v0)
    in
        if v1 < 0 then (Integer (~v1), m1)
        else (v0, m1)
    end
        
  | E'( itree(inode("factor",_), [ factorChild ] ), m ) =
        E'( factorChild, m ) (* prefix, postfix,  *)

  | E'( itree(inode("prefix",_), [ itree(inode("incrementDecrementOperator",_), [ itree(inode("++",_), []) ]), idTree ] ), m0 ) =
    let
        val id = getLeaf(idTree)
        val loc = getLoc(accessEnv(id, m0))
        val v = Integer (toInt(accessStore(loc, m0)) + 1)
        val m1 = updateStore(loc, v, m0)
    in
        (v, m1)
    end

  | E'( itree(inode("prefix",_), [ itree(inode("incrementDecrementOperator",_), [ itree(inode("--",_), []) ]), idTree ] ), m0 ) =
    let
        val id = getLeaf(idTree)
        val loc = getLoc(accessEnv(id, m0))
        val v = Integer (toInt(accessStore(loc, m0)) - 1)
        val m1 = updateStore(loc, v, m0)
    in
        (v, m1)
    end

  | E'( itree(inode("postfix",_), [ idTree, itree(inode("incrementDecrementOperator",_), [ itree(inode("++",_), []) ]) ] ), m0 ) =
    let
        val id = getLeaf(idTree)
        val loc = getLoc(accessEnv(id, m0))
        val v = accessStore(loc, m0)
        val m1 = updateStore(loc, Integer (toInt(v) + 1), m0)
    in
        (v, m1)
    end

  | E'( itree(inode("postfix",_), [ idTree, itree(inode("incrementDecrementOperator",_), [ itree(inode("--",_), []) ]) ] ), m0 ) =
    let
        val id = getLeaf(idTree)
        val loc = getLoc(accessEnv(id, m0))
        val v = accessStore(loc, m0)
        val m1 = updateStore(loc, Integer (toInt(v) - 1), m0)
    in
        (v, m1)
    end
    
  | E'( intTree as itree(inode("integer",_), [ _ ] ), m ) =
        let
            val intString = getLeaf(intTree)
            val v = valOf(Int.fromString intString)
        in
            (Integer v, m)
        end

  | E'( boolTree as itree(inode("boolean",_), [ _ ] ), m ) =
        let
            val boolString = getLeaf(boolTree)
            val v = valOf(Bool.fromString boolString)
        in
            (Boolean v, m)
        end
  
  | E'( idTree as itree(inode("id",_), [ _ ] ), m ) = 
    let
        val id = getLeaf(idTree)
        val loc = getLoc(accessEnv(id, m))
        val v = accessStore(loc, m)
    in
        (v, m)
    end
    
 | E' _ = raise Fail("error in Semantics.E' - this should never occur");
    
(***    M   ***)

fun M( itree(inode("statementList",_), [ statement, statementList ] ), m ) = 
        M( statementList, M(statement, m))
        
  | M( itree(inode("statementList",_), [ epsilon ] ), m ) = m
  
  | M( itree(inode("statement",_), [ singleStatement, itree(inode(";",_), []) ] ), m ) = 
        M(singleStatement, m)
        
  | M( itree(inode("statement",_), [ blockStatement ] ), m ) = 
        M(blockStatement, m)
        
  | M( itree(inode("singleStatement",_), [ itree(inode("skip",_), [ epsilon ] ) ] ), m ) = m
  
  | M( itree(inode("singleStatement",_), [ child ] ), m ) = 
        M(child, m)   (* declaration, assignment, out, prefix, postfix *)
        
  | M( itree(inode("blockStatement",_), [ child ] ), m ) = 
        M(child, m)   (* conditional, iterative, block *) 
        
  | M( itree(inode("declaration",_), [ itree(inode("type",_), [itree(inode("int",_), [])]), idTree ] ), m0 ) =
        let
            val id = getLeaf(idTree)
            val m1 = updateEnv(id, INT, Model.getCounter(m0), m0)
        in
            m1
        end
        
  | M( itree(inode("declaration",_), [ itree(inode("type",_), [itree(inode("bool",_), [])]), idTree ] ), m0 ) =
        let
            val id = getLeaf(idTree)
            val m1 = updateEnv(id, BOOL, Model.getCounter(m0), m0)
        in
            m1
        end
        
  | M( itree(inode("declaration",_), [ itree(inode("type",_), [itree(inode("int",_), [])]), idTree, itree(inode("=",_), []), expression ] ), m0 ) =
        let
            val id = getLeaf(idTree)
            val m1 = updateEnv(id, INT, Model.getCounter(m0), m0)
            val (v, m2) = E'(expression, m1)
            val loc = getLoc(accessEnv(id, m2))
            val m3 = updateStore(loc, v, m2)
        in
            m3
        end
        
  | M( itree(inode("declaration",_), [ itree(inode("type",_), [itree(inode("bool",_), [])]), idTree, itree(inode("=",_), []), expression ] ), m0 ) =
        let
            val id = getLeaf(idTree)
            val m1 = updateEnv(id, BOOL, Model.getCounter(m0), m0)
            val (v, m2) = E'(expression, m1)
            val loc = getLoc(accessEnv(id, m2))
            val m3 = updateStore(loc, v, m2)
        in
            m3
        end   
        
  | M( itree(inode("assignment",_), [ idTree, itree(inode("=",_), []), expression ] ), m0 ) =
        let
            val id = getLeaf(idTree)
            val (v, m1) = E'(expression, m0)
            val loc = getLoc(accessEnv(id, m1))
            val m2 = updateStore(loc, v, m1)
        in
            m2
        end
        
  | M( pTree as itree(inode("prefix",_), [ operator, id ] ), m0 ) =
        let
            val (v, m1) = E'( pTree, m0 )
        in
            m1
        end   
  
  | M( pTree as itree(inode("postfix",_), [ id, operator] ), m0 ) =
        let
            val (v, m1) = E'( pTree, m0 )
        in
            m1
        end
  
  | M( itree(inode("out",_), [ itree(inode("print",_), []), itree(inode("(",_), []), expression , itree(inode(")",_), [])] ), m0 ) =
        let
            val (v, m1) = E'(expression, m0)
        in
            print(toString(v) ^ "\n");
            m1
        end
  
  | M( itree(inode("conditional",_), [ child ] ), m ) = 
        M(child, m) (* ifThen, ifThenElse *)
  
  | M( itree(inode("iterative",_), [ child ] ), m ) = 
        M(child, m) (* whileLoop, forLoop *)
  
  | M( itree(inode("block",_), [ itree(inode("{",_), []), statementList, itree(inode("}",_), []) ] ), (env0, loc0, s0) ) = 
        let
            val (env1, loc1, s1) = M(statementList, (env0, loc0, s0))
            val m2 = (env0, loc0, s1)
        in
            m2
        end
  
  | M( itree(inode("ifThen",_), [ itree(inode("if",_), []), itree(inode("(",_), []), expression, itree(inode(")",_), []), itree(inode("then",_), []), block ] ), m0) = 
        let
            val (v, m1) = E'(expression, m0) 
        in
            if toBool(v) then M(block, m1) 
            else m1
        end
  
  | M( itree(inode("ifThenElse",_), [ itree(inode("if",_), []), itree(inode("(",_), []), expression, itree(inode(")",_), []), itree(inode("then",_), []), block1, itree(inode("else",_), []), block2 ] ), m0) = 
        let
            val (v, m1) = E'(expression, m0) 
        in
            if toBool(v) then M(block1, m1) 
            else M(block2, m1)
        end
        
  | M( pTree as itree(inode("whileLoop",_), [ itree(inode("while",_), []), itree(inode("(",_), []), expression, itree(inode(")",_), []), block ] ), m0) = 
        let
            val (v, m1) = E'(expression, m0) 
        in
            if toBool(v) then M(pTree, M(block, m1)) 
            else m1
        end
  
  | M( itree(inode("forLoop",_), [ itree(inode("for",_), []), itree(inode("(",_), []), forInitial, itree(inode(";",_), []), expression, itree(inode(";",_), []), forUpdate, itree(inode(")",_), []), block ] ), (env0, loc0, s0)) = 
        let
            fun N(expression, block, forUpdate, m0) =
                let
                    val (v, m1) = E'(expression, m0) 
                in
                    if toBool(v) then N(expression, block, forUpdate, M(forUpdate, M(block, m1)))
                    else m1
                end
             val (env1, loc1, s1) = M(forInitial, (env0, loc0, s0))
             val (env2, loc2, s2) = N(expression, block, forUpdate, (env1, loc1, s1))
             val m3 = (env0, loc0, s2)
        in
            m3
        end
        
  | M( itree(inode("forInitial",_), [ itree(inode("int",_), []), idTree, itree(inode("=",_), []), expression ]), m0) =
        let
            val id = getLeaf(idTree)
            val m1 = updateEnv(id, INT, Model.getCounter(m0), m0)
            val (v, m2) = E'(expression, m1)
            val loc = getLoc(accessEnv(id, m2))
            val m3 = updateStore(loc, v, m2)
        in
            m3
        end
  
  | M( itree(inode("forUpdate",_), [ child ] ), m ) = 
        M(child, m) (* prefix & postfix *)
  
  | M(  pTree as itree(inode(x_root,_), children),_) = raise General.Fail("\n\nIn M root = " ^ x_root ^ "\n\n")
  
  | M _ = raise Fail("error in Semantics.M - this should never occur");
    
(* =========================================================================================================== *)
end (* struct *)
(* =========================================================================================================== *)