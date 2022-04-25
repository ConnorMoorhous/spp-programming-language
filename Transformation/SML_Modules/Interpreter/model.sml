(*MODEL =========================================================================================================== *) 

structure Model = 

    struct  

    exception runtime_error;
    exception model_error;

    fun error msg = ( print msg; raise runtime_error );
    fun tError msg = ( print msg; raise model_error );
    
    fun getLeaf( term ) = CONCRETE.leavesToStringRaw term  

    datatype types = INT | BOOL | ERROR; 
    datatype denotable_value =  Boolean of bool | Integer of int; 
    
    fun toBool(Boolean b) = b
        | toBool _ = raise Fail("error in model.toBool");
    fun toInt(Integer x) = x
        | toInt _ = raise Fail("error in model.toInt");
    fun toString(Boolean b) = Bool.toString(b)
        | toString(Integer x) = Int.toString(x);
    
    type counter = int;
    type loc = int;
    type env   = (string * types * loc) list;
    type store = (loc * denotable_value) list;
    val initialModel = ( []:env, 0:loc, []:store );

    (* =========================================================================================================== *) 

    (* accessEnv *)
    
    fun accessEnv ( id0, (env, counter, store) ) = 
        let
            val msg = "Error: accessEnv " ^ id0 ^ " not found.";
            fun aux [] = error msg
            |   aux ((id1,t,loc)::env) = 
                    if id0 = id1 then (t,loc)
                    else aux env
        in
            aux env
        end;

    
    (* accessStore *)
    
    fun accessStore (loc0:loc, (e:env, c:counter, s:store) ) =  
        let 
            val msg = "Error: accessStore " ^ Int.toString loc0 ^ " not found>"; 
            fun aux [] = error msg 
            |   aux ((loc1:loc, dV:denotable_value)::s) =  
                if loc1 = loc0 then dV 
                else aux s; 
        in 
            aux s 
        end; 
        
        
    (*updateEnv*) 

    fun updateEnv (id0:string, t:types, loc0:loc, (e:env, c:counter, s:store)) = 
        let 
            val msgAlreadyDeclared = "Error: updateEnv " ^ id0 ^ " already declared." 
            val msgInvalidCount ="Error: invalid loc " ^ Int.toString loc0 ^ " for new variable."
             
            fun aux(id1:string, t1:types, loc1:loc, [])  = [(id1, t1, loc1)] 
            |   aux(id1, t1, loc1, (eId:string, eType:types, eLoc:loc)::env) =  
                if id1 = eId then  
                    error msgAlreadyDeclared 
                else 
                    (eId, eType, eLoc)::aux(id1, t1, loc1, env) 
        in  
            if c <> loc0 then error msgInvalidCount 
            else 
                (aux(id0, t, loc0, e), c+1, s) 
        end; 


    (*updateStore*) 
    
    fun updateStore (loc0:loc, dv0:denotable_value, (e:env, c:counter, s:store)) =  
        let 
            fun aux(loc0:loc, dv0: denotable_value, []) = [(loc0, dv0)] 
            |   aux(loc1:loc, dv1:denotable_value, (sLoc:loc, sDv:denotable_value)::s1)= 
                    if loc1 = sLoc then 
                        (loc1, dv1)::s1 
                    else 
                        (sLoc, sDv)::aux(loc1, dv1, s1) 
        in 
            (e, c, (aux(loc0, dv0, s))) 
        end;  
       
       
    (*getLoc*) 
    fun getLoc (t:types, l:loc) = l; 
    
    (*getType*) 
    fun getType (t:types, l:loc) = t; 
    
    (*getEnv*)
    fun getEnv (e:env, c:counter, s:store) = e; 
    
    (*getCounter*) 
    fun getCounter (e:env, c:counter, s:store) = c; 
    
    (*getStore*) 
    fun getStore (e:env, c:counter, s:store) = s; 
    
    (* =========================================================================================================== *) 
    
    fun typeToString BOOL = "bool"
        | typeToString INT = "int"
        | typeToString ERROR = "error";
        
    fun envEntryToString (id, t, loc) =
        "(" ^ id ^ "," ^ typeToString t ^ "," ^ Int.toString loc ^ ")";
    
    fun showEnv [] = print "\n"
        | showEnv (entry::env) = (
            print("\n" ^ envEntryToString entry);
            showEnv env
          );
    
    fun storeEntryToString (loc, v) =
        "(" ^ Int.toString loc ^ ":" ^ toString v ^ ")";
    
    fun showStore [] = print "\n"
        | showStore (entry::store) = (
            print("\n" ^ storeEntryToString entry);
            showStore store
          );
   
    fun printModel (env, counter, store) = (
            showEnv(env);
            print("\n" ^ Int.toString counter ^ "\n");
            showStore(store)
          );
    
end; (* struct *) 







