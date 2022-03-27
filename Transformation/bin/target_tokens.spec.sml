(*#line 31.10 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec"*)functor Target_LexFn(val getNextTokenPos : string -> {line: word, column: word})(*#line 1.1 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec.sml"*)
=
   struct
    structure UserDeclarations =
      struct
(*#line 1.1 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec"*)(* ============================================================================================== *) 
datatype lexresult	= SHELL of string * string * {line: word, column: word};
val error 			= fn x => TextIO.output(TextIO.stdOut,x ^ "\n")
val eof 			= fn () => SHELL("","eof",getNextTokenPos(""))
(* ============================================================================================== *)
(* ------------------------------------------------------------------ *)
(* assumes that ">" does not occur as part of a nonterminal symbol *)
fun generateSchemaTokenName( yytext ) =
    let
		fun split(x, []   ) =  raise General.Fail("an_error")
		  | split(x, y::ys) = if x=y then ys else split(x,ys);
													
		fun splitFirst(symbol,[])    = 	[] (* symbol was not in the input list *)
		  | splitFirst(symbol,x::xs) = 	if x = symbol 
						then (* found split point *)
							[]
						else (* keep looking      *)
							x::splitFirst(symbol,xs);
																		
        val s0   = explode(yytext);
        val s1   = split(#"<",s0);
        val s2   = splitFirst(#">",s1);  
    in
        implode(explode("!#schema_variable_") @ s2)        
    end;
	
(* ------------------------------------------------------------------ *)

(* ============================================================================================== *)
(*#line 35.1 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec.sml"*)
end (* end of user routines *)
exception LexError (* raised if illegal leaf action tried *)
structure Internal =
	struct

datatype yyfinstate = N of int
type statedata = {fin : yyfinstate list, trans: string}
(* transition & final state table *)
val tab = let
val s = [ 
 (0, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (1, 
"\003\003\003\003\003\003\003\003\003\069\070\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\069\067\003\003\003\066\003\003\065\064\063\061\003\059\003\058\
\\057\055\055\055\055\055\055\055\055\055\003\054\053\051\050\003\
\\003\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\
\\007\007\007\007\007\007\007\007\007\007\007\003\003\003\049\003\
\\003\046\042\007\007\038\032\007\007\028\007\007\007\007\007\026\
\\021\007\007\007\014\007\007\009\007\007\007\006\005\004\003\003\
\\003"
),
 (7, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000"
),
 (9, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\010\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000"
),
 (10, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\011\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000"
),
 (11, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\012\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000"
),
 (12, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000\008\008\008\008\013\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000"
),
 (14, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\018\008\008\008\008\008\008\008\
\\008\008\015\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000"
),
 (15, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\016\008\008\008\008\008\000\000\000\000\000\
\\000"
),
 (16, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000\008\008\008\008\017\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000"
),
 (18, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000\008\008\008\008\019\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000"
),
 (19, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\020\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000"
),
 (21, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\022\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000"
),
 (22, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\023\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000"
),
 (23, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\024\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000"
),
 (24, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\025\008\008\008\008\008\008\000\000\000\000\000\
\\000"
),
 (26, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\027\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000"
),
 (28, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000\008\008\008\008\008\031\008\008\008\008\008\008\008\029\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000"
),
 (29, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\030\008\008\008\008\008\008\000\000\000\000\000\
\\000"
),
 (32, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000\035\008\008\008\008\008\008\008\008\008\008\008\008\008\033\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000"
),
 (33, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\034\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000"
),
 (35, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\036\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000"
),
 (36, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\037\008\008\008\008\008\008\008\000\000\000\000\000\
\\000"
),
 (38, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\039\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000"
),
 (39, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\040\008\008\008\008\008\008\008\000\000\000\000\000\
\\000"
),
 (40, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000\008\008\008\008\041\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000"
),
 (42, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\043\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000"
),
 (43, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\044\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000"
),
 (44, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\045\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000"
),
 (46, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\047\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000"
),
 (47, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000\008\008\008\048\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000"
),
 (51, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\052\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (55, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\056\056\056\056\056\056\056\056\056\056\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (59, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\060\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (61, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\062\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (67, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\068\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (69, 
"\000\000\000\000\000\000\000\000\000\070\070\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\070\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
(0, "")]
fun f x = x 
val s = map f (rev (tl (rev s))) 
exception LexHackingError 
fun look ((j,x)::r, i: int) = if i = j then x else look(r, i) 
  | look ([], i) = raise LexHackingError
fun g {fin=x, trans=i} = {fin=x, trans=look(s,i)} 
in Vector.fromList(map g 
[{fin = [], trans = 0},
{fin = [], trans = 1},
{fin = [], trans = 1},
{fin = [(N 110)], trans = 0},
{fin = [(N 88),(N 110)], trans = 0},
{fin = [(N 80),(N 110)], trans = 0},
{fin = [(N 86),(N 110)], trans = 0},
{fin = [(N 105),(N 110)], trans = 7},
{fin = [(N 105)], trans = 7},
{fin = [(N 105),(N 110)], trans = 9},
{fin = [(N 105)], trans = 10},
{fin = [(N 105)], trans = 11},
{fin = [(N 105)], trans = 12},
{fin = [(N 26),(N 105)], trans = 7},
{fin = [(N 105),(N 110)], trans = 14},
{fin = [(N 105)], trans = 15},
{fin = [(N 105)], trans = 16},
{fin = [(N 102),(N 105)], trans = 7},
{fin = [(N 105)], trans = 18},
{fin = [(N 105)], trans = 19},
{fin = [(N 9),(N 105)], trans = 7},
{fin = [(N 105),(N 110)], trans = 21},
{fin = [(N 105)], trans = 22},
{fin = [(N 105)], trans = 23},
{fin = [(N 105)], trans = 24},
{fin = [(N 20),(N 105)], trans = 7},
{fin = [(N 105),(N 110)], trans = 26},
{fin = [(N 42),(N 105)], trans = 7},
{fin = [(N 105),(N 110)], trans = 28},
{fin = [(N 105)], trans = 29},
{fin = [(N 34),(N 105)], trans = 7},
{fin = [(N 4),(N 105)], trans = 7},
{fin = [(N 105),(N 110)], trans = 32},
{fin = [(N 105)], trans = 33},
{fin = [(N 30),(N 105)], trans = 7},
{fin = [(N 105)], trans = 35},
{fin = [(N 105)], trans = 36},
{fin = [(N 105)], trans = 16},
{fin = [(N 105),(N 110)], trans = 38},
{fin = [(N 105)], trans = 39},
{fin = [(N 105)], trans = 40},
{fin = [(N 14),(N 105)], trans = 7},
{fin = [(N 105),(N 110)], trans = 42},
{fin = [(N 105)], trans = 43},
{fin = [(N 105)], trans = 44},
{fin = [(N 39),(N 105)], trans = 7},
{fin = [(N 105),(N 110)], trans = 46},
{fin = [(N 105)], trans = 47},
{fin = [(N 46),(N 105)], trans = 7},
{fin = [(N 84),(N 110)], trans = 0},
{fin = [(N 72),(N 110)], trans = 0},
{fin = [(N 62),(N 110)], trans = 51},
{fin = [(N 65)], trans = 0},
{fin = [(N 70),(N 110)], trans = 0},
{fin = [(N 1),(N 110)], trans = 0},
{fin = [(N 92),(N 110)], trans = 55},
{fin = [(N 92)], trans = 55},
{fin = [(N 92),(N 110)], trans = 0},
{fin = [(N 58),(N 110)], trans = 0},
{fin = [(N 54),(N 110)], trans = 59},
{fin = [(N 78)], trans = 0},
{fin = [(N 52),(N 110)], trans = 61},
{fin = [(N 75)], trans = 0},
{fin = [(N 56),(N 110)], trans = 0},
{fin = [(N 50),(N 110)], trans = 0},
{fin = [(N 48),(N 110)], trans = 0},
{fin = [(N 60),(N 110)], trans = 0},
{fin = [(N 82),(N 110)], trans = 67},
{fin = [(N 68)], trans = 0},
{fin = [(N 108),(N 110)], trans = 69},
{fin = [(N 108)], trans = 69}])
end
structure StartStates =
	struct
	datatype yystartstate = STARTSTATE of int

(* start state definitions *)

val INITIAL = STARTSTATE 1;

end
type result = UserDeclarations.lexresult
	exception LexerError (* raised if illegal leaf action tried *)
end

structure YYPosInt : INTEGER = Int
fun makeLexer yyinput =
let	val yygone0= YYPosInt.fromInt ~1
	val yyb = ref "\n" 		(* buffer *)
	val yybl = ref 1		(*buffer length *)
	val yybufpos = ref 1		(* location of next character to use *)
	val yygone = ref yygone0	(* position in file of beginning of buffer *)
	val yydone = ref false		(* eof found yet? *)
	val yybegin = ref 1		(*Current 'start state' for lexer *)

	val YYBEGIN = fn (Internal.StartStates.STARTSTATE x) =>
		 yybegin := x

fun lex () : Internal.result =
let fun continue() = lex() in
  let fun scan (s,AcceptingLeaves : Internal.yyfinstate list list,l,i0) =
	let fun action (i,nil) = raise LexError
	| action (i,nil::l) = action (i-1,l)
	| action (i,(node::acts)::l) =
		case node of
		    Internal.N yyk => 
			(let fun yymktext() = substring(!yyb,i0,i-i0)
			     val yypos = YYPosInt.+(YYPosInt.fromInt i0, !yygone)
			open UserDeclarations Internal.StartStates
 in (yybufpos := i; case yyk of 

			(* Application actions *)

  1 => let val yytext=yymktext() in (*#line 46.34 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec"*) SHELL(yytext      , yytext,     getNextTokenPos(yytext))    (*#line 574.1 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec.sml"*)
 end
| 102 => let val yytext=yymktext() in (*#line 78.34 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec"*) SHELL("boolean"   , yytext,     getNextTokenPos(yytext))    (*#line 576.1 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec.sml"*)
 end
| 105 => let val yytext=yymktext() in (*#line 79.34 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec"*) SHELL("id"        , yytext,     getNextTokenPos(yytext))    (*#line 578.1 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec.sml"*)
 end
| 108 => let val yytext=yymktext() in (*#line 81.34 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec"*) getNextTokenPos(yytext); lex()  (*#line 580.1 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec.sml"*)
 end
| 110 => let val yytext=yymktext() in (*#line 83.34 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec"*) error("ignored an unprintable character: " ^ yytext); getNextTokenPos(yytext); lex()  (*#line 582.1 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec.sml"*)
 end
| 14 => let val yytext=yymktext() in (*#line 49.34 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec"*) SHELL(yytext      , yytext,     getNextTokenPos(yytext))    (*#line 584.1 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec.sml"*)
 end
| 20 => let val yytext=yymktext() in (*#line 50.34 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec"*) SHELL(yytext      , yytext,     getNextTokenPos(yytext))    (*#line 586.1 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec.sml"*)
 end
| 26 => let val yytext=yymktext() in (*#line 51.34 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec"*) SHELL(yytext      , yytext,     getNextTokenPos(yytext))    (*#line 588.1 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec.sml"*)
 end
| 30 => let val yytext=yymktext() in (*#line 52.34 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec"*) SHELL(yytext      , yytext,     getNextTokenPos(yytext))    (*#line 590.1 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec.sml"*)
 end
| 34 => let val yytext=yymktext() in (*#line 53.34 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec"*) SHELL(yytext      , yytext,     getNextTokenPos(yytext))    (*#line 592.1 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec.sml"*)
 end
| 39 => let val yytext=yymktext() in (*#line 54.34 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec"*) SHELL(yytext      , yytext,     getNextTokenPos(yytext))    (*#line 594.1 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec.sml"*)
 end
| 4 => let val yytext=yymktext() in (*#line 47.34 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec"*) SHELL(yytext      , yytext,     getNextTokenPos(yytext))    (*#line 596.1 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec.sml"*)
 end
| 42 => let val yytext=yymktext() in (*#line 55.34 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec"*) SHELL(yytext      , yytext,     getNextTokenPos(yytext))    (*#line 598.1 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec.sml"*)
 end
| 46 => let val yytext=yymktext() in (*#line 56.34 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec"*) SHELL(yytext      , yytext,     getNextTokenPos(yytext))    (*#line 600.1 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec.sml"*)
 end
| 48 => let val yytext=yymktext() in (*#line 57.34 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec"*) SHELL(yytext      , yytext,     getNextTokenPos(yytext))    (*#line 602.1 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec.sml"*)
 end
| 50 => let val yytext=yymktext() in (*#line 58.34 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec"*) SHELL(yytext      , yytext,     getNextTokenPos(yytext))    (*#line 604.1 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec.sml"*)
 end
| 52 => let val yytext=yymktext() in (*#line 59.34 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec"*) SHELL(yytext      , yytext,     getNextTokenPos(yytext))    (*#line 606.1 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec.sml"*)
 end
| 54 => let val yytext=yymktext() in (*#line 60.34 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec"*) SHELL(yytext      , yytext,     getNextTokenPos(yytext))    (*#line 608.1 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec.sml"*)
 end
| 56 => let val yytext=yymktext() in (*#line 61.34 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec"*) SHELL(yytext      , yytext,     getNextTokenPos(yytext))    (*#line 610.1 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec.sml"*)
 end
| 58 => let val yytext=yymktext() in (*#line 62.34 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec"*) SHELL(yytext      , yytext,     getNextTokenPos(yytext))    (*#line 612.1 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec.sml"*)
 end
| 60 => let val yytext=yymktext() in (*#line 63.34 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec"*) SHELL(yytext      , yytext,     getNextTokenPos(yytext))    (*#line 614.1 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec.sml"*)
 end
| 62 => let val yytext=yymktext() in (*#line 64.34 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec"*) SHELL(yytext      , yytext,     getNextTokenPos(yytext))    (*#line 616.1 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec.sml"*)
 end
| 65 => let val yytext=yymktext() in (*#line 65.34 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec"*) SHELL(yytext      , yytext,     getNextTokenPos(yytext))    (*#line 618.1 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec.sml"*)
 end
| 68 => let val yytext=yymktext() in (*#line 66.34 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec"*) SHELL(yytext      , yytext,     getNextTokenPos(yytext))    (*#line 620.1 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec.sml"*)
 end
| 70 => let val yytext=yymktext() in (*#line 67.34 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec"*) SHELL(yytext      , yytext,     getNextTokenPos(yytext))    (*#line 622.1 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec.sml"*)
 end
| 72 => let val yytext=yymktext() in (*#line 68.34 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec"*) SHELL(yytext      , yytext,     getNextTokenPos(yytext))    (*#line 624.1 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec.sml"*)
 end
| 75 => let val yytext=yymktext() in (*#line 69.34 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec"*) SHELL(yytext      , yytext,     getNextTokenPos(yytext))    (*#line 626.1 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec.sml"*)
 end
| 78 => let val yytext=yymktext() in (*#line 70.34 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec"*) SHELL(yytext      , yytext,     getNextTokenPos(yytext))    (*#line 628.1 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec.sml"*)
 end
| 80 => let val yytext=yymktext() in (*#line 71.34 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec"*) SHELL(yytext      , yytext,     getNextTokenPos(yytext))    (*#line 630.1 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec.sml"*)
 end
| 82 => let val yytext=yymktext() in (*#line 72.34 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec"*) SHELL(yytext      , yytext,     getNextTokenPos(yytext))    (*#line 632.1 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec.sml"*)
 end
| 84 => let val yytext=yymktext() in (*#line 73.34 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec"*) SHELL(yytext      , yytext,     getNextTokenPos(yytext))    (*#line 634.1 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec.sml"*)
 end
| 86 => let val yytext=yymktext() in (*#line 74.34 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec"*) SHELL(yytext      , yytext,     getNextTokenPos(yytext))    (*#line 636.1 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec.sml"*)
 end
| 88 => let val yytext=yymktext() in (*#line 75.34 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec"*) SHELL(yytext      , yytext,     getNextTokenPos(yytext))    (*#line 638.1 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec.sml"*)
 end
| 9 => let val yytext=yymktext() in (*#line 48.34 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec"*) SHELL(yytext      , yytext,     getNextTokenPos(yytext))    (*#line 640.1 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec.sml"*)
 end
| 92 => let val yytext=yymktext() in (*#line 77.34 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec"*) SHELL("integer"   , yytext,     getNextTokenPos(yytext))    (*#line 642.1 "C:\CSCI4220\Assignments\Project\Group1_M4_Domain\Transformation\bin\target_tokens.spec.sml"*)
 end
| _ => raise Internal.LexerError

		) end )

	val {fin,trans} = Vector.sub(Internal.tab, s)
	val NewAcceptingLeaves = fin::AcceptingLeaves
	in if l = !yybl then
	     if trans = #trans(Vector.sub(Internal.tab,0))
	       then action(l,NewAcceptingLeaves
) else	    let val newchars= if !yydone then "" else yyinput 1024
	    in if (size newchars)=0
		  then (yydone := true;
		        if (l=i0) then UserDeclarations.eof ()
		                  else action(l,NewAcceptingLeaves))
		  else (if i0=l then yyb := newchars
		     else yyb := substring(!yyb,i0,l-i0)^newchars;
		     yygone := YYPosInt.+(!yygone, YYPosInt.fromInt i0);
		     yybl := size (!yyb);
		     scan (s,AcceptingLeaves,l-i0,0))
	    end
	  else let val NewChar = Char.ord(CharVector.sub(!yyb,l))
		val NewChar = if NewChar<128 then NewChar else 128
		val NewState = Char.ord(CharVector.sub(trans,NewChar))
		in if NewState=0 then action(l,NewAcceptingLeaves)
		else scan(NewState,NewAcceptingLeaves,l+1,i0)
	end
	end
(*
	val start= if substring(!yyb,!yybufpos-1,1)="\n"
then !yybegin+1 else !yybegin
*)
	in scan(!yybegin (* start *),nil,!yybufpos,!yybufpos)
    end
end
  in lex
  end
end
