open Cil
open Feature
module E = Errormsg

let currentFile : file ref = ref {fileName="dummy"
                                ; globals=[]
                                ; globinit=None
                                ; globinitcalled=false}
let currentGlobal : global ref = ref (GText "dummy")
let currentFunc : fundec ref = ref (emptyFunction "@dummy")
                                   
let todo () = E.s (E.error "todo")
let error () = E.s (E.error "impossible")

let two = integer 2
let makeArray t = TArray(t, Some two, [])
let mkArraySelect vi e : lval = (Var vi, e)
let mkArraySelect_0 vi : lval = mkArraySelect vi (Index(zero, NoOffset))
let mkArraySelect_1 vi : lval = mkArraySelect vi (Index(one, NoOffset))

let dummyLocation = {line = 0; file = ""; byte = 0;}

let dummyMem : varinfo =
  let x =
    makeGlobalVar "dummy_mem" (TArray(charType, Some(integer 1024), [])) in
  x.vstorage <- Static; x

let dummyFun : varinfo =
  makeVarinfo false "dummy_f" (TFun(voidType, Some [], false ,[]))

let dummyFunDecl : fundec =
  let x = emptyFunction "dummy_f" in
  x.svar.vstorage <- Static; x
  
                       

let rec doStmt s cond =
  match s.skind with
  | Instr (x) ->
     s.skind <- Instr(List.flatten (List.map (fun e -> doInstr e cond) x))
  | Return (eop, loc) -> ()
  | Goto (sr, loc) ->  ()
  | ComputedGoto (e, loc) -> ()
  | Break (_) ->  ()
  | Continue (_) -> ()
  | If (c, t, e, loc) -> ()
  | Switch (_, _, _, _) ->  ()
  | Loop (b, loc, x, y) ->  ()
  | Block (b) ->  ()
  | TryFinally (_, _, _) ->  ()
  | TryExcept (_, _, _, _) ->  ()
             
and doIf cond t e loc =
  todo ()
      
and doInstr t cond =
  match t with
  | Set (l, (Lval(Mem(e), off) as r) , loc) ->
     let d1 = makeTempVar !currentFunc (typeOf r) in
     let s1 = makeTempVar !currentFunc
                (makeArray (typeOf(mkAddrOrStartOf (var d1)))) in
     let d2 = makeTempVar !currentFunc (typeOf r) in
     let s2 = makeTempVar !currentFunc
                (makeArray (typeOf(mkAddrOrStartOf (var d2)))) in
     [Set(mkArraySelect_0 s1, mkAddrOrStartOf (var d1), loc)
     ; Set(mkArraySelect_1 s1, mkAddrOrStartOf (Mem(e), off), loc)
     ; Set(mkArraySelect_0 s2, mkAddrOrStartOf (var d2), loc)
     ; Set(mkArraySelect_1 s2, mkAddrOrStartOf l, loc)
     ; Set( mkMem (Lval(mkArraySelect_1 s2)) NoOffset
          , Lval(mkMem (Lval(mkArraySelect_1 s1)) NoOffset)
          , loc)]
  | Set (l, e, loc) ->
     let dummy = makeTempVar !currentFunc (typeOf e) in
     let s = makeTempVar !currentFunc
               (makeArray (typeOf(mkAddrOrStartOf (var dummy)))) in
     [Set(mkArraySelect_0 s, mkAddrOrStartOf (var dummy), loc)
     ; Set(mkArraySelect_1 s, mkAddrOrStartOf l, loc)
     ; Set((mkMem (Lval(mkArraySelect s (Index(one, NoOffset)))) NoOffset), e, loc)]
  | Call (l, e, es, loc) ->
     let s = makeTempVar !currentFunc (makeArray (TPtr((typeOf e), []))) in
     let r = match l with
       | Some l ->
          let (t, _, _, _) = splitFunctionType (typeOf e) in
          let dummy = makeTempVar !currentFunc t in
          let s' = makeTempVar !currentFunc (makeArray (typeOf(mkAddrOrStartOf (var dummy)))) in
          [Set(mkArraySelect_0 s', mkAddrOrStartOf (var dummy), loc)
          ; Set(mkArraySelect_1 s', mkAddrOrStartOf l, loc)
          ; Set(mkArraySelect_1 s
              , Lval(Var(makeVarinfo false "dummy_f" (TFun(voidType
                                                         , Some []
                                                         , false
                                                         ,[]))), NoOffset)
              , loc)
          ; Set(mkArraySelect_0 s, e, loc)
          ; Call(Some (mkMem (Lval(mkArraySelect s' (Index(one, NoOffset)))) NoOffset)
               , Lval(mkArraySelect s (Index(one, NoOffset)))
               , es
               , loc)]
       | None -> 
          [Set(mkArraySelect_0 s
             , Lval(Var(makeVarinfo false "dummy_f" (TFun(voidType
                                                        , Some []
                                                        , false
                                                        ,[])))
                  , NoOffset)
             , loc)
          ; Set(mkArraySelect_1 s, e, loc)
          ; Call(None, Lval((mkArraySelect s (Index(one, NoOffset)))), es, loc)]
     in
     r
  | Asm (_, _, _, _, _, _) -> todo ()
                            
let rec scanStmts t =
  match t with
  |  [] -> ()
  | x :: rests ->
     doStmt x []; 
     scanStmts rests
     
let scanBlock t =
  scanStmts t.bstmts
  
            
  
let scanFunc f =
  currentFunc := f;
  scanBlock f.sbody
            
    
    
    
let feature = 
  { fd_name = "flatten";
    fd_enabled = false;
    fd_description = "flatten for fun!" ;
    fd_extraopt = [];
    fd_doit = (function (f : file) -> 
                         let _ = iterGlobals f (fun glob ->
                                     match glob with
                                     | GFun (fd,_) -> scanFunc fd
                                     | _ -> ()) in
                         f.globals <- GFun(dummyFunDecl, dummyLocation) :: f.globals;
                         f.globals <- GVar(dummyMem , {init=None}, dummyLocation) :: f.globals;
              );
    fd_post_check = true;
  }

let () = Feature.register feature
