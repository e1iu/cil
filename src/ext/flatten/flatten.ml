open Cil
open Feature
module E = Errormsg
module A = Alpha
module H = Hashtbl


         
let labelAlphaTable : (string, unit A.alphaTableData ref) H.t =
  H.create 11
  
let freshLabel (base:string) =
  fst (A.newAlphaName labelAlphaTable None base ())

let currentFunc : fundec ref = ref (emptyFunction "@dummy")
                             
let todo () = E.s (E.error "todo")
let error () = E.s (E.error "impossible")

let two = integer 2
let makeArray t : typ = TArray(t, Some two, [])
let mkArraySelect vi e : lval = (Var vi, Index(e, NoOffset))

let dummyLocation = {line = 0; file = ""; byte = 0;}

let dummyMem : varinfo =
  let x =
    makeGlobalVar "dummy_mem" (TArray(charType, Some(integer 1024), [])) in
  x.vstorage <- Static; x

let dummyMem' : exp = mkAddrOrStartOf (var dummyMem)
                    

let dummyFun : varinfo =
  makeVarinfo false "dummy_f" (TFun(voidType, Some [], false ,[]))

let dummyFunDecl : fundec =
  let x = emptyFunction "dummy_f" in
  x.svar.vstorage <- Static; x


let addrOfType t : typ =
  typeOf(mkAddrOrStartOf (var (makeVarinfo false "" t)))

let addrOfExp e : typ = addrOfType (typeOf e)
                      

let mkLAnd x y =
  BinOp(LAnd, x, y, typeOf x)

  
let mkTrue e =
  BinOp(Ne, e, zero, intType)

  
let mkFalse e =
  BinOp(Eq, e, zero, intType)

  
let rec splitCondition t =
  match t with
  | BinOp(LAnd, l, r, _) -> (splitCondition l) @ (splitCondition r)
  | BinOp(Eq, Lval(Var vi, NoOffset), r, _) -> (vi, zero) :: (splitCondition r)
  | BinOp(Ne, Lval(Var vi, NoOffset), r, _) -> (vi, one) :: (splitCondition r)
  | Const (CInt64(_, _, _)) -> []
  | _ -> 
     let p = new Cil.defaultCilPrinterClass in
     print_string (Pretty.sprint 80 (p#pExp () t));
     error ()


let fltInstr t cond : stmt =
  let r = 
    match t with
    | Set (l, (Lval(Mem(e), off) as r) , loc) ->
       let s = makeTempVar !currentFunc (makeArray (addrOfExp r)) in
       let s' = makeTempVar !currentFunc (makeArray (addrOfExp r)) in
       [Set(mkArraySelect s zero, dummyMem', loc)
       ; Set(mkArraySelect s one, mkAddrOrStartOf (Mem(e), off), loc)
       ; Set(mkArraySelect s' zero, dummyMem', loc)
       ; Set(mkArraySelect s' one, mkAddrOrStartOf l, loc)
       ; Set(mkMem (Lval(mkArraySelect s' cond)) NoOffset
           , Lval(mkMem (Lval(mkArraySelect s cond)) NoOffset)
           , loc)]
    | Set (l, e, loc) ->
       let s = makeTempVar !currentFunc (makeArray (addrOfExp e)) in
       [Set(mkArraySelect s zero, dummyMem', loc)
       ; Set(mkArraySelect s one, mkAddrOrStartOf l, loc)
       ; Set((mkMem (Lval(mkArraySelect s cond)) NoOffset), e, loc)]
    | Call (l, e, es, loc) ->
       let s = makeTempVar !currentFunc (makeArray (addrOfExp e)) in
       let r = match l with
         | Some l ->
            let (t, _, _, _) = splitFunctionType (typeOf e) in
            let s' = makeTempVar !currentFunc (makeArray (addrOfType t)) in
            [Set(mkArraySelect s' zero, dummyMem', loc)
            ; Set(mkArraySelect s' one, mkAddrOrStartOf l, loc)
            ; Set(mkArraySelect s zero, Lval(var dummyFun), loc)
            ; Set(mkArraySelect s one, e, loc)
            ; Call(Some (mkMem (Lval(mkArraySelect s' cond)) NoOffset)
                 , Lval(mkArraySelect s cond)
                 , es
                 , loc)]
         | None -> 
            [Set(mkArraySelect s zero, Lval(var dummyFun), loc)
            ; Set(mkArraySelect s one, e, loc)
            ; Call(None, Lval((mkArraySelect s cond)), es, loc)]
       in
       r
    | Asm (_, _, _, _, _, _) -> todo ()
  in
  mkStmt (Instr r)


let fltGoto sr loc cond : stmt list =
  let s = makeTempVar !currentFunc (makeArray voidPtrType) in
  let dummy = mkEmptyStmt () in
  let _ = dummy.labels <- [Label(freshLabel "FLT", dummyLocation, false)] in
  [mkStmt (Instr([Set(mkArraySelect s zero, AddrOfLabel(ref dummy), loc)
                 ; Set(mkArraySelect s one, AddrOfLabel(sr), loc)]))
  ; mkStmt (ComputedGoto(Lval(mkArraySelect s cond) , loc))
  ; dummy]

  
let mkLabel (l : label list) : stmt =
  let s = mkEmptyStmt () in
  s.labels <- l;
  s


let fltSpecial s cond : stmt list =
  let a = makeTempVar !currentFunc (makeArray voidPtrType) in
  let l1 = mkEmptyStmt () in
  let l2 = mkEmptyStmt () in
  let _ = l1.labels <- [Label(freshLabel "FLT", dummyLocation, false)] in
  let _ = l2.labels <- [Label(freshLabel "FLT", dummyLocation, false)] in
  [mkStmt (Instr([Set(mkArraySelect a zero, AddrOfLabel(ref l1), dummyLocation)
                 ; Set(mkArraySelect a one, AddrOfLabel(ref l2), dummyLocation)]))
  ; mkStmt (ComputedGoto(Lval(mkArraySelect a cond), dummyLocation))
  ; l2
  ; s
  ; l1]
  

let rec fltStmt s cond : stmt list =
  print_string "flt Stmt\n";
  let rec pickLabel = function
    | [] -> None
    | Label (l, _, _) :: _ -> Some l
    | _ :: rest -> pickLabel rest
  in
  let x =
    match s.skind with
    | Instr (x) ->
       List.map (fun e -> fltInstr e cond) x
    | Return (eop, loc) ->
       fltSpecial s cond
    | Goto (sr, loc) ->
       fltGoto sr loc cond
    | ComputedGoto (e, loc) -> todo ()
    | Break (loc) ->
       fltSpecial s cond
    | Continue (_) ->
       fltSpecial s cond
    | If (e, tb, eb, loc) ->
       let tmp = makeTempVar !currentFunc (typeOf e) in
       let set = mkStmt (Instr([Set(var tmp, e, loc)])) in
       let x = List.flatten (List.map (fun e' -> fltStmt e' (mkLAnd (mkTrue (Lval (var tmp))) cond)) tb.bstmts) in
       let y = List.flatten (List.map (fun e' -> fltStmt e' (mkLAnd (mkFalse (Lval (var tmp))) cond)) eb.bstmts) in
       set ::(x @ y)
    | Switch (_, _, _, _) ->
       fltSpecial s cond
    | Loop (b, loc, x, y) ->
       fltSpecial s cond
    | Block (b) ->
       List.flatten (List.map (fun e -> fltStmt e cond) b.bstmts)
    | TryFinally (_, _, _) ->  todo ()
    | TryExcept (_, _, _, _) ->  todo ()
  in
  match pickLabel s.labels with
  | None -> x
  | Some l ->
     let dummy = mkLabel [Label(freshLabel "FLT", dummyLocation, false)] in
     [mkStmt (Goto(ref dummy, dummyLocation))
     ; mkLabel s.labels
     ; (mkStmt (Instr(List.map (fun (vi, n) -> Set(var vi, n, dummyLocation)) (splitCondition cond))))
     ; dummy] @ x


let scanFunc f =
  currentFunc := f;
  let rec scanStmt s =
    match s.skind with
    | If (e, tb, eb, loc) ->
       print_string "scan If\n";
       let tmp = makeTempVar !currentFunc (typeOf e) in
       let set = mkStmt (Instr([Set(var tmp, e, loc)])) in
       let x = List.flatten (List.map (fun t -> fltStmt t (mkTrue (Lval(var tmp)))) tb.bstmts) in
       let y = List.flatten (List.map (fun t -> fltStmt t (mkFalse (Lval(var tmp)))) eb.bstmts) in
       s.skind <- Block(mkBlock (set :: (List.append x y)))
    | Block (b) ->
       print_string "scan Block\n";
       List.iter scanStmt b.bstmts
    | Loop (b, _, _, _) ->
       print_string "scan Loop\n";
       List.iter scanStmt b.bstmts
    | _ -> ()
  in
  List.iter scanStmt f.sbody.bstmts
  
  
let feature = 
  { fd_name = "flatten";
    fd_enabled = false;
    fd_description = "flatten for fun!" ;
    fd_extraopt = [];
    fd_doit = (function (f : file) -> 
                 let _ = iterGlobals f (fun glob ->
                             match glob with
                             | GFun (fd, _) -> scanFunc fd
                             | _ -> ())
                 in
                 f.globals <- GFun(dummyFunDecl, dummyLocation) :: f.globals;
                 f.globals <- GVar(dummyMem , {init=None}, dummyLocation) :: f.globals;
              );
    fd_post_check = true;
  }
  
let () = Feature.register feature
