open Typeur

let ex_id : Typeur.pterm = Typeur.Abs ("x", Var "x") 
let inf_ex_id : string = Typeur.inference ex_id 

let ex_k : Typeur.pterm = Typeur.Abs ("x", Abs ("y", Var "x")) 
let inf_ex_k : string = Typeur.inference ex_k

let ex_nat : Typeur.pterm = Typeur.N 5
let inf_ex_nat : string = Typeur.inference ex_nat

let ex_add : Typeur.pterm = Typeur.Add(Typeur.N 3, Typeur.N 5)
let inf_ex_add : string = Typeur.inference ex_add

let ex_add_add : Typeur.pterm = Typeur.Abs("x",Typeur.Abs("y",Typeur.Add(Typeur.Var "x", Typeur.Var "y")))
let inf_ex_add_add : string = Typeur.inference ex_add_add

let ex_sub : Typeur.pterm = Typeur.Sub(Typeur.N 5, Typeur.N 3)
let inf_ex_sub : string = Typeur.inference ex_sub


let ex_s : Typeur.pterm = Typeur.Abs ("x", Typeur.Abs ("y", Typeur.Abs ("z", Typeur.App (Typeur.App (Typeur.Var "x", Typeur.Var "z"), Typeur.App (Typeur.Var "y", Typeur.Var "z")))))
let inf_ex_s : string = Typeur.inference ex_s 


let ex_nat1 : Typeur.pterm = Typeur.App (Typeur.Abs ("x", Typeur.Add(Typeur.Var "x", Typeur.N 1)), Typeur.N 3)
let inf_ex_nat1 : string = Typeur.inference ex_nat1


let ex_nat2 : Typeur.pterm = Typeur.Abs ("x", Typeur.Add( Typeur.Var "x", Typeur.Var "x"))
let inf_ex_nat2 : string = Typeur.inference ex_nat2

let ex_liste_nil : Typeur.pterm = Typeur.Liste(Typeur.Nil)
let inf_ex_liste_nil : string = Typeur.inference ex_liste_nil


let ex_liste : Typeur.pterm = Typeur.Liste(Typeur.Cons ((Typeur.N 5),(Typeur.Cons (Typeur.N 3, Typeur.Nil))))
let inf_ex_liste : string = Typeur.inference ex_liste


let ex_liste_nil_head : Typeur.pterm = Typeur.Head(Typeur.Liste(Typeur.Nil))
let inf_ex_liste_nil_head : string = Typeur.inference ex_liste_nil_head

let ex_liste_head : Typeur.pterm = Typeur.Head(Typeur.Liste(Typeur.Cons ((Typeur.N 5),(Typeur.Cons (Typeur.N 3, Typeur.Nil)))))
let inf_ex_liste_head : string = Typeur.inference ex_liste_head



let ex_liste_nil_tail : Typeur.pterm = Typeur.Tail(Typeur.Liste(Typeur.Nil))
let inf_ex_liste_nil_tail : string = Typeur.inference ex_liste_nil_tail

let ex_liste_nil_tail2 : Typeur.pterm = Typeur.Tail(Typeur.Liste(Typeur.Cons(Typeur.N 1,Typeur.Nil)))
let inf_ex_liste_nil_tail2 : string = Typeur.inference ex_liste_nil_tail2

let ex_liste_tail : Typeur.pterm = Typeur.Tail(Typeur.Liste(Typeur.Cons ((Typeur.N 5),(Typeur.Cons (Typeur.N 3, Typeur.Nil)))))
let inf_ex_liste_tail : string = Typeur.inference ex_liste_tail

let ex_ifZero_t1 : Typeur.pterm = Typeur.IfZero(Typeur.N 0,Typeur.N 1, Typeur.N 2)
let inf_ex_ifZero_t1 : string = Typeur.inference ex_ifZero_t1

let ex_ifZero_t2 : Typeur.pterm = Typeur.IfZero(Typeur.N 1,Typeur.N 2, Typeur.N 3)
let inf_ex_ifZero_t2 : string = Typeur.inference ex_ifZero_t2

let ex_ifZero_t3 : Typeur.pterm = Typeur.IfZero(Typeur.N 0,ex_add, ex_sub)
let inf_ex_ifZero_t3 : string = Typeur.inference ex_ifZero_t3

let ex_ifZero_t4 : Typeur.pterm = Typeur.IfZero(Typeur.N 1,ex_add, ex_sub)
let inf_ex_ifZero_t4 : string = Typeur.inference ex_ifZero_t4

let ex_ifZero_t5 : Typeur.pterm = Typeur.IfZero(Typeur.N 1,ex_add, ex_liste_nil_tail2)
let inf_ex_ifZero_t5 : string = Typeur.inference ex_ifZero_t5

let ex_ifEmpty_t1 : Typeur.pterm = Typeur.IfEmpty(Typeur.Liste (Typeur.Nil),ex_add, ex_sub)
let inf_ex_ifEmpty_t1 : string = Typeur.inference ex_ifEmpty_t1

let ex_ifEmpty_t2 : Typeur.pterm = Typeur.IfEmpty(ex_liste,ex_add, ex_sub)
let inf_ex_ifEmpty_t2 : string = Typeur.inference ex_ifEmpty_t2


let ex_fix_t1 : Typeur.pterm = Typeur.Fix(Typeur.Var "f", (Typeur.Abs("x",Typeur.IfZero(Typeur.Var "x" , (Typeur.N 0) , Typeur.Add(Typeur.App(Typeur.Var "f" , Typeur.Sub(Typeur.Var "x", Typeur.N 1)), Typeur.Var "x")))))
let inf_ex_fix_t1 : string = Typeur.inference ex_fix_t1


let ex_letIn : Typeur.pterm = Typeur.LetIn("x",ex_sub, Typeur.Add(Typeur.Var "x", Typeur.N 2))
let inf_ex_letIn : string = Typeur.inference ex_letIn

let ex_ref : Typeur.pterm = Typeur.Ref(Typeur.N 2)
let inf_ex_ref : string = Typeur.inference ex_ref

let ex_ref_1 : Typeur.pterm = Typeur.Abs("x",Typeur.Ref(Typeur.Var "x"))
let inf_ex_ref_1 : string = Typeur.inference ex_ref_1

let ex_dref : Typeur.pterm = Typeur.Deref(ex_ref)
let inf_ex_dref : string = Typeur.inference ex_dref

let ex_assign : Typeur.pterm = Typeur.Assign(ex_ref, (Typeur.N 3))
let inf_ex_assign : string = Typeur.inference ex_assign

let ex_letInAs : Typeur.pterm = Typeur.LetIn("x",Typeur.Ref(Typeur.N 2),Typeur.LetIn("y",Typeur.Ref(Typeur.N 3), Typeur.LetIn("z",Typeur.Assign(Typeur.Var "x",(Typeur.Deref (Typeur.Var "y"))), Typeur.Deref(Typeur.Var "x") ) ) )
let inf_ex_letInAs : string = Typeur.inference ex_letInAs



let ex_letInAsTy : Typeur.pterm = Typeur.LetIn("l",Typeur.Ref(Typeur.Liste (Typeur.Cons(Typeur.N 5 , Typeur.Nil))), (Typeur.LetIn("_",Typeur.Assign(Typeur.Var "l", (Typeur.Liste(Typeur.Cons(Typeur.Abs("z",Typeur.Var "z"), Typeur.Nil)))), Typeur.Add(Typeur.Head(((Typeur.Deref(Typeur.Var "l")))), Typeur.N 2 ))))
let inf_ex_letInAsTy : string = Typeur.inference ex_letInAsTy
 

let main () =
  print_endline "======================\n";
  print_endline inf_ex_id;
  print_endline "======================\n";
  print_endline inf_ex_k;
  print_endline "======================\n";
  print_endline inf_ex_nat;
  print_endline "======================\n";
  print_endline inf_ex_add;
  print_endline "======================\n";
  print_endline inf_ex_add_add;
  print_endline "======================\n";
  print_endline inf_ex_sub;
  print_endline "======================\n";
  print_endline inf_ex_liste_nil;
  print_endline "======================\n";
  print_endline inf_ex_liste;
  print_endline "======================\n";
  print_endline inf_ex_liste_nil_head;
  print_endline "======================\n";
  print_endline inf_ex_liste_head;
  print_endline "======================\n";
  print_endline inf_ex_liste_nil_tail;
  print_endline "======================\n";
  print_endline inf_ex_liste_nil_tail2;
  print_endline "======================\n";
  print_endline inf_ex_liste_tail;
  print_endline "======================\n";
  print_endline inf_ex_s;
  print_endline "======================\n";
  print_endline inf_ex_nat1;
  print_endline "======================\n";
  print_endline inf_ex_nat2;
  print_endline "======================\n";
  print_endline inf_ex_ifZero_t1;
  print_endline "======================\n";
  print_endline inf_ex_ifZero_t2;
  print_endline "======================\n";
  print_endline inf_ex_ifZero_t3;
  print_endline "======================\n";
  print_endline inf_ex_ifZero_t4;
  print_endline "======================\n";
  print_endline inf_ex_ifZero_t5;
  print_endline "======================\n";
  print_endline inf_ex_ifEmpty_t1;
  print_endline "======================\n";
  print_endline inf_ex_ifEmpty_t2;
  print_endline "======================\n";
  print_endline inf_ex_fix_t1;
  print_endline "======================\n";
  print_endline inf_ex_letIn;
  print_endline "======================\n";
  print_endline inf_ex_ref;
  print_endline "======================\n";
  print_endline inf_ex_ref_1;
  print_endline "======================\n";
  print_endline inf_ex_dref;
  print_endline "======================\n";
  print_endline inf_ex_assign;
  print_endline "======================\n";
  print_endline inf_ex_letInAs;
  print_endline "======================\n";
  print_endline inf_ex_letInAsTy;; 



main()
 