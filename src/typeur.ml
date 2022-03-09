module Typeur = 
struct
 (* Sofiane BELKHIR *)
 (* David Herzog *)
  module Map = Map.Make(String)
    (*--------------------------------------------Type pTerm---------------------------------------------- *)

    type pterm =      Var of string   (*Variable *)
                    | App of pterm * pterm (*Application*)
                    | Abs of string * pterm (*Abstraction*)
                    | N of int  (*Entier*)
                    | Add of pterm * pterm  (*Addition*)
                    | Sub of pterm * pterm  (*Soustraction*)
                    | Liste of pterm liste  (*Liste*)
                    | Head of pterm  (*Head*)
                    | Tail of pterm  (*Tail*)
                    | IfZero of pterm * pterm * pterm (*If Zero then else*)
                    | IfEmpty of pterm * pterm * pterm (*If Empty then else*)
                    | Fix of pterm * pterm (*Point Fix*)
                    | LetIn of string * pterm * pterm  (*Let  .....  In*)
                    | Ref of pterm  (*Ref m*)
                    | Deref of pterm (*!n*)
                    | Assign of pterm * pterm (*m := n*)
                    
    and 'a liste  =  Cons of 'a * 'a liste 
                    | Nil
      (*-------------------------------------------pretty printer liste --------------------------------------------*)
      let rec printListeTerm (l : pterm liste) : string = 
        match l with  
          | Cons(s,p) ->  "( "^(printTerm s)^" , "^(printListeTerm p)^" )"
          | Nil ->"Nil"
          
      (*-------------------------------------------pretty printer Term--------------------------------------------- *)
      and  printTerm (t : pterm) : string =
        match t with
          | Var x -> x
          | App (t1, t2) -> "(" ^ (printTerm t1) ^" "^ (printTerm t2) ^ ")"
          | Abs (x, t) -> "(λ"^ x ^". " ^ (printTerm t) ^")" 
          | N n -> string_of_int n 
          | Add (t1,t2) -> "("^ (printTerm t1) ^" + "^ (printTerm t2) ^")"
          | Sub (t1,t2) -> "("^ (printTerm t1) ^" - "^ (printTerm t2) ^")"
          | Liste t -> "["^printListeTerm(t)^"]"
          | Head t -> "Head ["^printTerm(t)^"]"
          | Tail t -> "Tail ["^printTerm(t)^"]"
          | IfZero(a,b,c) -> "(IfZ ("^(printTerm a) ^") then "^(printTerm b)^" else "^(printTerm c)^")"
          | IfEmpty(a,b,c) -> "(IfEm ("^(printTerm a) ^") then "^(printTerm b)^" else "^(printTerm c)^")"
          | Fix(a,b) -> "Fix("^(printTerm a)^","^(printTerm b)^")"
          | LetIn(s,p1,p2)-> "(Let "^s^" = "^(printTerm p1)^" in "^(printTerm p2)^")"
          | Ref p1 -> "Ref("^printTerm p1^")"
          | Deref p1 -> "!("^printTerm p1^")"
          | Assign (p1, p2) -> "("^printTerm p1^" := "^(printTerm p2)^")"


      (*-------------------------------------------Constructeur Term--------------------------------------------- *)

      let makeVar (s : string): pterm =  Var s

      let makeApp (t1 : pterm) (t2 : pterm) : pterm = App (t1, t2)

      let makeAbs (s : string) (t : pterm) = Abs (s, t) 

      let makeAdd (t1 : pterm) (t2 : pterm) : pterm = Add (t1, t2)

      let makeN (n : int): pterm = N n

      let makeSub (t1 : pterm) (t2 : pterm) : pterm = Sub(t1,t2)

      let makeIfZero (p1 : pterm) (p2 : pterm) (p3 : pterm) : pterm = IfZero(p1,p2,p3)

      let makeIfEmpty (p1 : pterm) (p2 : pterm) (p3 : pterm) : pterm = IfEmpty(p1,p2,p3)

      let makeFix  (p1 : pterm) (p2 : pterm) = Fix(p1,p2)

      let makeLetIn (s : string) (p1 : pterm) (p2 : pterm) : pterm = LetIn(s,p1,p2) 

      let makeRef (p1 : pterm) = Ref(p1)

      let makeDeref (p1 : pterm) = Deref(p1)

      let makeAssign (p1 : pterm) (p2 :pterm) = Assign(p1,p2)

      (* Constructeur de list de hd et de tail --- a faire *)
      (*--------------------------------------------Type pType---------------------------------------------- *)
      type ptype =   Var of string 
                  |  Arr of ptype * ptype
                  |  L of ptype
                  |  Nat
                  |  ForAll of ptype * ptype
                  |  Ref of ptype 
                  |  Unit
                  
                  
      (*-------------------------------------------Constructeur Type--------------------------------------------- *)
      let makePVar (s : string) : ptype = Var s

      let makeArr (p1 : ptype) (p2 : ptype) : ptype = Arr (p1, p2) 

      let makeL (p1 : ptype) : ptype = L p1

      let makeNat : ptype = Nat

      let makePRef (t : ptype) : ptype = Ref t 

      let makeUnit : ptype = Unit

      (*-------------------------------------------pretty printer type--------------------------------------------- *)
      let rec printType (t : ptype) : string = 
        match t with 
          | Var s -> s
          | Arr (t1,t2) -> "("^(printType t1)^" -> "^(printType t2)^")"
          | L t1 -> "["^printType t1^"]"
          | Nat -> "Nat"
          | ForAll (t1 ,t2) -> "(∀"^(printType t1)^". "^(printType t2)^")"
          | Ref t1 -> "ref("^(printType t1)^")"
          | Unit -> "unit"
        
          

      (*égalité entre deux types*)
      let rec stype_egal (p1 : ptype) (p2 : ptype) : bool =
        match p1, p2 with
          |(Var s1 , Var s2) ->  true
          |(Arr (a,b) , Arr (c,d)) ->  (stype_egal a b) && (stype_egal c d)
          |(L t1, L t2) -> (stype_egal t1 t2)
          |(Nat, Nat) -> true
          |(Ref t1, Ref t2) -> (stype_egal t1 t2) 
          |_ -> false     


      (*-------------------------------------------type environnement--------------------------------------------- *)
      type env = (string * ptype) list 

      let printEnvironnement (monEnv : env) : string = let rec aux (maListe) : string =
                                                          match maListe with
                                                            |[] -> ""
                                                            |(a,b) :: e2 -> let s  = (printType b) in
                                                                               "("^a^" ,"^s^" )"^(aux e2) in
                                                            "["^aux monEnv^"]"

      (*-------------------------------------------type equa --------------------------------------------- *)
      type equa = (ptype * ptype) list

      (*----------------Débugage equation----------------*)
      let printEquation (maList : equa) : string = let rec aux l  = 
                                                      match l with
                                                        |(a,b)::e -> "("^printType a^","^printType b^")"^(aux e)
                                                        |[]->""
                                                      in "["^aux maList^"]"


      (* Trouver le type grace au nom  *)
      let rec trouverType (s : string) (monEnv : env) : ptype = 
        match monEnv with
          |[] -> raise Not_found
          |(s1,p) :: l when (s = s1) -> p 
          |(_,_) :: l -> trouverType s l

    

      (*-------------------------------------------Partie Sémentique--------------------------------------------- *)
      type resultBraendregt =   Some of string 
                              | None

      (* Compteur de variable  *)
      let (compteurVariable : int ref) = ref 0

      (* Sémentique typeur version 1  *)
      let nouvelleVariable () : string = compteurVariable := !compteurVariable + 1; 
                                    "T"^(string_of_int !compteurVariable)


      (*une fonction qui alpha-convertit un terme en le transformant en un terme qui respecte une convention de Barendregt*)
      let rec barendregt_rec (l: pterm)  (monEnv : (string * string) list) : pterm = 
          match l with
            | Var v1 -> let rec getElm (m:(string * string) list) : resultBraendregt =(match m with
                                                                                                  |(x,y)::res when x = v1-> Some y 
                                                                                                  |(x,y) :: res -> getElm res 
                                                                                                  |[] -> None ) in

                                                                                                  (match (getElm monEnv) with
                                                                                                    |Some r -> makeVar r
                                                                                                    |None -> makeVar v1)

            | App (t1, t2) ->  makeApp (barendregt_rec t1 monEnv) (barendregt_rec t2 monEnv)
            | Abs (x, t) ->  let nv : string = nouvelleVariable() in 
                                      let (newEnv : (string * string) list) = (x,nv) :: monEnv in
                                      makeAbs nv (barendregt_rec  t newEnv) 
            | N k -> N k
            | Add(t1,t2) -> Add(barendregt_rec t1 monEnv,barendregt_rec t2 monEnv)
            | Sub(t1,t2) -> Sub(barendregt_rec t1 monEnv,barendregt_rec t2 monEnv)
            | Liste t1 -> ( let rec aux l monE =
                                match l with
                                    Nil -> Nil
                                  | Cons (t1, t2) -> Cons(barendregt_rec t1 monEnv, aux t2 monEnv)
                                  in Liste (aux t1 monEnv))
            | Head t1 -> Head (barendregt_rec t1 monEnv)(* let rec aux l monE =
                              match l with
                                  Nil -> Nil
                                | Cons (t1, t2) -> Cons(barendregt_rec t1 monEnv, aux t2 monEnv)
                                in Head (aux t1 monEnv)*)

            | Tail t1 -> Tail (barendregt_rec t1 monEnv)(*let rec aux l monE =
                              match l with
                                  Nil -> Nil
                                | Cons (t1, t2) -> Cons(barendregt_rec t1 monEnv, aux t2 monEnv)
                                in Tail (aux t1 monEnv)*)

            | IfZero(t1,t2,t3) -> IfZero(barendregt_rec t1 monEnv, barendregt_rec t2 monEnv, barendregt_rec t3 monEnv)

            | IfEmpty(t1,t2,t3) -> IfEmpty(barendregt_rec t1 monEnv, barendregt_rec t2 monEnv, barendregt_rec t3 monEnv)

            | Fix(Var t1,t2) -> (let nv : string = nouvelleVariable() in 
                                    let (newEnv : (string * string) list) = (t1,nv) :: monEnv in
                                      Fix((Var nv), (barendregt_rec  t2 newEnv)) )

            | LetIn(s,t2,t3) -> LetIn(s,barendregt_rec t2 monEnv, barendregt_rec t3 monEnv)

            | Ref t1 -> Ref(barendregt_rec t1 monEnv)

            | Deref t1 -> Deref(barendregt_rec t1 monEnv)

            | Assign (p1, p2) -> Assign(barendregt_rec p1 monEnv, barendregt_rec p2 monEnv)

            | _ -> raise (Invalid_argument "B")

      (*Appel a la fonction barendregt_rec *)
      let barendregt (l: pterm) : pterm = 
        barendregt_rec l []

     

      (*une fonction qui alpha-convertit un Type en le transformant en un Type qui respecte une convention de Barendregt*)
      let rec barendregt_rec_type rmap (t: ptype) : ptype =
          match t with
                    Var x -> (try Var (Map.find x rmap) with
                              Not_found -> Var x)
                  | Arr (t1, t2) ->  Arr (barendregt_rec_type rmap t1, barendregt_rec_type rmap t2)
                  | Nat -> Nat
                  | L l -> L (barendregt_rec_type rmap l)
                  | ForAll (Var x, s) -> let nx = nouvelleVariable () in ForAll (Var nx, barendregt_rec_type (Map.add x nx rmap) s)
                  | ForAll (x, s) -> ForAll (barendregt_rec_type rmap x, barendregt_rec_type rmap s)
                  | Unit -> Unit
                  | Ref t1 -> Ref (barendregt_rec_type rmap t1)
               

      (*Appel a la fonction barendregt_rec_type*)
      let barendregt_type (t:ptype) : ptype =
              barendregt_rec_type Map.empty t


      (*une fonction qui substitue une variable par un terme dans un autre terme *)
      let rec instantie (l : pterm) (x : string) (a : pterm) : pterm = 
          match l with
              | Var v1 when (x == v1) -> a
              | Var v2 -> makeVar v2
              | App (t1, t2) ->  makeApp (instantie t1 x a) (instantie t2 x a)
              | Abs (y, t) ->  makeAbs (y) (instantie t x a) 
              | N k -> N k
              | Add(m,n) -> makeAdd (instantie m x a) (instantie n x a)
              | Sub(m,n) -> makeSub (instantie m x a) (instantie n x a)
              | Liste t1 -> ( let rec aux l e f =
                                     match l with
                                        Nil -> Nil
                                        | Cons (t1, t2) -> Cons(instantie t1 e f, aux t2 e f)
                in Liste (aux t1 x a))
              | Head t1 ->  Head (instantie t1 x a)(* let rec aux l e f =
                                     match l with
                                        Nil -> Nil
                                        | Cons (t1, t2) -> Cons(instantie t1 e f, aux t2 e f)
                in Head (aux t1 x a)*)
              | Tail t1 -> Tail (instantie t1 x a)(*let rec aux l e f =
                                     match l with
                                        Nil -> Nil
                                        | Cons (t1, t2) -> Cons(instantie t1 e f, aux t2 e f)
                in Tail (aux t1 x a)*)

              | IfZero(t1,t2,t3) -> IfZero(instantie t1 x a, instantie t2 x a, instantie t3 x a)
              
              | IfEmpty(t1,t2,t3) -> IfEmpty(instantie t1 x a, instantie t2 x a, instantie t3 x a)

              | Fix(t1,t2) -> Fix(t1, instantie t2 x a)

              | LetIn(s, t1, t2) -> LetIn(s, instantie t1 x a, instantie t2 x a)

              | Ref t1 -> Ref(instantie t1 x a)

              | Deref t1 -> Deref(instantie t1 x a)

              | Assign(p1,p2) -> Assign(instantie p1 x a, instantie p2 x a)


      (* une mémoire qui permet d'assosier une adresse a une valeur *)
      type memoire = (pterm * pterm) list

      (* un nouveau environnement *)
      type envPterm = (string * pterm) list

      (* Résultat de la sémentique *)
      type reval = 
            |Some of pterm * memoire * envPterm
            |None

      (* déclaration de la région RHO *)
      let addressId = ref 0
  
      (* allocation de la mémoire *)
      let alloc mem = (N(!addressId),N(-1))::mem 

      (*-------------------------------------------Gestion de la mémoire--------------------------------------------- *)
      (*retourner l'adresse*)
      let intOfAddress ads = 
        match ads with 
            N(adresse) -> adresse
           |_ ->failwith("ce n'est pas une adresse")

      (*ecrire dans un espace mémoire spécifique*)
      let rec ecrireMem mem a v =
            match mem with
              | [] -> []
              | (N(i),s) :: t -> (if  (intOfAddress a) = i 
                                      then (N(i),v) :: t
                                      else (N(i),s) :: ecrireMem t a v )
                                    
              | _ -> failwith("ce n'est pas une adresse ecrireMem")

      (*retourner la valeur assosier a une adresse*)
      let rec getValueAddress mem ads = 
            match mem with 
              | (N(l),v) :: xs-> if (intOfAddress ads) = l
                                then v
                                else getValueAddress xs ads 
              | [] -> failwith("l'adresse n'existe pas getValue")
              | _ ->  failwith("ce n'est pas une adresse")


      (*trouver une adresse d'un terme*)
      let rec getAdress (monEnv : envPterm) (t : pterm) : pterm =
            match monEnv, t with 
              |((s, N z) :: es, Var k) when s = k -> print_endline ("adress rho == "^ k ^" mon adresse == "^string_of_int z);N z
              |((s, N z) :: es, _) ->  getAdress es t
              |([], _) -> failwith("region RHO inéxistante")
              | _ -> failwith("envirronement error")

      (*trouver l'adresse d'une valeur dans une mémoire*)
      let rec getAddressValue mem val_val = match mem with
                                          |(ad,v) :: xs -> if  (v == val_val) then
                                                                      ad                               
                                                               else getAddressValue xs val_val
                                          |[] -> failwith("error value")

      (*Compteur pour les régions*)
      let (compteurRHO : int ref) = ref 0

      let rho = compteurRHO := !compteurRHO + 1; 
                "RHO"^(string_of_int !compteurRHO)
                
      
      (*une  étape d évaluation selon la stratégie Left-to-Right Call-by-Value*)
      let rec ltrcbv_etape (t : pterm) (mem : memoire) (monEnv : envPterm): reval = 
        match t with
          | Var x ->  None
          | Abs (x, t) -> None
          | App (m, n) -> (match (ltrcbv_etape m mem monEnv) with
                            |Some (a,mem,monEnv) -> Some(App(a,n),mem,monEnv)
                            |None -> (match (ltrcbv_etape n mem monEnv) with
                                                |Some (x,mem,monEnv) -> Some (App(m,x),mem,monEnv)
                                                |None -> (match m with
                                                            |Abs(e,f) -> Some(instantie f e n , mem, monEnv)
                                                            |_ -> None)))
          | N k -> None
          | Add(t1,t2) -> (match ltrcbv_etape t1 mem monEnv with
                              | None -> (match ltrcbv_etape t2 mem monEnv with 
                                                | None -> (match t1,t2 with
                                                            |(N k1, N k2) -> Some (N (k1+k2), mem, monEnv)
                                                            |(_,_) -> None)
                                          
                                                | Some (p,mem,monEnv) -> Some (Add(t1,p),mem,monEnv)
                                                
                                                )
                              | Some (tm,mem,monEnv) -> Some(Add(tm,t2),mem,monEnv)
                            )


          | Sub(t1,t2) -> (match ltrcbv_etape t1 mem monEnv with
                            | None -> (match ltrcbv_etape t2 mem monEnv with 
                                              | None -> (match t1,t2 with
                                                          |(N k1, N k2) -> Some (N (k1-k2), mem, monEnv)
                                                          |(_,_) -> None)
                                        
                                              | Some (p,mem,monEnv) -> Some (Sub(t1,p),mem,monEnv)
                                              
                                              )
                            | Some (tm,mem,monEnv) -> Some(Sub(tm,t2),mem,monEnv)
                          )

          | Liste t1 -> ( let rec aux l =
                               match l with
                                    Nil -> Nil
                                    | Cons (t1, t2) ->( match ltrcbv_etape t1 mem monEnv with
                                                            |None -> Cons(t1 , aux t2 )
                                                            |Some (x,mem,monEnv) -> Cons(x, aux t2) )
                          in  match ((aux t1)) with
                                    |Nil -> None
                                    |Cons(a,b) -> Some(Liste (Cons(a,b)),mem,monEnv))
                          
          | Head (Liste t1) ->( match t1 with
                          Cons(p1,q) ->  Some (p1,mem,monEnv)
                        | Nil -> None)
                        
                  
          | Tail (Liste t1) -> (match t1 with
                            Nil -> None
                            |Cons(p,q)-> Some (Liste q,mem,monEnv))

          | IfZero(t1,t2,t3) -> (match (ltrcbv_etape t1 mem monEnv) with
                                    |None -> (match t1 with 
                                                | N 0 -> Some (t2,mem,monEnv)
                                                | _ -> Some (t3,mem,monEnv))
                                    |Some (w,mem,monEnv) -> Some (IfZero(w,t2,t3),mem,monEnv))

          | IfEmpty(t1,t2,t3) -> (match t1 with
                                    |Liste Nil -> Some (t2,mem,monEnv)
                                    |Liste Cons(a,b) -> Some (t3,mem,monEnv)
                                    |_ -> None
                                                )

          | Fix(Var t1,t2) -> Some (Fix(Var t1, instantie t2 t1 (Fix(Var t1,t2))),mem,monEnv)
            

          | LetIn(s,t1,t2) -> (match (ltrcbv_etape t1 mem monEnv) with
                                  |None -> (let tmp = instantie t2 s t1 in
                                               match (ltrcbv_etape(tmp) mem monEnv) with
                                                  | None -> Some (tmp,mem,monEnv)
                                                  | Some (res,mem,monEnv) -> Some (res,mem,monEnv)
                                        
                                          )
                                  |Some (res,mem,monEnv) -> Some(LetIn(s,res,t2),mem,monEnv)
                          )

         | Ref t1 -> ( 
                        match (ltrcbv_etape t1 mem monEnv) with
                          | None ->  let newMem = alloc mem in 
                                      let newnewMem = ecrireMem newMem (N(!addressId)) t1 in
                                        let newEnv = (rho,N(!addressId)) :: monEnv in
                                          addressId := !addressId +1;
                                          Some(Var rho,newnewMem,newEnv)
                                         

                          | Some (res,mem,monEnv) -> Some(Ref res,mem,monEnv)
                    )

         | Deref t1 -> (match (ltrcbv_etape t1 mem monEnv) with
                          | None ->  (match t1 with
                                        |Var x -> Some(getValueAddress mem (getAdress monEnv (Var x)),mem,monEnv)
                                        |_ -> raise(failwith "Deref Error"))

                          | Some (res,mem,monEnv) -> Some((Deref res),mem,monEnv)
                     )

        | Assign(p1,p2) -> (match (ltrcbv_etape p1 mem monEnv) with
                                | None -> (match p1 with
                                            | Var rho1 -> (match (ltrcbv_etape p2 mem monEnv) with
                                                            | None -> let newMem = ecrireMem mem (getAdress monEnv (Var rho1) ) (p2) in
                                                                            Some(Var "()", newMem, monEnv)
                                                                               
                                                            
                                                            | Some (res,maMem,monEEnv) -> Some(Assign(p1,res),maMem,monEEnv)
                                                        )
                                            | _ -> failwith "Assign Error p1")
                                | Some (res,maMem,monEEnv) -> Some(Assign(res,p2),maMem,monEEnv)
        
                        )

        | _ -> None 

                     
      (* Echec gen equation*)
      exception EchecEquas of string

      


      (*----------------------------------printer equation débugage------------------------- *)
      let rec printEqua (m : equa) : string = 
        match m with
          |[] -> ""
          |(x,y):: r ->"("^(printType x)^", "^(printType y)^") "^(printEqua r)

      (*-------------------------------------- Partie Unification ---------------------------------------- *)

      (* vérifie si une variable appartient a un type *)
      (* a modifier *)
      let rec occur_check (v : string) (t : ptype) : bool = 
        match t with
          | Var x when (x = v) -> true 
          | Arr(t1, t2) -> (occur_check v t1) || (occur_check v t2)
          | Nat -> false
          | L t1 ->  occur_check v t1
          | ForAll(t1,t2) -> (occur_check v t2) 
          | Ref t1 -> (occur_check v t1)
          | Unit -> false
          | _ -> false

      (* substitue une variable de type par un type a l’intèrieur d’un autre type *)
      let rec substitue_type (t : ptype) (v : string) (t0 : ptype) : ptype =
        match t with
          Var v1 when v1 = v -> t0
        | Var v2 -> Var v2
        | Arr (t1, t2) -> Arr (substitue_type t1 v t0, substitue_type t2 v t0)
        | L t1 -> L (substitue_type t1 v t0)
        | ForAll (t1 ,t2) -> ForAll(t1, substitue_type t2 v t0)
        | Nat -> Nat
        | Unit -> Unit
        | Ref t1 ->Ref (substitue_type t1 v t0)
  

      (* substitue une variable de type par un type partout  dans un systeme d'equation *)
      let substitue_type_partout (e : equa) (v : string) (t0 : ptype) : equa =
        List.map (fun (x, y) -> (substitue_type x v t0, substitue_type y v t0)) e


      (* Un couple d'équations *)
      type equaList = equa * equa

      (* remplace une variable par un type dans un zipper d'Ã©quations *)
      let substitue_type_equaList (e : equaList) (v : string) (t0 : ptype) : equaList =
        match e with
          (e1, e2) -> (substitue_type_partout e1 v t0, substitue_type_partout e2 v t0)

      (* supprimer le premier élèment dans la premiére equation et l'ajouter a la deuxiéme equation *)   
      let rec subtitution (e : equaList) =
        match e with
            ([], _) -> e
          | (c::e1, e2) -> (e1, c::e2)

      (* Trouver le type d'une variable dans une liste d'équations *)
      let rec trouveType (e : equaList) (but : string) = 
        match e with
          (_, []) -> raise Not_found
        | (_, (Var v, t)::_) when v = but -> t
        | (_, (t, Var v)::_) when v = but -> t 
        | (e1, c::e2) -> trouveType (c::e1, e2) but 


      (* fonction qui détermine si une expression est non expansif *)
      (* en fonction de l'algorithme du cours *)
      let rec non_expansif (p : pterm) : bool = 
        match p with
          | Var x -> false 
          | App (t1,t2) -> (non_expansif t1) && (non_expansif t2)
          | Abs (s ,t1) -> true
          | N k -> true
          | Add (t1,t2) -> (non_expansif t1) && (non_expansif t2) 
          | Sub (t1,t2) -> (non_expansif t1) && (non_expansif t2) 
          | Liste m -> false 
          | Head m -> (non_expansif (m))
          | Tail m -> (non_expansif (m))
          | IfZero (c,t1,t2) -> (non_expansif t1) && (non_expansif t2)
          | IfEmpty (c,t1,t2) -> (non_expansif t1) && (non_expansif t2)
          | Fix (t1,t2) -> (non_expansif t2)
          | LetIn (v,e1,e2) -> (non_expansif e1) && (non_expansif e2)
          | Ref t1 -> false
          | Deref t1 -> (non_expansif t1)
          | Assign (t1,t2)-> (non_expansif t1) && (non_expansif t2) 
          
          

      (* Exception unification *)
      exception EchecUnif of string

      (* Unification dans les systèmes d’équations de typage selon un algo d’unification*)
      let rec unification_etape (eqs : equaList) (b : string) : ptype = 
        match eqs with 
        
          (* on a passe toutes les équations : succes *)
          |(_, []) -> (try (match  trouveType (subtitution eqs) b with
                              | ForAll(a, Arr (t1,t2)) -> t2
                              | t1 -> t1
                      )
                      with Not_found -> raise (EchecUnif "but introuvable"))

          (* deux variables : remplacer l'une par l'autre *)
          | (e1, (Var v1, Var v2)::e2) ->  unification_etape (substitue_type_equaList (subtitution (e1,e2)) v2 (Var v1)) b
          
          (* equation avec but : on passe *)
          | (e1, (Var v1, t2)::e2) when v1 = b -> unification_etape ((Var v1, t2)::e1, e2) b

          (* une variable a gauche : vérification d'occurence puis remplacement *)
          | (e1, (Var v1, t2)::e2) ->  if (occur_check v1 t2) then 
                                        raise (EchecUnif ("occurence de "^ v1 ^" dans "^(printType t2))) 
                                      else  
                                        unification_etape (substitue_type_equaList (subtitution (e1,e2)) v1 t2) b (* probleme *)
        
          (* une variable a droite : vérification d'occurence puis remplacement *)
          | (e1, (t1, Var v2)::e2) ->  if (occur_check v2 t1) then 
                                        raise (EchecUnif ("occurence de "^ v2 ^" dans " ^(printType t1))) 
                                      else  
                                        unification_etape (substitue_type_equaList (subtitution (e1,e2)) v2 t1) b
          
          (* types fleche des deux cotes : on decompose  *)
          | (e1, (Arr (t1,t2), Arr (t3, t4))::e2) -> unification_etape (e1, (t1, t3)::(t2, t4)::e2) b 
      
          (* types fleche a  gauche pas a droite : echec  *)
          | (e1, (Arr (_,_), t3)::e2) -> raise (EchecUnif ("type fleche non-unifiable avec "^(printType t3)))     
        
          (* types fleche a  droite pas a  gauche : echec  *)
          | (e1, (t3, Arr (_,_))::e2) -> raise (EchecUnif ("type fleche non-unifiable avec "^(printType t3)))   
            
          (* types nat des deux cotes : on passe *)
          | (e1, (Nat, Nat)::e2) -> unification_etape (e1, e2) b

          (* types nat a  gauche pas a  droite : echec *)
          | (e1, (Nat, t3)::e2) -> raise (EchecUnif ("type entier non-unifiable avec "^(printType t3)))   

          (* types a  droite pas a  gauche : echec *)
          | (e1, (t3, Nat)::e2) -> raise (EchecUnif ("type entier non-unifiable avec "^(printType t3))) 
              
           (* Unification ForAll a gauche *)
          | (e1, (ForAll(Var t1,t2), t3)::e2) -> (match barendregt_rec_type Map.empty (ForAll (t2,t3)) with
                                                        | ForAll (t11,t22) -> unification_etape(e1,(t22,t3):: e2) b
                                                        | _ -> raise (EchecUnif ("ForAll Error")))
                                    
                                                  

           (* Unification ForAll a droite *)
          | (e1, (t1,ForAll(t2,t3))::e2) -> (match barendregt_rec_type Map.empty (ForAll (t2,t3)) with
                                                | ForAll (t11,t22) ->  unification_etape(e1,(t1,t22):: e2) b
                         
                                                | _ -> raise (EchecUnif ("ForAll Error")))
                                    

          (* Unification des listes *)
          | (e1, (L t1, L t2)::e2) -> unification_etape (e1, (t1,t2) :: e2) b

          (* types  a  gauche pas a  droite : echec *)
          | (e1, (L t4, t3)::e2) -> raise (EchecUnif ("type liste non-unifiable avec "^(printType t3)))   

          (* types a  droite pas a  gauche : echec *)
          | (e1, (t3, L t4)::e2) -> raise (EchecUnif ("type liste non-unifiable avec "^(printType t3))) 

          (* types Ref a gauche et droite : unification *)
          | (e1, (Ref t1 , Ref t2)::e2) -> unification_etape (e1, ( t1,  t2) :: e2) b

          (* types Ref a droite  : unification *)
          | (e1, (t1 , Ref t2)::e2) ->  raise (EchecUnif ("type ref non-unifiable avec "^(printType t2)))
     
          (* types Ref a gauche  : unification *)
          | (e1, (Ref t1 , t2)::e2) -> print_endline "melissa"; raise (EchecUnif ("type ref non-unifiable avec "^(printType t1)))
 
          (* Unit des deux coté on passe *)
          | (e1,(Unit, Unit) :: e2) -> unification_etape (e1,e2) b

          (* Dans tout les autres cas on echoue *)
          | _ -> raise (EchecUnif ("type"))

        




      (*-------------------- Trouver les variables libres pour généraliser --------------------------*)
      let rec getVariablesLibres (l : pterm) (monEnv : string list)  =
         match l with 

          | Var v -> v :: monEnv

          | Abs(s,t1) -> let _  = getVariablesLibres t1 monEnv in
                            if((List.mem s monEnv)) then
                              List.filter (fun elm -> not(s == elm))monEnv
                            else
                              monEnv

          | App(t1,t2) -> let _ = getVariablesLibres t1 monEnv in
                            getVariablesLibres t2 monEnv

          | N k -> monEnv

          | Add(t1,t2) -> let _ = getVariablesLibres t1 monEnv in
                              getVariablesLibres t2 monEnv

          | Sub(t1,t2) -> let _ = getVariablesLibres t1 monEnv in
                              getVariablesLibres t2 monEnv

          | Liste t1  -> (let rec aux elements =
                            (match elements with
                              |Cons(a,b) -> (let _ =getVariablesLibres a monEnv in
                                                aux b)
                              |Nil -> monEnv) 
                              in
                                aux t1)

          | Head t1 -> getVariablesLibres t1 monEnv(*let rec aux elements =
                          (match elements with
                            |Cons(a,b) -> (let _ =getVariablesLibres a monEnv in
                                              aux b)
                            |Nil -> monEnv) 
                            in
                              aux t1*)

          | Tail t1 ->  getVariablesLibres t1 monEnv (*let rec aux elements =
                        (match elements with
                          |Cons(a,b) -> (let _ =getVariablesLibres a monEnv in
                                            aux b)
                          |Nil -> monEnv) 
                          in
                            aux t1*)

          | IfZero(t1,t2,t3) -> let _ = getVariablesLibres t1 monEnv in
                                  let _ = getVariablesLibres t2 monEnv in
                                    getVariablesLibres t3 monEnv

          | IfEmpty(t1,t2,t3) -> let _ = getVariablesLibres t1 monEnv in
                                    let _ = getVariablesLibres t2 monEnv in
                                      getVariablesLibres t3 monEnv

          | Fix(Var s,t1) -> let _  = getVariablesLibres t1 monEnv in
                                if((List.mem s monEnv)) then
                                  List.filter (fun elm -> not(s == elm))monEnv
                                else
                                  monEnv

          | LetIn(s, t1, t2) ->let _ = getVariablesLibres t1 monEnv in
                                  let _ = getVariablesLibres t2 monEnv in
                                    if((List.mem s monEnv)) then
                                      List.filter (fun elm -> not(s == elm))monEnv
                                    else
                                      monEnv

          | Ref t1 -> getVariablesLibres t1 monEnv

          | Deref t1 -> getVariablesLibres t1 monEnv

          | Assign(t1,t2) -> let _ = getVariablesLibres t1 monEnv in
                              getVariablesLibres t2 monEnv

          | _ -> monEnv


      (* ------------------------- generalisation du term ---------------------------- *)
      let generalise (envi : (string list)) (l : ptype) : ptype = 
        List.fold_left (fun acc elm -> ForAll(Var elm,acc)) l envi
      
      let evaluation(t : pterm) (mem : memoire) (monEnv : envPterm)= 
            let rec eval u i m e = match (ltrcbv_etape  (barendregt u) m e) , i with
                                  |(Some (res,newMem,newEnv) , m) when m == 0 -> Some (res,newMem,newEnv) 
                                  |(Some (res,newMem,newEnv), m) -> eval res (m-1) newMem newEnv
                                  |(None , _) -> Some (u,m,e) in
                                  eval  t 4 mem monEnv


      let execSementique  (t : pterm) (mem : memoire)  (monEnv : envPterm)= 
          match (evaluation  t mem monEnv) with
              |Some (res,_,_) -> "Résultats de l'Evaluation ====> "^printTerm res
              |None -> "Résultats de l'Evaluation ====> None"


      (*une fonction qui génère des  équations de typage a partir d un terme*)
      let rec gen_equas (monEnv : env) (l : pterm) (t : ptype) : equa =
        match l with
          | Var s1 ->  [(t, (trouverType s1 monEnv))] 
          | App (p1,p2) ->  let newV : string = nouvelleVariable() in
                                let (equation1 : equa) =  gen_equas monEnv  p1 (makeArr (makePVar newV) t ) in                
                                    let (equation2 : equa) =  gen_equas monEnv  p2  (makePVar newV) in
                                        equation1 @ equation2
          | Abs (s1,p1) ->  let newV1 : string = nouvelleVariable() in
                                let newV2 : string = nouvelleVariable() in                
                                    let elm  : ptype * ptype = (t,(makeArr (makePVar newV1) (makePVar newV2))) in 
                                        elm :: gen_equas ((s1, makePVar newV1) :: monEnv) p1 (makePVar newV2)
          | N k -> [t,Nat]

          | Add(t1,t2) -> let eq1 : equa = gen_equas monEnv t1 Nat  in
                            let eq2 : equa = gen_equas monEnv t2 Nat  in
                              (t, Nat)::(eq1 @ eq2)

          | Sub(t1,t2) -> let eq1 : equa = gen_equas monEnv t1 Nat  in
                              let eq2 : equa = gen_equas monEnv t2 Nat  in
                                (t, Nat)::(eq1 @ eq2)

          | Liste t1 -> ( match t1 with
                            | Nil -> []
                            | Cons(a,b)  -> let varfrech = nouvelleVariable() in
                                                (t, L (Var varfrech)) :: (gen_equas monEnv a (Var varfrech)) @ (gen_equas monEnv (Liste b) (L (Var varfrech))))
          

          | Head (Liste t1) -> (match t1 with
                          | Cons(a,b) -> (let frech = nouvelleVariable() in
                                [(t, ((L(Var frech))))] @ gen_equas monEnv (a)  ((Var frech)) @ gen_equas monEnv (Liste b) (L(Var frech)) )


                          | Nil -> [])

          | Head t1 ->  let frech = nouvelleVariable() in
                               [(t, ((L(Var frech))))]  @ gen_equas monEnv (t1) (L(Var frech)) 

          | Tail (Liste t1) ->  (match t1 with
                            |Cons(a,b) -> (let frech = nouvelleVariable() in
                              [(t, ((L(Var frech))))] @  gen_equas monEnv (a)  ((Var frech)) @gen_equas monEnv (Liste t1)  (L(Var frech)))
                            |Nil -> [])

          | Tail t1 -> let frech = nouvelleVariable() in
                          (t, L(Var frech)) :: gen_equas  monEnv (t1) (L(Var frech))

          | IfZero(t1,t2,t3) -> (let newV = nouvelleVariable() in 
                                  let eq1 : equa = gen_equas monEnv t1 Nat in
                                    let eq2 : equa = gen_equas monEnv t2 (makePVar newV) in
                                      let eq3 : equa = gen_equas monEnv t3 (makePVar newV) in
                                        let (a,b) = List.hd eq2 in
                                          let (c,d) = List.hd eq3 in
                                            if(stype_egal b d) then
                                              [(t,b)]@eq1@(eq2)@(eq3)
                                            else
                                              eq1@eq2@eq3
                                            )

          | IfEmpty(t1,t2,t3) -> (let newV = nouvelleVariable() in 
                                  let eq1 : equa = gen_equas monEnv t1 (ForAll(makePVar newV,L (makePVar newV) )) in
                                    let eq2 : equa = gen_equas monEnv t2 (makePVar newV) in
                                      let eq3 : equa = gen_equas monEnv t3 (makePVar newV) in
                                        let (a,b) = List.hd eq2 in
                                          let (c,d) = List.hd eq3 in
                                            if(stype_egal b d) then
                                              [(t,b)]@eq1@(eq2)@(eq3)
                                            else
                                              eq1@eq2@eq3
                                           )

          | Fix(Var s1,t2) -> let newV1  = makePVar (nouvelleVariable()) in
                                let newV2 = makePVar (nouvelleVariable()) in 
                                (t,  newV1) :: gen_equas ((s1, newV2) :: monEnv) t2 ((newV1))

                            
          | LetIn(s, t1, t2) -> (try
                                  
                                  let t0 = unification_etape (([], gen_equas monEnv t1  t  )) (printType t) in
                                    gen_equas ((s,(generalise (getVariablesLibres t1 []) t0))::monEnv) t2 t 

                                with
                                    EchecUnif s ->  raise (EchecEquas s))
                                
          | Ref t1 -> (let newV = nouvelleVariable() in
                            let eq1  =(makePRef (makePVar (newV))) in
                              (t, eq1) :: gen_equas monEnv t1  ((makePVar (newV)))
          )
                            
          | Deref t1 ->  (let newV = nouvelleVariable() in
                              [(t, (Var newV))] @ gen_equas monEnv t1  (makePRef (makePVar (newV)))
                        )

          | Assign (t1, t2) -> (let newV = nouvelleVariable() in
                                  let eq1  = ForAll(Var newV, makeArr (makePRef(makePVar newV)) (makeArr (makePVar newV) (Unit))) in
                                    [(t, Unit)] @ gen_equas monEnv t1 eq1
                              )

          | s -> raise (EchecEquas (printTerm s))
                                

      (* enchaine generation d'equation et unification  *)                                   
      let inference (t : pterm) : string =
        let (a,b) : equaList = ([], gen_equas [] t  (Var "but")  ) in
        
        try (
            let res = unification_etape (a,b) "but" in
              "Génération Equation ===>"^printEqua b^"\n"^(printTerm t)^" ***TYPABLE*** avec le type "^(printType res)^"\n"^(execSementique t [] []))

        with EchecUnif bla -> "Génération Equation ===>"^printEqua b^ "\n"^(printTerm t)^" ***PAS TYPABLE*** : "^bla

      
end;;