(*Implementation of the verivada algorithm*)

open Nerode

(*alphabet letters; labels of the terminals on the tree*)
type alpha = Alphabet.symbol

(*labels of the nonterminals on the tree*)
type nonterminal = char

(*labels of the nodes of the parse tree*)
type label = 
|Alpha of alpha
|Nonterminal of nonterminal

let alphaToLabel a = Alpha a
let nontermToLabel n = Nonterminal n

(*base parsetree type*)
type parseTree = 
| Leaf of alpha
| Node of nonterminal * parseTree list

let rec treeEquiv t1 t2 =
  match t1 with
    |Leaf a -> (match t2 with
                  | Leaf b -> a = b
                  | _ -> false)
    |Node (n,l) -> (match t2 with
                  | Leaf x -> false
                  | Node (n2,l2) -> n=n2 && List.fold_left2 (fun acc t1' t2' -> acc && (treeEquiv t1' t2')) true l l2)


module ParseTrees = struct
  type t = parseTree
  let compare t1 t2 =
    if treeEquiv t1 t2 then 0 else 1

end

(*type forest = Set.Make(ParseTrees).t*)

type forest = parseTree list





(*a bubblecandidate is a list of parse trees where the roots of these trees will
become the direct children of the new bubble nonterminal*)
type bubbleCandidate = parseTree list

(*a one bubble or a two bubble*)
type bubble =
| OneBubble of (char * bubbleCandidate)
| TwoBubble of (char * bubbleCandidate) * (char * bubbleCandidate)

(*bubble to tree, takes a character for the new bubble nonterminal and the 
bubble candidate provided and makes them into 1 (or 2, for a two bubble
candidate) parse tree(s) post-bubbling*)
let b2t (b:bubble) : parseTree list =
  match b with
  | OneBubble (c,l) -> [Node (c, l)]
  | TwoBubble ((c1,l1), (c2,l2)) -> [Node (c1,l1); Node(c2,l2)]






(*getting fresh nonterminals:*)
let rec findNewChar (candidate: char) (nonterms: char list) : char =
  if (List.mem candidate nonterms) then
    findNewChar (Char.chr ((Char.code candidate) + 1)) nonterms
  else candidate

let makeNewChar (nonterms: char list): char =
  let sorted = List.sort (Stdlib.compare) nonterms in
  let final = List.nth sorted (List.length sorted - 1) in
  let finalint = Char.code final in
  let newCandidate = Char.chr (finalint + 1) in findNewChar newCandidate nonterms

let rec makeNewChars (num: int) (nonterms: char list) (acc : char list) : char list =
  match num with
    |0 -> acc
    |_ -> let newbie = makeNewChar nonterms in
            makeNewChars (num-1) (nonterms @ [newbie]) (acc @ [newbie])




(*Generates from an input word a flat parse tree, including character nonterminals.*)
let flatTree (w : Word.t) = 
  let fresh = makeNewChars (List.length w) ['S'] [] in
  (Node ('S', (List.mapi (fun i a -> Node (List.nth fresh i, [Leaf a])) w)), fresh)


(*get the nonterminal label at the root of a parse tree; should not be a terminal
node beacuse we define our smallest valid parse trees as a character nonterminal parse tree*)
let treeVal (t : parseTree) = match t with
| Leaf a -> '0'
| Node (c,l) -> c


module StringSet = Set.Make(String)

(*the yield of a subtree*)
let rec yield (t : parseTree) : label list=
  match t with
  | Leaf a -> [alphaToLabel a]
  | Node (c,l) -> [nontermToLabel c] @ (List.fold_left (fun acc t2 -> acc @ yield(t2)) [] l)





(*all these below are helper functions to help get the bubble candidate*)
let trim l i (f : int) =
  let options = List.mapi (fun int x -> if (i <= int && int < f) then Some x else None) l in
  List.filter_map (fun z -> z) options

let label_equal (l1 : label) (l2 : label) =
  match l1 with
  |Alpha a1 -> (match l2 with
      |Alpha a2 -> if Alphabet.compare a1 a2 = 0 then true else false
      | _ -> false)
  |Nonterminal n1 -> (match l2 with
      |Nonterminal n2 -> n1 = n2
      | _ -> false)

(*checks if st is a sublist of y*)
let sublist (st : alpha list) (y : label list) : bool=
  let label_st = List.map (fun a -> alphaToLabel a) st in
  let st_len = List.length st in
  let y_len = List.length y in 
  let allSublists = List.mapi (fun i m -> trim y i (i+st_len)) (trim y 0 (y_len-st_len)) in
  List.fold_left (fun acc b -> acc || b) false (List.map (fun l -> List.equal (label_equal) label_st l) allSublists)

let rec commonAncestor (t : parseTree) (st : alpha list) : parseTree option =
  match t with
  | Leaf a -> None
  | Node (c,l) -> (match (sublist st (yield(t))) with 
      |true -> let nontermChild = List.nth l (List.length l - 1) in
        if (Bool.not (sublist st (yield nontermChild))) then Some t else (commonAncestor nontermChild st) 
      |false -> None)

let rec left_prune (t: parseTree) (st : label list) (nonterms : nonterminal list) =
  if (yield(t) = st) then (match t with | Leaf a -> None | Node (c,l) -> Some (Node(makeNewChar nonterms,l))) else
    (match t with
      |Leaf a -> None
      | Node (c,l) -> (match l with
          | h :: t -> left_prune (Node (c,t)) st nonterms
          | _ -> None)
        )


(*Gets all valid bubble candidates, as in all sequences of leaves that can be bubbled from the rightmost that aren't already captured*)
(*Will only be rightmost bc right spine tree*)
let getBubbleCandidates (forest : forest) (nonterms : nonterminal list) : bubbleCandidate list = 
  let (allCands : (alpha list * parseTree) list) = [] in
  let filterTree = List.filter_map (fun (st,t) -> let subtree = commonAncestor t st in
  let label_st = List.map (fun a -> alphaToLabel a) st in
    match subtree with
      | None -> None
      | Some subt -> if yield(subt) = label_st then None else (left_prune t label_st nonterms)) allCands in
  List.filter_map (fun t -> match t with |Leaf a -> None |Node (c,l) -> Some l) filterTree








(*getting all bubble candidates, for one bubbles and two bubbles:*)
let oneBubbleCandToBubble nonterminals (b : bubbleCandidate) : bubble = OneBubble (makeNewChar nonterminals, b)

let twoBubbleCands (candidates : bubbleCandidate list) : (bubbleCandidate * bubbleCandidate) list =
  List.fold_left2
  (fun (acc : (bubbleCandidate * bubbleCandidate) list) (c1 : (bubbleCandidate))
  (c2 : bubbleCandidate) -> if (c1 != c2) then (acc @ [(c1,c2)]) else acc)
  [] candidates candidates

let twoBubbleCandToBubble nonterminals (b1 : bubbleCandidate) (b2 : bubbleCandidate) : bubble =
  TwoBubble ((makeNewChar nonterminals, b1), (makeNewChar nonterminals, b2))




(*From a parse tree get a list of all its subtrees for exhaustive search reasons. SHOULD ONLY INCLUDE nonterminal subtrees, no leafs*)
let rec getSubtrees (t : parseTree) = match t with
  | Leaf a -> []
  | Node (c,l) -> [t] @ (List.fold_left (fun acc t -> acc @ (getSubtrees t)) [] l)

let getAllSubtrees (f : forest) = List.fold_left (fun acc t -> acc @ (getSubtrees t)) [] f






(*TODO - check if merge is valid*)
  let mergeIsValid (b1 : bubbleCandidate) (b2 : parseTree) = true


(*get a one bubble that is actually valid*)
let findOneBubble (b : bubbleCandidate) (forest : parseTree list) =
  let possibleNonterms = List.filter (fun n -> mergeIsValid b n) (getAllSubtrees(forest)) in
  match possibleNonterms with
  | [] -> None
  | h :: t -> Some (OneBubble (treeVal(h), b))



(*TODO - apply bubble list to forest, also return traces*)
let applyBubbles (forest : forest) (bubbles: bubble list) = forest, []

(*TODO*)
let mergeIsValid_2b (b1 : bubbleCandidate) (b2 : bubbleCandidate) = true


(*get a two bubble that is actually valid*)
let confirmTwoBubble ((b1,b2) : bubbleCandidate * bubbleCandidate) (nonterms) : (bubble list) option=
  if mergeIsValid_2b b1 b2 then (
    let fresh = makeNewChar nonterms in 
    Some [OneBubble (fresh,b1); OneBubble (fresh,b2)]) 
  else None

(*Find all possible one bubbles; apply them. Then find all possible two bubbles; apply them.*)
let bubble(forest, nonterminals, bubbles, traces, negatives) =
  let candidates : bubbleCandidate list = getBubbleCandidates forest nonterminals in
  let oneBubbles : bubble list = List.filter_map (fun bc -> findOneBubble bc forest) candidates in 
  let currForest, traces1 = applyBubbles forest oneBubbles in
  let remainingCandidates = getBubbleCandidates currForest nonterminals in
  let twoBubbleCand : (bubbleCandidate * bubbleCandidate) list = twoBubbleCands remainingCandidates in
  let twoBubbles : bubble list = List.flatten (List.filter_map (fun (bc1,bc2) -> confirmTwoBubble (bc1,bc2) nonterminals) twoBubbleCand) in
  let currForest, traces2 = applyBubbles currForest twoBubbles in
  (currForest, bubbles @ oneBubbles @ (twoBubbles), traces @ traces1 @ traces2, negatives)


let algorithm (l : Word.t list) =
  (*Initialization*)
  let temp = List.map flatTree l in
  let forest : forest = List.map (fun (x,y) -> x) temp in
  let nonterminals = List.fold_left (fun acc (t,l) -> acc @ l) ['S'] temp in
  let bubbles = [] in
  let traces = [forest] in
  let negatives = StringSet.empty in
  let currForest = forest in


  (*Bubbling; each round of bubbling only applies all currently possible bubbles, so I need
  to iterate over convergence of the bubble function until there are no more bubbles left to perform*)
  let bubbled1 = bubble(forest, nonterminals, bubbles, traces, negatives) in
  let currForest', _, _, _ = bubbled1 in

  (*very errorful while loop; too iterative for ocaml*)
  (while (currForest != currForest') do
  (let currForest = currForest' in
  let bubbled = bubble(forest, nonterminals, bubbles, traces, negatives) in
  let currForest', _, _, _ = bubbled in ())
  done);
  currForest'


  (*once this is done essentially ARVADA is complete; next I have to implement the
  Verifier oracle of Verivada and send this hypothesis forest to the Verifier to get counterexamples, etc etc*)




