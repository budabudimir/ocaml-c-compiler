open Common

type token =
  | TChar of char
  | TEps
  | TOr
  | TStar
  | TOpen
  | TClose

let rec flatten = function
  | []       -> Eps
  | hd :: tl -> List.fold_left (fun a b -> AnyOf (a,b)) hd tl

let rec build stack top =
  let hd, tl = decons top in
  function
  | TChar c :: TStar :: rest -> build stack (Follow (hd, Repeat (Char c)) :: tl) rest
  | TClose  :: TStar :: rest -> build stack [Repeat (flatten top)] (TClose :: rest)
  | TEps    :: rest -> build stack top rest
  | TChar c :: rest -> build stack (Follow (hd, Char c) :: tl) rest
  | TOr     :: rest -> build stack (Eps :: top) rest
  | TOpen   :: rest -> build (top :: stack) [Eps] rest
  | TClose  :: rest -> let mid,    stack  = decons stack in
                       let mid_hd, mid_tl = decons mid in
                       build stack (Follow (mid_hd, flatten top) :: mid_tl) rest
  | [] -> flatten top
  | _  -> fail ()

let create regex = 
   let tokenize str =
     let rec loop i esc =
       if i = String.length str then
         []
       else if esc then
         let t = match str.[i] with
           | '_' -> TChar ' '
           | 'n' -> TChar '\n'
           | 't' -> TChar '\t'
           | c   -> TChar c
         in
         t :: loop (i+1) false
       else if str.[i] = '\\' then
         loop (i+1) true
       else
         let t = match str.[i] with
           | '$' -> TEps
           | '|' -> TOr
           | '*' -> TStar
           | '(' -> TOpen
           | ')' -> TClose
           | c   -> TChar c
         in
         t :: loop (i+1) false
     in loop 0 false
   in build [] [Eps] (tokenize regex)

