
type regexp =
   | Eps
   | Chr of char
   | Any of regexp * regexp
   | Fol of regexp * regexp
   | Rep of regexp


type token = 
   | TChar of char
   | TEps
   | TOr
   | TStar
   | TOpen
   | TClose

let flatten t = List.fold_left (fun a b -> Any (a, b)) Eps t

let rec build stack top = 
   let hd :: tl = top in function
      | TChar c :: TStar :: rest -> build stack (Fol (hd, Rep (Chr c)) :: tl) rest
      | TClose  :: TStar :: rest -> build stack [Rep (flatten top)] (TClose :: rest)
      | TEps             :: rest -> build stack top rest
      | TChar c          :: rest -> build stack (Fol (hd, Chr c) :: tl) rest
      | TOr              :: rest -> build stack (Eps :: top) rest
      | TOpen            :: rest -> build (top :: stack) [Eps] rest
      | TClose           :: rest -> let (mh :: mt) :: stack = stack in
                                    build stack (Fol (mh, flatten top) :: mt) rest 
      | []                       -> flatten top
      | _                        -> failwith "Tools: unknown token"

let regexp_of_string str =
   let rec loop i esc =
      if i = String.length str then
         []
      else if esc then
         let t = match str.[i] with
         | '_' -> TChar ' '
         | 'n' -> TChar '\n'
         | 't' -> TChar '\t'
         |  c  -> TChar c
         in t :: loop (i + 1) false 
      else if str.[i] = '\\' then
         loop (i+1) true
      else 
         let t = match str.[i] with
         | '$' -> TEps
         | '|' -> TOr
         | '*' -> TStar
         | '(' -> TOpen
         | ')' -> TClose
         |  c  -> TChar c
         in t :: loop (i+1) false
   in build [] [Eps] (loop 0 false)

type action = 
   | NewLine
   | GoBack  of int
   | GoState of string
   | Lexeme  of string

type gen_rule = {
   regx : regexp;
   acts : action list;
   stat : string
}

