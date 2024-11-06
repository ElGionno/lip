open Token
    
(* tokenize : Lexing.lexbuf -> LexingLib.Token.token list *)

let rec tokenize lexbuf =
  match Lexer.read_token lexbuf with
    EOF -> [EOF]
  | t -> t::(tokenize lexbuf)

(* lexer : string -> LexingLib.Token.token list *)

let lexer (s : string) =
  let lexbuf = Lexing.from_string s in
  tokenize lexbuf

(* string_of_tokenlist : token list -> string *)
    
let string_of_tokenlist tl = 
  List.fold_left (fun s t -> s ^ (string_of_token t ^ (if t=EOF then "" else " "))) "" tl

(* string_of_frequencies : (token * int) list -> string *)
    
let string_of_frequencies fl =
  List.fold_left (fun s (t,n) -> s ^ ((string_of_token t) ^ " -> " ^ string_of_int n ^ "\n")) "" fl

(* frequency : int -> 'a list -> ('a * int) list *)
(* Funzione ausiliaria per contare le occorrenze di ciascun elemento in una lista *)
let count_occurrences x lst =
  List.fold_left (fun acc y -> if x = y then acc + 1 else acc) 0 lst

(* Funzione principale per calcolare le frequenze dei token *)
let frequency n tokens =
  (* Rimuoviamo i duplicati per contare ciascun token una sola volta *)
  let unique_tokens = List.sort_uniq compare tokens in
  (* Creiamo una lista di coppie (token, numero di occorrenze) *)
  let freq_list = List.map (fun t -> (t, count_occurrences t tokens)) unique_tokens in
  (* Ordiniamo per numero di occorrenze in ordine decrescente *)
  let sorted_freq_list = List.sort (fun (_, count1) (_, count2) -> compare count2 count1) freq_list in
  (* Prendiamo i primi n elementi dalla lista ordinata *)
  let rec take n lst =
    match lst with
    | [] -> []
    | h :: t -> if n > 0 then h :: take (n - 1) t else []
  in
  take n sorted_freq_list
