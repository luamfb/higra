fun check_alpha (c:char) : bool = 
  (c >= #"a" andalso c <= #"z")
  orelse (c >= #"A" andalso c <= #"Z")

fun check_digit (c: char) : bool =
  c >= #"0" andalso c <= #"9" 

fun check_id_char (c: char): bool =
  (check_alpha c) orelse (check_digit c) orelse c = #"_"

fun parse_hex_pure (c : char) : int option =
  case c of
       #"0" => SOME 0
     | #"1" => SOME 1
     | #"2" => SOME 2
     | #"3" => SOME 3
     | #"4" => SOME 4
     | #"5" => SOME 5
     | #"6" => SOME 6
     | #"7" => SOME 7
     | #"8" => SOME 8
     | #"9" => SOME 9
     | (#"a" | #"A") => SOME 10
     | (#"b" | #"B") => SOME 11
     | (#"c" | #"C") => SOME 12
     | (#"d" | #"D") => SOME 13
     | (#"e" | #"E") => SOME 14
     | (#"f" | #"F") => SOME 15
     | _ => NONE

structure Token = struct
  datatype TokenKind = Semicolon
                     | Comma
                     | Equals
                     | OpenBrace
                     | CloseBrace
                     | OpenBracket
                     | CloseBracket
                     | ContDirEdge
                     | ContNonDirEdge
                     | DottedDirEdge
                     | DottedNonDirEdge
                     | Id of string
                     | StrLit of string
                     | NumLit of int
                     | BoolLit of bool
                     | ColorCode of int * int * int

  type SourcePos = {line: int, col: int}
  type Token = TokenKind * SourcePos

  exception InvalidToken of SourcePos
end

exception InternalError
type LexState = string * Token.SourcePos

fun raise_invalid_token ((_, pos): LexState) : 'a =
  raise (Token.InvalidToken pos)

fun new_lexer (s: string) : LexState = (s, {line=1, col=1})

fun advance (("", pos): LexState) = (("", pos), NONE)
  | advance (s, {line, col}) =
  let
    val cur_char = String.sub (s, 0)
    val new_s = String.extract (s, 1, NONE)
    val new_state =
      if cur_char = #"\n" then
        (new_s, {line=(line + 1), col=1})
      else
        (new_s, {line=line, col=(col + 1)})
  in
    (new_state, SOME cur_char): LexState * char option
  end

(* raises an exception if the lexer finishes before advancing all steps *)
fun advanceBy 0 (state: LexState) : LexState = state
  | advanceBy n state =
  if n < 0 then
    raise InternalError
  else
    let 
      val (new_state, char_opt) = advance state 
    in
      case char_opt of
         NONE => raise InternalError
       | _ => advanceBy (n - 1) new_state
    end

fun eat_whitespace (state: LexState) : LexState =
  case advance state of
       (("", pos), NONE) => ("", pos)
     | (new_state, cur_char) =>
         case cur_char of
              SOME (#" " | #"\n" | #"\t" | #"\r") => eat_whitespace new_state
            | _ => state


(* takes a "pure" (stateless) function and runs it in the lexer.
 * This raises an exception if the function returns NONE, or
 * if the lexer can't give new characters.
 *)
fun lexify_pure_fn (f: char -> 'a option) (state : LexState)
  : (LexState * 'a) =
  case advance state of
       (_, NONE) => raise_invalid_token state
     | (new_state, SOME cur_char) =>
         case (f cur_char) of
              NONE => raise_invalid_token state
            | SOME x => (new_state, x)

(* accumulates characters from lexer until pred returns true.
 * If delim is true, the character where pred returns true is considered
 * a delimiter and is "swallowed" by the function. Also, if delim is true
 * and the lexer finishes before pred returns true, this raises an
 * exception.
 *)
fun acc_until_pred (pred: char -> bool) (delim: bool) (state0: LexState)
  : LexState * string =
  case advance state0 of
       (_, NONE) =>
       if delim then
         raise_invalid_token state0
       else
         (state0, "")
     | (state1, SOME c) =>
         if (pred c) then
           if delim then
             (state1, "")
           else
             (state0, "")
         else
           let
             val (state2, s) = acc_until_pred pred delim state1
             val new_s = (str c) ^ s
           in
             (state2, new_s)
           end

(* assumes '-' has been previously found *)
val cont_edge : LexState -> LexState * Token.TokenKind =
  let
    val pure_fn = fn c => case c of
                               #">" => SOME Token.ContDirEdge
                             | #"-" => SOME Token.ContNonDirEdge
                             | _ => NONE
  in
    lexify_pure_fn pure_fn
  end

(* assumes '.' has been previously found *)
val dotted_edge : LexState -> LexState * Token.TokenKind =
  let
    val pure_fn = fn c => case c of
                               #">" => SOME Token.DottedDirEdge
                             | #"." => SOME Token.DottedNonDirEdge
                             | _ => NONE
  in
    lexify_pure_fn pure_fn
  end

(* assumes input has 2 hex digits *)
fun parse_hex_number (state0: LexState) : LexState * int =
let
  val (state1, d1) = lexify_pure_fn parse_hex_pure state0
  val (state2, d2) = lexify_pure_fn parse_hex_pure state1
in
  (state2, 16 * d1 + d2)
end

(* assumes '#' has already been found *)
fun color_code (state0: LexState) : LexState * Token.TokenKind =
let
  val (state1, r) = parse_hex_number state0
  val (state2, g) = parse_hex_number state1
  val (state3, b) = parse_hex_number state2
in
  (state3, Token.ColorCode (r, g, b))
end

fun num_literal (state: LexState) : LexState * Token.TokenKind =
let
  val check_non_digit = fn c => c < #"0" orelse c > #"9"
  val (new_state, num_str) = acc_until_pred check_non_digit false state
in
  (new_state, Token.NumLit (Option.valOf (Int.fromString num_str)))
end

fun str_literal (state : LexState) : LexState * Token.TokenKind =
let
  val check_dquote = fn c => c = #"\""
  val into_strlit = fn (st, s) => (st, (Token.StrLit s))
in
  into_strlit (acc_until_pred check_dquote true state)
end

(* for tokens that start with an alphabetic character; could be either
 * an ID or a boolean literal
 *)
fun alpha_token (state: LexState) : LexState * Token.TokenKind =
let
  val check_non_id_char = fn c => not (check_id_char c)
  val into_id = fn (st, s) => (st, (Token.Id s))
in
  case state of
       ("", _) => raise InternalError
     | (s, _) =>
         if String.isPrefix "true" s then
           ((advanceBy (size "true") state), (Token.BoolLit true))
         else if String.isPrefix "false" s then
           ((advanceBy (size "false") state), (Token.BoolLit false))
         else
           into_id (acc_until_pred check_non_id_char false state)
end

(* assumes state's current character is non-whitespace *)
fun next_token (state: LexState) : LexState * Token.TokenKind option =
let
  val tok_to_opt = fn (st, tok) => (st, SOME tok)
in
  case advance state of
       (new_state, NONE) => (new_state, NONE)
     | (new_state, SOME c) =>
       case c of
            (* single-char tokens *)
            #"," => (new_state, SOME Token.Comma)
          | #";" => (new_state, SOME Token.Semicolon)
          | #"=" => (new_state, SOME Token.Equals)
          | #"{" => (new_state, SOME Token.OpenBrace)
          | #"}" => (new_state, SOME Token.CloseBrace)
          | #"[" => (new_state, SOME Token.OpenBracket)
          | #"]" => (new_state, SOME Token.CloseBracket)
          (* edge tokens *)
          | #"-" => tok_to_opt (cont_edge new_state)
          | #"." => tok_to_opt (dotted_edge new_state)
          (* string / color literals *)
          | #"#" => tok_to_opt (color_code new_state)
          | #"\"" => tok_to_opt (str_literal new_state)
          (* IDs / other literals *)
          | _ =>
              if (check_digit c) then
                (* use previous state to include the current digit *)
                tok_to_opt (num_literal state)
              else if (check_alpha c) then
                (* again, use previous state *)
                tok_to_opt (alpha_token state)
              else raise_invalid_token new_state
end

fun get_remaining_tokens (state0: LexState) : Token.Token list =
let
  val state1 = eat_whitespace state0
in
  case state1 of
       ("", _) => []
     | (s, {line, col}) =>
         case next_token state1 of
              (_, NONE) => []
            | (state2, SOME kind) =>
                let
                  val token = (kind, {line=line, col=col})
                in
                  token :: (get_remaining_tokens state2)
                end
end

structure Lexer = struct
  fun tokenize (s: string) : Token.Token list =
    get_remaining_tokens (new_lexer s)
end
