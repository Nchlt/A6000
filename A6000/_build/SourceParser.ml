
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | WHILE
    | VAR
    | TRUE of (
# 13 "SourceParser.mly"
       (bool)
# 13 "SourceParser.ml"
  )
    | THEN
    | SUB
    | SET
    | SEMI
    | PRINT
    | OR
    | NEQ
    | MULT
    | MINUS
    | MAIN
    | LT
    | LE
    | INTEGER of (
# 12 "SourceParser.mly"
       (int)
# 30 "SourceParser.ml"
  )
    | INT
    | INCR
    | IF
    | IDENT of (
# 8 "SourceParser.mly"
       (string)
# 38 "SourceParser.ml"
  )
    | FOR
    | FALSE of (
# 14 "SourceParser.mly"
       (bool)
# 44 "SourceParser.ml"
  )
    | EQ
    | EOF
    | END
    | ELSE
    | DIV
    | BOOL
    | BEGIN
    | AND
    | ADD
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState70
  | MenhirState65
  | MenhirState61
  | MenhirState57
  | MenhirState55
  | MenhirState53
  | MenhirState51
  | MenhirState48
  | MenhirState45
  | MenhirState43
  | MenhirState41
  | MenhirState39
  | MenhirState37
  | MenhirState35
  | MenhirState33
  | MenhirState31
  | MenhirState29
  | MenhirState27
  | MenhirState25
  | MenhirState17
  | MenhirState16
  | MenhirState14
  | MenhirState10
  | MenhirState6

# 1 "SourceParser.mly"
  

  open SourceAst


# 102 "SourceParser.ml"

let rec _menhir_goto_instruction : _menhir_env -> 'ttv_tail -> _menhir_state -> (SourceAst.instruction) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMI ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FOR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | IDENT _v ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
        | IF ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | PRINT ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | WHILE ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | END ->
            _menhir_reduce17 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run25 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | IDENT _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | INTEGER _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | TRUE _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25

and _menhir_run29 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | IDENT _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | INTEGER _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | TRUE _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29

and _menhir_run27 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | IDENT _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | INTEGER _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | TRUE _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27

and _menhir_run31 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | IDENT _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | INTEGER _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | TRUE _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31

and _menhir_run35 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | IDENT _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | INTEGER _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | TRUE _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35

and _menhir_run39 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | IDENT _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | INTEGER _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | TRUE _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39

and _menhir_run33 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | IDENT _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | INTEGER _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | TRUE _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33

and _menhir_run41 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | IDENT _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | INTEGER _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | TRUE _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41

and _menhir_run37 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | IDENT _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | INTEGER _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | TRUE _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37

and _menhir_goto_literal : _menhir_env -> 'ttv_tail -> _menhir_state -> (SourceAst.literal) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (lit : (SourceAst.literal)) = _v in
    let _v : (SourceAst.expression) = 
# 105 "SourceParser.mly"
                                          ( Literal(lit) )
# 309 "SourceParser.ml"
     in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_instructions : _menhir_env -> 'ttv_tail -> _menhir_state -> (SourceAst.block) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | FOR ->
                    _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState61
                | IDENT _v ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
                | IF ->
                    _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState61
                | PRINT ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState61
                | WHILE ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState61
                | END ->
                    _menhir_reduce17 _menhir_env (Obj.magic _menhir_stack) MenhirState61
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (cond : (SourceAst.expression))), _, (iter : (SourceAst.block))), _, (ins : (SourceAst.block))) = _menhir_stack in
            let _9 = () in
            let _7 = () in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (SourceAst.instruction) = 
# 74 "SourceParser.mly"
  (
    let new_ins = iter @ ins in
    While(cond, new_ins)
  )
# 382 "SourceParser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (i : (SourceAst.instruction))), _, (is : (SourceAst.block))) = _menhir_stack in
        let _2 = () in
        let _v : (SourceAst.block) = 
# 63 "SourceParser.mly"
                                          ( i :: is           )
# 399 "SourceParser.ml"
         in
        _menhir_goto_instructions _menhir_env _menhir_stack _menhir_s _v
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ELSE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BEGIN ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | FOR ->
                        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState70
                    | IDENT _v ->
                        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
                    | IF ->
                        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState70
                    | PRINT ->
                        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState70
                    | WHILE ->
                        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState70
                    | END ->
                        _menhir_reduce17 _menhir_env (Obj.magic _menhir_stack) MenhirState70
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (e : (SourceAst.expression))), _, (ins_if : (SourceAst.block))), _, (ins_else : (SourceAst.block))) = _menhir_stack in
            let _10 = () in
            let _8 = () in
            let _7 = () in
            let _6 = () in
            let _4 = () in
            let _3 = () in
            let _1 = () in
            let _v : (SourceAst.instruction) = 
# 71 "SourceParser.mly"
                                            ( If(e, ins_if, ins_else) )
# 476 "SourceParser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (e : (SourceAst.expression))), _, (ins : (SourceAst.block))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (SourceAst.instruction) = 
# 69 "SourceParser.mly"
                                                         ( While(e, ins) )
# 501 "SourceParser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | EOF ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, (x : (
# 8 "SourceParser.mly"
       (string)
# 526 "SourceParser.ml"
                ))), _, (vds : (SourceAst.identifier_info SourceAst.Symb_Tbl.t))), _, (is : (SourceAst.block))) = _menhir_stack in
                let _10 = () in
                let _9 = () in
                let _6 = () in
                let _5 = () in
                let _3 = () in
                let _2 = () in
                let _1 = () in
                let _v : (
# 36 "SourceParser.mly"
      (SourceAst.main)
# 538 "SourceParser.ml"
                ) = 
# 43 "SourceParser.mly"
                                                   (
    let infox = { typ=TypInteger; kind=FormalX } in
    let init  = Symb_Tbl.singleton x infox in
    let union_vars = fun _ _ v -> Some v in
    let locals = Symb_Tbl.union union_vars init vds in
    {locals = locals; code=is} )
# 547 "SourceParser.ml"
                 in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_1 : (
# 36 "SourceParser.mly"
      (SourceAst.main)
# 554 "SourceParser.ml"
                )) = _v in
                Obj.magic _1
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (SourceAst.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | BEGIN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FOR ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState43
            | IDENT _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
            | IF ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState43
            | PRINT ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState43
            | WHILE ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState43
            | END ->
                _menhir_reduce17 _menhir_env (Obj.magic _menhir_stack) MenhirState43
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43)
        | EQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | ADD | AND | BEGIN | END | LE | LT | OR | SEMI | SUB | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (SourceAst.expression))), _, (e2 : (SourceAst.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (SourceAst.expression) = 
# 103 "SourceParser.mly"
                                          ( Binop(Sub, e1, e2) )
# 644 "SourceParser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | BEGIN | END | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (SourceAst.expression))), _, (e2 : (SourceAst.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (SourceAst.expression) = 
# 104 "SourceParser.mly"
                                          ( Binop(Neq, e1, e2) )
# 683 "SourceParser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | BEGIN | END | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (SourceAst.expression))), _, (e2 : (SourceAst.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (SourceAst.expression) = 
# 101 "SourceParser.mly"
                                          ( Binop(Or, e1, e2) )
# 714 "SourceParser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | ADD | AND | BEGIN | END | LE | LT | MULT | OR | SEMI | SUB | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (SourceAst.expression))), _, (e2 : (SourceAst.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (SourceAst.expression) = 
# 97 "SourceParser.mly"
                                          ( Binop(Mult, e1, e2) )
# 739 "SourceParser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | BEGIN | END | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (SourceAst.expression))), _, (e2 : (SourceAst.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (SourceAst.expression) = 
# 98 "SourceParser.mly"
                                          ( Binop(Eq, e1, e2) )
# 778 "SourceParser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | BEGIN | END | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (SourceAst.expression))), _, (e2 : (SourceAst.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (SourceAst.expression) = 
# 99 "SourceParser.mly"
                                          ( Binop(Lt, e1, e2) )
# 809 "SourceParser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | ADD | AND | BEGIN | END | LE | LT | OR | SEMI | SUB | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (SourceAst.expression))), _, (e2 : (SourceAst.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (SourceAst.expression) = 
# 96 "SourceParser.mly"
                                          ( Binop(Add, e1, e2) )
# 836 "SourceParser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | BEGIN | END | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (SourceAst.expression))), _, (e2 : (SourceAst.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (SourceAst.expression) = 
# 100 "SourceParser.mly"
                                          ( Binop(Le, e1, e2) )
# 867 "SourceParser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | BEGIN | END | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (SourceAst.expression))), _, (e2 : (SourceAst.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (SourceAst.expression) = 
# 102 "SourceParser.mly"
                                          ( Binop(And, e1, e2) )
# 898 "SourceParser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (SourceAst.expression))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (SourceAst.instruction) = 
# 68 "SourceParser.mly"
                                   ( Print(e) )
# 927 "SourceParser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | EQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | FOR ->
                    _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                | IDENT _v ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
                | IF ->
                    _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                | PRINT ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                | WHILE ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                | END ->
                    _menhir_reduce17 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FOR ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | IDENT _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
            | IF ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | PRINT ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | WHILE ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | END ->
                _menhir_reduce17 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55)
        | SUB ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (loc : (SourceAst.location))), _, (e : (SourceAst.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (SourceAst.instruction) = 
# 67 "SourceParser.mly"
                                      ( Set(loc, e) )
# 1091 "SourceParser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run18 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 13 "SourceParser.mly"
       (bool)
# 1106 "SourceParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (b : (
# 13 "SourceParser.mly"
       (bool)
# 1114 "SourceParser.ml"
    )) = _v in
    let _v : (SourceAst.literal) = 
# 111 "SourceParser.mly"
           ( Bool true )
# 1119 "SourceParser.ml"
     in
    _menhir_goto_literal _menhir_env _menhir_stack _menhir_s _v

and _menhir_run19 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 12 "SourceParser.mly"
       (int)
# 1126 "SourceParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (i : (
# 12 "SourceParser.mly"
       (int)
# 1134 "SourceParser.ml"
    )) = _v in
    let _v : (SourceAst.literal) = 
# 110 "SourceParser.mly"
            ( Int i )
# 1139 "SourceParser.ml"
     in
    _menhir_goto_literal _menhir_env _menhir_stack _menhir_s _v

and _menhir_run21 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 14 "SourceParser.mly"
       (bool)
# 1146 "SourceParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (b : (
# 14 "SourceParser.mly"
       (bool)
# 1154 "SourceParser.ml"
    )) = _v in
    let _v : (SourceAst.literal) = 
# 112 "SourceParser.mly"
            ( Bool false )
# 1159 "SourceParser.ml"
     in
    _menhir_goto_literal _menhir_env _menhir_stack _menhir_s _v

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_reduce17 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (SourceAst.block) = 
# 62 "SourceParser.mly"
                                          ( []                )
# 1173 "SourceParser.ml"
     in
    _menhir_goto_instructions _menhir_env _menhir_stack _menhir_s _v

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | IDENT _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | INTEGER _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | TRUE _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17

and _menhir_run44 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FALSE _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
        | IDENT _v ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
        | INTEGER _v ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
        | TRUE _v ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run48 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | IDENT _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | INTEGER _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | TRUE _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48

and _menhir_run20 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "SourceParser.mly"
       (string)
# 1248 "SourceParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (id : (
# 8 "SourceParser.mly"
       (string)
# 1256 "SourceParser.ml"
    )) = _v in
    let _v : (SourceAst.location) = 
# 116 "SourceParser.mly"
            ( Identifier id )
# 1261 "SourceParser.ml"
     in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState57 | MenhirState53 | MenhirState48 | MenhirState45 | MenhirState41 | MenhirState39 | MenhirState37 | MenhirState35 | MenhirState33 | MenhirState31 | MenhirState29 | MenhirState27 | MenhirState25 | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (loc : (SourceAst.location))) = _menhir_stack in
        let _v : (SourceAst.expression) = 
# 95 "SourceParser.mly"
                                          ( Location(loc) )
# 1272 "SourceParser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState16 | MenhirState43 | MenhirState70 | MenhirState51 | MenhirState65 | MenhirState61 | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
            | IDENT _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
            | INTEGER _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
            | TRUE _v ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run52 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FALSE _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
        | IDENT _v ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
        | INTEGER _v ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
        | TRUE _v ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_var_decls : _menhir_env -> 'ttv_tail -> _menhir_state -> (SourceAst.identifier_info SourceAst.Symb_Tbl.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), (ident : (
# 8 "SourceParser.mly"
       (string)
# 1346 "SourceParser.ml"
        ))), _, (table : (SourceAst.identifier_info SourceAst.Symb_Tbl.t))) = _menhir_stack in
        let _4 = () in
        let t = () in
        let _1 = () in
        let _v : (SourceAst.identifier_info SourceAst.Symb_Tbl.t) = 
# 57 "SourceParser.mly"
  ( let info = {typ=TypInteger; kind=Local} in (Symb_Tbl.add ident info table) )
# 1354 "SourceParser.ml"
         in
        _menhir_goto_var_decls _menhir_env _menhir_stack _menhir_s _v
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), (ident : (
# 8 "SourceParser.mly"
       (string)
# 1363 "SourceParser.ml"
        ))), _, (table : (SourceAst.identifier_info SourceAst.Symb_Tbl.t))) = _menhir_stack in
        let _4 = () in
        let t = () in
        let _1 = () in
        let _v : (SourceAst.identifier_info SourceAst.Symb_Tbl.t) = 
# 55 "SourceParser.mly"
  ( let info = {typ=TypBoolean; kind=Local} in (Symb_Tbl.add ident info table) )
# 1371 "SourceParser.ml"
         in
        _menhir_goto_var_decls _menhir_env _menhir_stack _menhir_s _v
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FOR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | IDENT _v ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
        | IF ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | PRINT ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | WHILE ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | END ->
            _menhir_reduce17 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16)
    | _ ->
        _menhir_fail ()

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_reduce24 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (SourceAst.identifier_info SourceAst.Symb_Tbl.t) = 
# 53 "SourceParser.mly"
                          ( Symb_Tbl.empty )
# 1502 "SourceParser.ml"
     in
    _menhir_goto_var_decls _menhir_env _menhir_stack _menhir_s _v

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMI ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | VAR ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState14
                | END | FOR | IDENT _ | IF | PRINT | WHILE ->
                    _menhir_reduce24 _menhir_env (Obj.magic _menhir_stack) MenhirState14
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | INT ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMI ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | VAR ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState10
                | END | FOR | IDENT _ | IF | PRINT | WHILE ->
                    _menhir_reduce24 _menhir_env (Obj.magic _menhir_stack) MenhirState10
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and main : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 36 "SourceParser.mly"
      (SourceAst.main)
# 1606 "SourceParser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = Obj.magic () in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | MAIN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | INT ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | IDENT _v ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = (_menhir_stack, _v) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | END ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | BEGIN ->
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _menhir_env = _menhir_discard _menhir_env in
                            let _tok = _menhir_env._menhir_token in
                            (match _tok with
                            | VAR ->
                                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState6
                            | END | FOR | IDENT _ | IF | PRINT | WHILE ->
                                _menhir_reduce24 _menhir_env (Obj.magic _menhir_stack) MenhirState6
                            | _ ->
                                assert (not _menhir_env._menhir_error);
                                _menhir_env._menhir_error <- true;
                                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6)
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            let _menhir_stack = Obj.magic _menhir_stack in
                            raise _eRR)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        raise _eRR)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    raise _eRR)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                raise _eRR)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            raise _eRR)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR)

# 219 "/Users/nour/.opam/system/lib/menhir/standard.mly"
  


# 1694 "SourceParser.ml"
