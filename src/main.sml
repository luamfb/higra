structure Main = struct
  fun die (msg : string) : 'a =
  let
    val _ = print ("error: " ^ msg ^ "\n")
  in
    OS.Process.exit OS.Process.failure
  end

  fun read_file (filename : string) : string =
  let
    val stream = TextIO.openIn filename
    handle (IO.Io _) =>
      die ("failed to open input file '" ^ filename ^ "'")

    val file_contents = TextIO.input stream
    handle (IO.Io _) =>
      die ("failed to read from file '" ^ filename ^ "'")

    val _ = TextIO.closeIn stream
    handle (IO.Io _) =>
      die ("failed to close input file '" ^ filename ^ "'")
  in
    file_contents
  end

  fun write_to_file (filename : string) (contents : string) : unit =
  let
    val stream = TextIO.openOut filename
    handle (IO.Io _) =>
      die ("failed to open output file '" ^ filename ^ "'")
    val _ = TextIO.output (stream, contents)
    handle (IO.Io _) =>
      die ("failed to write to file '" ^ filename ^ "'")

    val _ = TextIO.closeOut stream
    handle (IO.Io _) =>
      die ("failed to close output file '" ^ filename ^ "'")
  in
    ()
  end

  fun compile (src : string) : (Sema.VertexId * string) list =
  let
    val tokens = Lexer.tokenize src
    handle
    Token.InvalidToken pos =>
      die ("invalid token at " ^ (Debug.dump_pos pos))

    val ast = Parser.parse_figure tokens
    handle
    Ast.SyntaxError loc =>
      die ("syntax error at " ^ (Debug.dump_error_loc loc))

    val sema = Sema.sema_figure ast
    handle
    Sema.SemaError (pos_beg, pos_end) =>
      die ("semantic error from "
      ^ (Debug.dump_pos pos_beg) ^ " to "
      ^ (Debug.dump_pos pos_end))

    val svg_list = Backend.draw_figure sema
  in
    svg_list
  end

  fun write_svgs (dirname : string) ([] : (Sema.VertexId * string) list)
    : unit = ()
    | write_svgs dirname ((id,svg)::rest) =
    let
      val basename = id ^ ".svg"
      val filename = OS.Path.concat (dirname, basename)
      val _ = write_to_file filename svg
      val _ = write_svgs dirname rest
    in
      ()
    end

  fun main (src_file : string) (out_dir : string) : unit =
  let
    val src = read_file src_file
    val _ = write_svgs out_dir (compile src)
  in
    ()
  end

end
