(*
 * corebuild which.native
 *)

open Core.Std

let find_paths name paths =
  Sequence.of_list paths
  |> Sequence.filter_map ~f:(fun path ->
    let path = path ^ "/" ^ name in
    match Sys.file_exists path with
    | `Yes -> Some path
    | _ -> None)

let main () =
  let spec = Command.Spec.(
    empty
    +> flag "--all" no_arg
      ~doc:"Print all matches in PATH, not just the first"
      ~aliases:["-a"]
    +> anon ("command" %: string)) in
  Command.basic
    ~summary:"Write the full path of COMMAND(s) to standard output."
    spec
    (fun all exe_name () ->
      (match Sys.getenv "PATH" with
      | Some paths -> String.split paths ~on:':'
      | None -> [])
      |> find_paths exe_name
      |> (fun p -> if all then p else
        match Sequence.next p with
        | Some (e, _) -> Sequence.of_list [e]
        | None -> Sequence.empty)
      |> Sequence.iter ~f:print_endline)
  |> Command.run

let () = main ()
