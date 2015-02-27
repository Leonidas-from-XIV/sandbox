(*
 * corebuild which.native
 *)

open Core.Std

let find_paths name paths =
  List.map paths (fun p -> p ^ "/" ^ name) |>
  List.filter ~f:(fun path ->
    match Sys.file_exists path with
    | `Yes -> true
    | _ -> false)

let main () =
  let spec = Command.Spec.(
    empty
    +> flag "-a" no_arg ~doc:"Foo"
    +> anon ("command" %: string)) in
  Command.basic
    ~summary:"Locate command"
    spec
    (fun all exe_name () ->
      (match Sys.getenv "PATH" with
      | Some paths -> String.split paths ~on:':'
      | None -> [])
      |> find_paths exe_name
      |> (function
        | x::_ when not all -> [x]
        | x -> x)
      |> List.iter ~f:print_endline)
  |> Command.run

let () = main ()
