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
  let spec =
    let open Command.Spec in
    empty
    +> anon ("command" %: string) in
  Command.basic
    ~summary:"Locate command"
    spec
    (fun exe_name () ->
      (match Sys.getenv "PATH" with
      | Some paths -> String.split paths ~on:':'
      | None -> [])
      |> find_paths exe_name
      |> List.iter ~f:print_endline)
  |> Command.run

let () = main ()
