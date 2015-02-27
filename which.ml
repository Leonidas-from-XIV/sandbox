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
  let exe_name = Sys.argv.(1) in
  (match Sys.getenv "PATH" with
    | Some paths -> String.split paths ~on:':'
    | None -> [])
  |> find_paths exe_name
  |> List.iter ~f:print_endline

let () = main ()
