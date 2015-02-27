(*
 * corebuild which.native
 *)

open Core.Std


let find_path name paths =
  List.find_map paths (fun path ->
    let p = path ^ "/" ^ name in
    match Sys.file_exists p with
    | `Yes -> Some p
    | _ -> None)

let main () =
  let exe_name = Sys.argv.(1) in
  let paths = match Sys.getenv "PATH" with
  | Some paths -> String.split paths ~on:':'
  | None -> [] in
  match find_path exe_name paths with
  | Some p -> print_endline p
  | None -> ()

let () = main ()
