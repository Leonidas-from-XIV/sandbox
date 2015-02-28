(*
 * corebuild -pkg async which.native
 *)

open Core.Std
open Async.Std

let find_paths name paths =
  Pipe.of_list paths
  |> Pipe.filter_map' ~f:(fun path ->
    let path = path ^ "/" ^ name in
    Sys.file_exists path >>= function
    | `Yes -> return @@ Some path
    | _ -> return None)

let main () =
  let spec = Command.Spec.(
    empty
    +> flag "--all" no_arg
      ~doc:"Print all matches in PATH, not just the first"
      ~aliases:["-a"]
    +> anon ("command" %: string)) in
  Command.async_basic
    ~summary:"Write the full path of COMMAND(s) to standard output."
    spec
    (fun all exe_name () ->
      (match Sys.getenv "PATH" with
      | Some paths -> String.split paths ~on:':'
      | None -> [])
      |> find_paths exe_name
      |> (fun p -> if all then return p else
        (Pipe.read p >>= function
          | `Ok v -> return @@ Pipe.of_list [v]
          | _ -> return @@ Pipe.of_list []))
      >>= Pipe.iter ~f:(fun e -> return @@ print_endline e))
  |> Command.run

let () = main ()
