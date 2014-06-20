module Parser = BuildOCPParser


let read_process command =
  ignore(Unix.system ("echo " ^command));
  let buffer_size = 2048 in
  let buffer = Buffer.create buffer_size in
  let string = String.create buffer_size in
  let in_channel = Unix.open_process_in command in
  let chars_read = ref 1 in
  while !chars_read <> 0 do
    chars_read := input in_channel string 0 buffer_size;
    Buffer.add_substring buffer string 0 !chars_read
  done;
  ignore (Unix.close_process_in in_channel);
  Buffer.contents buffer


let ocp_name = ref "" 
let target = ref "ocp2opam_package"
let version = ref (let open Unix in 
                   let tm = gmtime (gettimeofday ()) in
                   Printf.sprintf "%i%s%i" (1900+ tm.tm_year) 
                     (if tm.tm_mon < 9 then
                        Printf.sprintf "0%i" (tm.tm_mon +1)
                      else
                        string_of_int tm.tm_mon)
                        tm.tm_mday)
    (*(read_process "date +%Y%m%d")*)
let name = ref ""
let url = ref ""
let keep_version = ref false

let main () =
  let ocp_arg  = ("-ocp", Arg.String (fun s -> ocp_name := s), 
                  "name of .ocp file in current directory") 
  and target_arg  = ("-target", Arg.String (fun s -> target := s), 
                  "target folder (default \"ocp2opam_packages\")") 

  and url_arg  = ("-url", Arg.String (fun s -> url := s), 
                  "url of repository (default \"\")") 

  and version_arg  = ("-version", Arg.String (fun s -> url := s), 
                  "version (default date yyyymmdd)") 

  and keep_version_arg  = ("-keep", Arg.Bool (fun b -> keep_version := b), 
                  "keep your .ocp numbered version (default false)") 

  in
  Arg.parse [ocp_arg; target_arg;url_arg; version_arg; keep_version_arg] (fun s -> ()) "";
  if !ocp_name = "" then
    (
      print_endline (Arg.usage_string [ocp_arg; target_arg;url_arg; version_arg; keep_version_arg]  "");
      exit 0
    );
  try 
    let ocp_channel =  open_in !ocp_name in
    Printf.printf "Parsing %s\n%!" !ocp_name;
    Printf.printf "Will write into  %s\n%!" !version
  with
  | _ ->
    (Printf.eprintf "%s not found\n%!" !ocp_name;
     exit 1)
;;


main ();;


type package = {
  mutable package_type : string;
  mutable package_name : string;
  mutable requires : BuildOCPTree.string_with_attributes list;
  mutable authors : string list;
  mutable descr: string list;
  mutable license : string list;
}


let run command = 
  print_endline command;
  ignore(Unix.system command)


let get_package package =
  read_process ("ocamlfind list | grep \"^"^package^"[ ]*(\"")

let get_package_version package =
  let base = get_package package in
  let s =  (Str.split_delim (Str.regexp "  +") base) in
  match s with
  |  [] -> ""
  | _ ->
    let r = List.hd (Str.split_delim (Str.regexp ")") (List.nth s (List.length s - 1))) in
    List.fold_left (fun a b -> a ^b) "" (Str.split_delim (Str.regexp "(version: ") r) 

  
let to_path l = 
  List.fold_left (fun a b -> a ^ Filename.dir_sep ^ b) "" l

let _ =
  let stmts = 
    BuildOCPParse.read_ocamlconf !ocp_name in
  let authors = ref [] 
  and dirname = ref []
  and descr = ref []
  and packages = ref []
  and requires = ref []
  and license = ref [] in

  let open BuildOCPTree in
  let rec parse_option = function
    | OptionListSet ("dirname" , l) -> 
      dirname := !dirname@l
    | OptionListSet ("authors" , l) -> 
      authors := !authors@l
    | OptionListSet ("descr" , l) -> 
      descr := !descr@l
    | OptionListSet ("license" , l) -> 
      license := !license@l
   | _ -> ()
  and parse_package pt n l = 
    let pkg = {
      package_type = BuildOCPTree.string_of_package_type pt;
      package_name = n;
      requires = [];
      authors = !authors;
      descr = !descr;
      license = !license;      
    } in
    parse_statements (Some pkg )l;
    packages := pkg:: !packages
    
  and parse_statements pkg = function
    | [] -> ()
    | StmtOption l::q -> parse_option l; parse_statements pkg q
    | StmtDefinePackage (pt,n,l) :: q-> parse_package pt n l; parse_statements pkg q
    | StmtRequiresSet l :: q -> 
      ( match pkg with 
        | None -> requires := l 
        | Some pkg -> pkg.requires <- l); parse_statements pkg q
    | t::q -> parse_statements pkg q
  in
  parse_statements None stmts;
  let pwd = Sys.getcwd () in
  if not !keep_version then
    run ("sed -i \"1iversion = [\\\""^ !version ^ "\\\"]\" " ^ !ocp_name);
  
  List.iter (fun p ->
      print_endline ("preparing opam package : "^p.package_name);
      let package_name = p.package_name ^ "-" ^
                        !version ^ ".tar.gz"
                         
      and md5sum = ref "" in

      Sys.chdir pwd; Sys.chdir (List.hd !dirname);
      let project_dir = (Sys.getcwd ()) in
      ignore (Unix.system "ocp-build clean");
      let package_dir = to_path [pwd; !target;"packages";p.package_name;p.package_name ^ "." ^ !version]  
      and archive_dir = to_path [pwd; !target; "archives"] in
      let package_path = archive_dir ^ Filename.dir_sep ^ p.package_name ^ "-" ^ !version ^ ".tar.gz" in
      run ("mkdir -p " ^ package_dir);
      run ("mkdir -p " ^ archive_dir);
      let command = "tar --exclude=_obuild --exclude=ocp-build.root* --exclude=.git --exclude=" ^ 
                    p.package_name ^"*"^".tar.gz " ^ 
                    " -czf " ^ package_path ^ " ."  in
      run command;
      md5sum := List.hd (Str.split_delim (Str.regexp " +") (read_process ("md5sum "^package_path)));
      let opam_channel = open_out (to_path [package_dir;"opam"]) in
      let descr_channel = open_out (to_path [package_dir;"descr"]) in
      let url_channel = open_out (to_path [package_dir;"url"]) in
      output_string opam_channel (Printf.sprintf "opam-version: \"1\" \n");
      output_string opam_channel (Printf.sprintf "maintainer: \"%s\" \n" 
        (let s = ref "" in
         List.iter (fun a -> s := !s ^ (Printf.sprintf "%s " a)) p.authors; 
         !s));
      output_string opam_channel ("build: [\n"^
                     "\t[make \"build\"]\n"^ 
                     "\t[make \"install\"]\n"^
                     "]\n");
      output_string opam_channel ("remove: [\n"^
                     "\t[make \"uninstall\"]\n"^
                     "]\n");
      output_string opam_channel (Printf.sprintf"depends: [ %s ] \n" 
        (let s = ref "" in
         List.iter (fun r -> 
             s := !s ^ 
                        (let v = get_package_version (fst r) in
                         if v = "[distributed with Ocaml]"  then
                           "" else
                           if  v = "" then
                             (Printf.sprintf "\"%s\" " (fst r))
                           else
                           (Printf.sprintf "\"%s\" {>= \"%s\"}" (fst r) v)
                       )
           ) p.requires; 
         !s));
      
      List.iter (fun s -> output_string descr_channel (Printf.sprintf "%s. " s)) !descr;
      output_string descr_channel "\nOpam package generated by ocp2opam\n";
      output_string descr_channel ((String.capitalize p.package_type)^" :  "^p.package_name^"\n");
      output_string descr_channel ("by :\t"^
                                   (let rec aux = function
                                      | [] -> "Someone unknown"
                                      | t::[] -> t
                                      | t::q -> t^ "\nand \t"^ aux q
                                    in aux p.authors)^"\n");


      output_string url_channel ("archive: \""^ !url ^"/"^ !version ^"/" ^package_name^"\"\n");
      output_string url_channel ("checksum: \""^ !md5sum ^ "\"\n");
      close_out opam_channel;
      close_out descr_channel;
      close_out url_channel;
      if not !keep_version then
        run ("sed -i 1d "^ !ocp_name ); 
      
    ) 
    (List.rev !packages);

;;
  
