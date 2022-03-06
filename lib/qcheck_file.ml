include QCheck2
include Filename
include Sys

let genUpper = Gen.char_range 'A' 'Z'
let genLower = Gen.char_range 'a' 'z'
let genFilechar = Gen.(oneof [ genUpper; genLower; numeral; pure '_' ])
let genFilename = Gen.(string_size ~gen:genFilechar (int_range 1 10))

let testdir base (dir, file, content) =
  let dirpath = concat base dir in
  let filepath = concat dirpath file in
  try
    mkdir dirpath 0o755;
    let oc = open_out filepath in
    output_string oc content;
    close_out oc;
    let ic = open_in filepath in
    let s = input_line ic in
    close_in ic;
    remove filepath;
    rmdir dirpath;
    s = content
  with Sys_error msg ->
    prerr_endline msg;
    false

let testfile base (file, content) =
  let filepath = concat base file in
  try
    let oc = open_out filepath in
    output_string oc content;
    close_out oc;
    let ic = open_in filepath in
    let s = input_line ic in
    close_in ic;
    remove filepath;
    s = content
  with Sys_error msg ->
    prerr_endline msg;
    false

let testunison base (a, b, dir) =
  let apath = concat base a in
  let bpath = concat base b in
  let unison =
    "unison " ^ apath ^ " " ^ bpath
    ^ " -batch -confirmbigdel=false > logs.txt 2> error.txt"
  in
  try
    FileUtil.rm ~force:Force ~recurse:true [ apath; bpath ];
    FileUtil.mkdir apath;
    FileUtil.mkdir bpath;
    if command unison != 0 then (
      print_endline "first unison failed";
      false)
    else
      let adir = concat apath dir in
      let bdir = concat bpath dir in
      mkdir adir 0x755;
      if command unison != 0 then (
        print_endline "second unison failed";
        false)
      else (
        prerr_endline (a ^ " " ^ b ^ " " ^ dir);
        try
          mkdir bdir 0o755;
          print_endline "unexpected success";
          false
        with _ ->
          print_endline "as expected";
          true)
  with e ->
    print_endline (Printexc.to_string e);
    false