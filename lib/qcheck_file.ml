include QCheck2
include Filename
include Sys

let genUpper = Gen.char_range 'A' 'Z'
let genLower = Gen.char_range 'a' 'z'
let genFilechar = Gen.(oneof [ genLower; genUpper; numeral; pure '_' ])
let genFilename = Gen.(string_size ~gen:genFilechar (int_range 1 10))
let genFilename2 = Gen.(string_size ~gen:genFilechar (int_range 2 10))

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

let testunison base (a, b, file, (content1, content2)) =
  let adir = concat base a in
  let bdir = concat base b in
  let unison =
    "unison " ^ adir ^ " " ^ bdir
    ^ " -batch -confirmbigdel=false > logs.txt 2> error.txt"
  in
  try
    FileUtil.rm ~force:Force ~recurse:true [ adir; bdir ];
    FileUtil.mkdir adir;
    FileUtil.mkdir bdir;
    if command unison != 0 then (
      print_endline ("0th unison failed: " ^ unison);
      false)
    else
      let apath = concat adir file in
      let oc1 = open_out apath in
      output_string oc1 content1;
      close_out oc1;
      if command unison != 0 then (
        print_endline "1st unison failed";
        false)
      else (
        let bpath = concat bdir file in
        let oc2 = open_out bpath in
        output_string oc2 content2;
        close_out oc2;
        remove apath;
        let ret = command unison in
        if ret != 1 then (
          print_endline ("2nd unison returned " ^ Print.int ret);
          false)
        else true)
  with e ->
    print_endline (Printexc.to_string e);
    false