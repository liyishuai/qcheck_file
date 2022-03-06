include Qcheck_file
include QCheck2
include QCheck_base_runner
include Filename
include Sys

let tmpdir = concat (get_temp_dir_name ()) "qcheck_file";;

FileUtil.mkdir tmpdir;;
print_endline tmpdir

let testsuite =
  [
    (* Test.make ~name:"testdir"
      ~print:Print.(triple string string string)
      (Gen.triple genFilename genFilename genFilename)
      (testdir tmpdir);
    Test.make ~name:"testfile"
      ~print:Print.(pair string string)
      (Gen.pair genFilename genFilename)
      (testfile tmpdir); *)
    Test.make ~name:"testunison"
      ~print:Print.(quad string string string (pair string string))
      Gen.(quad genFilename genFilename2 genFilename (pair genFilename genFilename))
      (testunison tmpdir);
  ]
;;

run_tests ~long:true testsuite
