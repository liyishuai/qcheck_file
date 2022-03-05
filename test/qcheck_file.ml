include Qcheck_file
include QCheck2
include QCheck_base_runner
include Filename
include Sys

let tmpdir = concat (get_temp_dir_name ()) "qcheck_file";;

try mkdir tmpdir 0o755 with Sys_error msg -> prerr_endline msg;;
print_endline tmpdir

let testsuite =
  [
    Test.make
      ~print:Print.(triple string string string)
      (Gen.triple genFilename genFilename genFilename)
      (testdir tmpdir);
    Test.make
      ~print:Print.(pair string string)
      (Gen.pair genFilename genFilename)
      (testfile tmpdir);
  ]
;;

run_tests testsuite;;
rmdir tmpdir