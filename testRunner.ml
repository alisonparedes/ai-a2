(** A very simple test runner *)

type test = {
  f : (unit -> bool);
  name : string
}

let testRunner () =
    let testList = ref [] in
    let getTestList () =
      !testList in
    let addTest test name =
      testList := {f=test; name=name} :: !testList in
    getTestList
      , addTest

	  
let runTests tests =
  List.iter (fun test ->
	       let result = test.f () in
	       if not result then
		 Printf.fprintf stdout "%s failed\n%!" test.name)
	      tests;
  Printf.fprintf stdout "Done\n%!"	       
