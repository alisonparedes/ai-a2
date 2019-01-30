open Fheap

let getTests, addTest = TestRunner.testRunner ()

let makeArrayOfDummyNodes () =
  Vacroplan.makeArrayOfDummyNodes 1
					      
let test41 =
   let test () =
     let openHeap = makeEmptyOpenHeap (makeArrayOfDummyNodes ()) in
     openHeap.size = 0 in
   addTest test "testEmptyHeapHasSize0" 


 let test42 =
   let test () =
     let openHeap = makeEmptyOpenHeap (makeArrayOfDummyNodes ()) in
     match pop openHeap Vacroplan.compareNodesByF with
       None -> true
     | Some _ -> false in
   addTest test "testPopReturnsNoneIfHeapIsEmpty"

let makeState () =
  let location = 0
    and dirtList = [] in
  Vacroplan.initializeState location dirtList 
	   
 let makeANode () =
   Vacroplan.startNode (makeState ())
	   
 let test43 = 
   let test () =
     let openHeap = makeEmptyOpenHeap (makeArrayOfDummyNodes ()) 
     and node = makeANode () in
     let _ = add openHeap node Vacroplan.makeArrayOfDummyNodes Vacroplan.compareNodesByF in
     match pop openHeap Vacroplan.compareNodesByF with
       None -> false
     | Some popped -> popped == node in
   addTest test "testPopsSameNode" 


 let makeABunchOfNodes gList =
   List.map (fun g ->
	      {
		Vacroplan.state = makeState ();
		Vacroplan.action = "None";
		Vacroplan.g = g;
		Vacroplan.parent = None;
		Vacroplan.h = 0;
	      })
	    gList
	    
 let test44 = 
   let test () =
     let openHeap = makeEmptyOpenHeap (makeArrayOfDummyNodes ()) in
     List.iter (fun node ->
		let _ = add openHeap node Vacroplan.makeArrayOfDummyNodes Vacroplan.compareNodesByF in
		())
	       (makeABunchOfNodes [0; 1; 2; 3; 4; 5; 6; 7; 8; 9]);
     match pop openHeap Vacroplan.compareNodesByF with
       None -> false
     | Some popped -> popped.Vacroplan.g = 0 in 
   addTest test "testsPopsMin"


 let test45 =
   let test () =
     let gList = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9]
     and openHeap = makeEmptyOpenHeap (makeArrayOfDummyNodes ()) in
     List.iter (fun node ->
		let _ = add openHeap node Vacroplan.makeArrayOfDummyNodes Vacroplan.compareNodesByF in
		())
	       (makeABunchOfNodes gList);
     List.fold_left (fun b g ->
		      match pop openHeap Vacroplan.compareNodesByF with
			None -> false
		      | Some popped ->
			 popped.Vacroplan.g = g && b)
		    true
		    gList in
   addTest test "testsPopsInAscendingOrder"


 let test45 =
   let test () =
     let gList = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9]
     and openHeap = makeEmptyOpenHeap (makeArrayOfDummyNodes ()) in
     List.iter (fun node ->
		let _ = add openHeap node Vacroplan.makeArrayOfDummyNodes Vacroplan.compareNodesByF in
		())
	       (makeABunchOfNodes (List.rev gList));
     List.fold_left (fun b g ->
		      match pop openHeap Vacroplan.compareNodesByF with
			None -> false
		      | Some popped ->
			 popped.Vacroplan.g = g && b)
		    true
		    gList in
   addTest test "testsPopsInAscendingOrderAddedInReverse"

 let main =
  TestRunner.runTests (getTests ())
