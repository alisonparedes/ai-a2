open Vacroplan

let getTests, addTest = TestRunner.testRunner ()

let make3x4Problem () =
  let dimension1 = 3
  and dimension2 = 4
  and obstacleList = [] in
  initializeProblem dimension1 dimension2 obstacleList

		    
let test1 =
  let test () =
    let problem = make3x4Problem () in
    Array.length problem.blocked = 3 * 4 in
  addTest test "testIn3x4World12PossibleObstacles"


let makeState () =
  let location = 0
    and dirtList = [] in
  initializeState location dirtList 
		  
let test6 =
  let test () =
    let state = makeState ()
    and location = 0 in
    setCurrentLocation state location;
    state.cur = location in
  addTest test "testInitializeCurrentLocationToFirstCell"

let test7 =
  let test () =
    let state = makeState ()
    and location = 0 in
    addDirt state location;
    state.dirt = [location] in
  addTest test "testAddsOneDirtToEmptyList"

let makeDirtyState () =
  let state = makeState () in
  addDirt state 1;
  state
			
	  
let test8 =
  let test () =
    let state = makeDirtyState ()
    and location = 0 in
    let len = List.length state.dirt in
    addDirt state location;
    List.length state.dirt = len + 1 in
  addTest test "testAddsOneDirt"

let makeDirtyAt0To10 () =
  let state = makeState () in
  for i = 0 to 10 do
    addDirt state i
  done;
  state
    
	  
let test9 =
  let test () =
    let state = makeDirtyAt0To10 () in
    isDirty state.dirt 0 in
  addTest test "testFinds0IsDirty"
    
let test10 =
  let test () =
    let state = makeDirtyAt0To10 () in
    isDirty state.dirt 10 in
  addTest test "testFinds10IsDirty"


let test11 =
  let test () =
    let state = makeDirtyAt0To10 () in
    not (isDirty state.dirt 11) in
  addTest test "testFinds11IsNotDirty"

let test12 =
  let test () =
    let state = makeDirtyAt0To10 ()
    and location = 0 in
    let len = List.length state.dirt in
    let new_dirt = clean state.dirt location in
    List.length new_dirt = len - 1 in
  addTest test "testRemovesDirt"

let test12_1 =
  let test () =
    let state = makeDirtyAt0To10 ()
    and location = 0 in
    let len = List.length state.dirt in
    let _ = clean state.dirt location in
    List.length state.dirt = len in
  addTest test "testCopiesDirt"

let test13 =
  let test () =
    let state = makeDirtyAt0To10 ()
    and location = 0 in
    let newDirtList = clean state.dirt location in
    not (isDirty newDirtList  location) in
  addTest test "testNoDirtAtLocationAfterClean"

let aMap () =
  [|'_';
    '#';
    '*';
    '@';
    ':'|]

 let test14 =
  let test () =
    let map = aMap () in
    let dirtList, _, _, _ = extractFeatures map in
    dirtList = [2] in
  addTest test "testDirtIsInPosition2"


 let test15 =
  let test () =
    let map = aMap () in
    let _, robotLocation, _, _ = extractFeatures map in
    robotLocation = 3 in
  addTest test "testRobotIsAtPosition3"

 let test16 =
  let test () =
    let map = aMap () in
    let _, _, rechargerLocation, _ = extractFeatures map in
    rechargerLocation = 4 in
  addTest test "testRechargerIsAtPosition4"

 let test17 =
  let test () =
    let map = aMap () in
    let _, _, _, obstacleList = extractFeatures map in
    obstacleList = [1] in
  addTest test "testObstacleIsAtPosition1"

 let test18 =
   let test () =
     let state = makeState () 
     and problem = make3x4Problem () in
     let actions = applicableActions problem state in
     List.exists (fun a -> a.name = "E") actions in
   addTest test "testActionListContainsW"

 let makeGoWest () =
   {name="W"; operator=goWest; cost=1}
	   
 let test19 =
   let test () =
     let state = makeState () in
     let action = makeGoWest () in
     let nextState = transition state action in
     nextState.cur = state.cur - 1 in
   addTest test "testGoesWest"
	   
 let test20 =
   let test () =
     let state = makeState () in
     let action = makeGoWest () in
     let nextState = transition state action in
     nextState.cur = state.cur - 1 in
   addTest test "testGoesWest"

let makeProblemAndStateForRobotInCenterOf3x3Map () =
  let nrows = 3
  and ncols = 3
  and obstacleList = [] in
  let problem = initializeProblem ncols nrows obstacleList
  and location = 4
  and dirtList = [] in
  let state = initializeState location dirtList in
  problem, state
	   
 let test21 =
   let test () =
     let problem, state = makeProblemAndStateForRobotInCenterOf3x3Map () in
     let actions = applicableActions problem state in
     List.length actions = 4 in (* N, S, E, W*)
   addTest test "testGetAllMoveActions"

let makeProblemAndStateForRobotInUpperLeftCornerOf3x4Map () =
  let nrows = 3
  and ncols = 4
  and obstacleList = [] in
  let problem = initializeProblem ncols nrows obstacleList
  and location = 0
  and dirtList = [] in
  let state = initializeState location dirtList in
  problem, state

let makeProblemAndStateForRobotInUpperRightCornerOf3x4Map () =
  let nrows = 3
  and ncols = 4
  and obstacleList = [] in
  let problem = initializeProblem ncols nrows obstacleList
  and location = 3
  and dirtList = [] in
  let state = initializeState location dirtList in
  problem, state

let makeProblemAndStateForRobotInLowerRightCornerOf3x4Map () =
  let nrows = 3
  and ncols = 4
  and obstacleList = [] in
  let problem = initializeProblem ncols nrows obstacleList
  and location = 11
  and dirtList = [] in
  let state = initializeState location dirtList in
  problem, state

let makeProblemAndStateForRobotInLowerLeftCornerOf3x4Map () =
  let nrows = 3
  and ncols = 4
  and obstacleList = [] in
  let problem = initializeProblem ncols nrows obstacleList
  and location = 8
  and dirtList = [] in
  let state = initializeState location dirtList in
  problem, state
	   
 let test22 =
   let test () =
     let problem, state = makeProblemAndStateForRobotInUpperLeftCornerOf3x4Map () in
     not (isApplicableGoWest problem state) in
   addTest test "testCannotGoWestFromUpperLeft"

 let test23 =
   let test () =
     let problem, state = makeProblemAndStateForRobotInUpperLeftCornerOf3x4Map () in
     isApplicableGoEast problem state in
   addTest test "testCanGoEastFromUpperLeft"

 let test24 =
   let test () =
     let problem, state = makeProblemAndStateForRobotInUpperLeftCornerOf3x4Map () in
     not (isApplicableGoNorth problem state) in
   addTest test "testCannotGoNorthFromUpperLeft"

 let test25 =
   let test () =
     let problem, state = makeProblemAndStateForRobotInUpperLeftCornerOf3x4Map () in
     isApplicableGoSouth problem state in
   addTest test "testCanGoSouthFromUpperLeft"

 (* Test upper right *)
 let test22 =
   let test () =
     let problem, state = makeProblemAndStateForRobotInUpperRightCornerOf3x4Map () in
     isApplicableGoWest problem state in
   addTest test "testCanGoWestFromUpperRight"

 let test23 =
   let test () =
     let problem, state = makeProblemAndStateForRobotInUpperRightCornerOf3x4Map () in
     not (isApplicableGoEast problem state) in
   addTest test "testCannotGoEastFromUpperRight"

 let test24 =
   let test () =
     let problem, state = makeProblemAndStateForRobotInUpperRightCornerOf3x4Map () in
     not (isApplicableGoNorth problem state) in
   addTest test "testCannotGoNorthFromUpperRight"

 let test25 =
   let test () =
     let problem, state = makeProblemAndStateForRobotInUpperRightCornerOf3x4Map () in
     isApplicableGoSouth problem state in
   addTest test "testCanGoSouthFromUpperRight"


 (* TODO: Test lower right *)
 let test22 =
   let test () =
     let problem, state = makeProblemAndStateForRobotInLowerRightCornerOf3x4Map () in
     isApplicableGoWest problem state in
   addTest test "testCanGoWestFromLowerRight"

 let test23 =
   let test () =
     let problem, state = makeProblemAndStateForRobotInLowerRightCornerOf3x4Map () in
     not (isApplicableGoEast problem state) in
   addTest test "testCannotGoEastFromLowerRight"

 let test24 =
   let test () =
     let problem, state = makeProblemAndStateForRobotInLowerRightCornerOf3x4Map () in
     isApplicableGoNorth problem state in
   addTest test "testCanGoNorthFromLowerRight"

 let test25 =
   let test () =
     let problem, state = makeProblemAndStateForRobotInLowerRightCornerOf3x4Map () in
     not (isApplicableGoSouth problem state) in
   addTest test "testCannotGoSouthFromLowerRight"


 (* TODO: Test lower left *)
 let test22 =
   let test () =
     let problem, state = makeProblemAndStateForRobotInLowerLeftCornerOf3x4Map () in
     not (isApplicableGoWest problem state) in
   addTest test "testCannotGoWestFromLowerLeft"

 let test23 =
   let test () =
     let problem, state = makeProblemAndStateForRobotInLowerLeftCornerOf3x4Map () in
     isApplicableGoEast problem state in
   addTest test "testCanGoEastFromLowerLeft"

 let test24 =
   let test () =
     let problem, state = makeProblemAndStateForRobotInLowerLeftCornerOf3x4Map () in
     isApplicableGoNorth problem state in
   addTest test "testCanGoNorthFromLowerLeft"

 let test25 =
   let test () =
     let problem, state = makeProblemAndStateForRobotInLowerLeftCornerOf3x4Map () in
     not (isApplicableGoSouth problem state) in
   addTest test "testCannotGoSouthFromLowerLeft"
     

 let test27 =
   let test () =
     let dirtListA = [1; 3]
     and dirtListB = [2; 3] in
     not (eqSetOfDirt dirtListA dirtListB) in
   addTest test "testDirtListsAreNotEqual"

 let test27 =
   let test () =
     let dirtListA = [1; 3]
     and dirtListB = [1; 3] in
     eqSetOfDirt dirtListA dirtListB in
   addTest test "testDirtListsAreEqual"


 let test27 =
   let test () =
     let dirtListA = [3]
     and dirtListB = [2; 3] in
     not (eqSetOfDirt dirtListA dirtListB) in
   addTest test "testDirtListsHaveUnequalLengths"

 let test27 =
   let test () =
     let dirtListA = []
     and dirtListB = [] in
     eqSetOfDirt dirtListA dirtListB in
   addTest test "testDirtListsAreEquallyEmpty"


 let test27 =
   let test () =
     let dirtListA = []
     and dirtListB = [2; 3] in
     not (eqSetOfDirt dirtListA dirtListB) in
   addTest test "testDirtListsHaveUnequalLengths"

 let test28 =
   let test () =
     let stateA = initializeState 8 []
     and stateB = initializeState 8 [] in
     eqState stateA stateB in
   addTest test "testEqualStates"

 let test29 =
   let test () =
     let stateA = initializeState 8 []
     and stateB = initializeState 7 [] in
     not (eqState stateA stateB) in
   addTest test "testStatesAtDifferentLocations"

 let test30 =
   let test () =
     let stateA = initializeState 8 []
     and stateB = initializeState 8 [9] in
     not (eqState stateA stateB) in
   addTest test "testStatesDifferentDirt"

 let test31 =
   let test () =
     let stateA = initializeState ~charge:2 8 [9]
     and stateB = initializeState ~charge:2 8 [9] in
     eqState ~bat:true stateA stateB in
   addTest test "testStatesEqualIncludingCharge"

 let test32 =
   let test () =
     let stateA = initializeState ~charge:1 8 [9]
     and stateB = initializeState ~charge:2 8 [9] in
     not (eqState ~bat:true stateA stateB) in
   addTest test "testStatesHaveDifferentCharge" 

let makeProblemAndStateForRobotInCleanMap () =
  let nrows = 3
  and ncols = 3
  and obstacleList = [] in
  let problem = initializeProblem ncols nrows obstacleList
  and location = 4
  and dirtList = [] in
  let state = initializeState location dirtList in
  problem, state

let makeProblemAndStateForRobotInDirtyMap () =
  let nrows = 3
  and ncols = 3
  and obstacleList = [] in
  let problem = initializeProblem ncols nrows obstacleList
  and location = 4
  and dirtList = [1] in
  let state = initializeState location dirtList in
  problem, state
	   
 let test33 =
   let test () =
     let problem, cleanState = makeProblemAndStateForRobotInCleanMap () in
     let goalNode = depthFirstSearch ~bat:false ~init:cleanState ~exp:(expand problem) ~goal:isGoal () in
     match goalNode with
       None -> false
     | Some n -> eqState n.state cleanState in
   addTest test "testFindsInitialState"

 let test34 =
   let test () =
     let problem, dirtyState = makeProblemAndStateForRobotInDirtyMap () in
     let goalNode = depthFirstSearch ~bat:false ~init:dirtyState ~exp:(expand problem) ~goal:isGoal () in
     match goalNode with
       None -> false
     | Some n -> isGoal n.state in
   addTest test "testWhenInitialStateIsDirty"


 let test35 =
   let test () =
     let problem, cleanState = makeProblemAndStateForRobotInCleanMap () in
     let goalNode = depthFirstSearch ~bat:false ~init:cleanState ~exp:(expand problem) ~goal:isGoal () in
     match goalNode with
       None -> false
     | Some n -> List.length (recoverNodesOnPath n) = 1 in
   addTest test "testOneNodeOnPathWhenInitialStateIsAGoalState"


 let test36 =
   let test () =
     let problem, cleanState = makeProblemAndStateForRobotInCleanMap () in
     let goalNode = depthFirstSearch ~bat:false ~init:cleanState ~exp:(expand problem) ~goal:isGoal () in
     match goalNode with
       None -> false
     | Some n -> eqState (List.hd (recoverNodesOnPath n)).state cleanState in
   addTest test "testInitialStateOnPathWhenInitialStateIsAGoalState"

 let test39 =
   let test () =
     let state = makeState ()
     and _, neverGen = generated () in
     neverGen state in
   addTest test "testNeverGenerated"

 let test40 =
   let test () =
     let state = makeState () in
     let node = startNode state in
     let addGen, neverGen = generated () in
     addGen node;
     not (neverGen state) in
   addTest test "testAlreadyGenerated"

 let test41 =
   let test () =
     let openHeap = makeEmptyOpenHeap () in
     openHeap.size = 0 in
   addTest test "testEmptyHeapHasSize0" 


 let test42 =
   let test () =
     let openHeap = makeEmptyOpenHeap () in
     match pop openHeap with
       None -> true
     | Some _ -> false in
   addTest test "testPopReturnsNoneIfHeapIsEmpty"

 let makeANode () =
   startNode (makeState ())
	   
 let test43 = 
   let test () =
     let openHeap = makeEmptyOpenHeap () 
     and node = makeANode () in
     let _ = add openHeap node in
     match pop openHeap with
       None -> false
     | Some popped -> popped == node in
   addTest test "testPopsSameNode" 


 let makeABunchOfNodes gList =
   List.map (fun g ->
	      {
		state = makeState ();
		action = "None";
		g = g;
		parent = None;
		h = 0;
		depth = 0
	      })
	    gList
	    
 let makeSearchNodeWithSpecificGAndH ~g:costToCome ~h:estimatedCostToGo =
   {
     state = makeState ();
     action = "None";
     g = costToCome;
     parent = None;
     h = estimatedCostToGo;
     depth = 0;
   }
	    
 let test44 = 
   let test () =
     let openHeap = makeEmptyOpenHeap () in
     List.iter (fun node ->
		let _ = add openHeap node in
		())
	       (makeABunchOfNodes [0; 1; 2; 3; 4; 5; 6; 7; 8; 9]);
     match pop openHeap with
       None -> false
     | Some popped -> popped.g = 0 in 
   addTest test "testsPopsMin"


 let test45 =
   let test () =
     let gList = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9]
     and openHeap = makeEmptyOpenHeap () in
     List.iter (fun node ->
		let _ = add openHeap node in
		())
	       (makeABunchOfNodes gList);
     List.fold_left (fun b g ->
		      match pop openHeap with
			None -> false
		      | Some popped ->
			 popped.g = g && b)
		    true
		    gList in
   addTest test "testsPopsInAscendingOrder"


 let test45 =
   let test () =
     let gList = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9]
     and openHeap = makeEmptyOpenHeap () in
     List.iter (fun node ->
		let _ = add openHeap node in
		())
	       (makeABunchOfNodes (List.rev gList));
     List.fold_left (fun b g ->
		      match pop openHeap with
			None -> false
		      | Some popped ->
			 popped.g = g && b)
		    true
		    gList in
   addTest test "testsPopsInAscendingOrderAddedInReverse"


 let test45 =
   let test () =
     let gList = [3; 2; 3; 1; 3; 2; 0]
     and openHeap = makeEmptyOpenHeap () in
     List.iter (fun node ->
		let _ = add openHeap node in
		())
	       (makeABunchOfNodes gList);
     List.fold_left (fun b g ->
		      match pop openHeap with
			None -> false
		      | Some popped ->
			 popped.g = g && b)
		    true
		    (List.sort (fun a b -> a - b) gList) in
   addTest test "testsPopsInNondecreasingOrder"

 let test46 =
   let test () =
     let node1 = makeSearchNodeWithSpecificGAndH ~g:0 ~h:1
     and node2 = makeSearchNodeWithSpecificGAndH ~g:1 ~h:1 in
     compareNodesByF node1 node2 < 0 in
   addTest test "testsNode1IsLessThanNode2"

 let test46 =
   let test () =
     let node1 = makeSearchNodeWithSpecificGAndH ~g:0 ~h:1
     and node2 = makeSearchNodeWithSpecificGAndH ~g:1 ~h:1 in
     compareNodesByF node2 node1 > 0 in
   addTest test "testsNode2IsGreaterThanNode1"

 let test47 =
   let test () =
     let nodeA = makeSearchNodeWithSpecificGAndH ~g:0 ~h:1
     and nodeB = makeSearchNodeWithSpecificGAndH ~g:1 ~h:0 in
     compareNodesByF nodeA nodeB = 0 in
   addTest test "testsNodeAAndBHaveEqualF"

 let test78 =
   let test () =
     let startNode = Vacroplan.startNode (makeState ()) in
     startNode.h = 0 in
   addTest test "testStartNodeHIsZero"

 let makeGeneratedNode parentNode =
   let action = makeGoWest () 
   and successorState = makeState () in
   generateNode parentNode action successorState

 let makePathOfKNodes parentNode k =
   let action = makeGoWest () in
   let rec gen par i =
     let successorState = makeState () in
     let successorNode = generateNode par action successorState in
     if i = 1 then
       successorNode
     else
       gen successorNode (i - 1) in
   gen parentNode k
	   
 let test79 =
   let test () =
     let parentNode = (makeANode ()) in
     let newNode = makeGeneratedNode parentNode in
     newNode.h = 0 in
   addTest test "testGeneratedNodeHIsZero"

	   
 let test80 =
   let test () =
     let startNode = startNode (makeState ()) in
     startNode.depth = 0 in
   addTest test "testDepthOfStartNodeIsZero"

 let test81 =
   let test () =
     let startNode = startNode (makeState ()) in
     let depth1 = makeGeneratedNode startNode in
     depth1.depth = 1 in
   addTest test "testDepthOfSuccessorStartNodeIsOne"

 let test82 =
   let test () =
     let startNode = startNode (makeState ()) in
     let node1 = makeGeneratedNode startNode
     and node2 = makeGeneratedNode startNode in
     node1.depth = 1 && node2.depth = 1 in
   addTest test "testDepthOfBothSuccessorsStartNodeIsOne"

 let test83 =
   let test () =
     let startNode = startNode (makeState ())
       and numberOfNodesToAddAfterStart = 3 in
     let lastNodeInPath = makePathOfKNodes startNode numberOfNodesToAddAfterStart in
     lastNodeInPath.depth = numberOfNodesToAddAfterStart in
   addTest test "testDepthOfPathOfFourNodesIsThree"

 let test84 =
   let test () =
     let dirtList = [1; 2; 3; 4]
     and problem = make3x4Problem () in
     let edges = makeEdges problem dirtList in
     List.length edges = 6 in
   addTest test "testNumberOfEdges"

 let test85 =
   let test () =
     let x1 = 10
     and x2 = 0
     and y1 = 10
     and y2 = 0 in
     manhatanDistance x1 y1 x2 y2 = 20 in
   addTest test "testManhatanDistance0_0And10_10Is20"

 let test86 =
   let test () =
     let position = 2
     and ncols = 4 in
     calculateX ncols position = position in
   addTest test "testXOf2In4ColIs2"

 let test87 =
   let test () =
     let position = 6
     and ncols = 4 in
     calculateX ncols position = 2 in
   addTest test "testXOf6In4ColIs2"

 let test88 =
   let test () =
     let position = 6
     and ncols = 4 in
     calculateY ncols position = 1 in
   addTest test "testYOf6In3RowIs1"

 let test89 =
   let test () =
     let set = makeSetNode 1 in
     set.set == set in
   addTest test "testNewSetNodeIsItsOwnSet"

 let test90 =
   let test () =
     let problem = make3x4Problem () in
     let addSet, findSet, unionSet = setFinder problem
     and dirtPosition = 1 in
     addSet dirtPosition;
     let newSetNode = findSet dirtPosition in
     newSetNode.data = dirtPosition in
   addTest test "testFindSetFindsAddedSetNode"

 let test90 =
   let test () =
     let problem = make3x4Problem () in
     let addSet, findSet, unionSet = setFinder problem
     and dirtPosition = 1 in
     addSet dirtPosition;
     let newSetNode = findSet dirtPosition in
     newSetNode.set == newSetNode in
   addTest test "testRepresentativeSetIsItself"

 let test91 =
   let test () =
     let problem = make3x4Problem () in
     let addSet, findSet, unionSet = setFinder problem
     and dirtPos1 = 1
     and dirtPos2 = 2 in
     addSet dirtPos1;
     addSet dirtPos2;
     let set1 = findSet dirtPos1
     and set2 = findSet dirtPos2 in
     unionSet set1 set2;
     set1 == findSet dirtPos2 in
   addTest test "testUnionSet2IntoSet1"

 let test92_1 =
   let test () =
     let problem = make3x4Problem () 
     and dirtList = [0; 11] in
     let edgeHeap = makeEdgesBetweenDirt problem dirtList in
     edgeHeap.Fheap.size = 1 in
   addTest test "testEdgeHeapInTwoDirtStateHasOneEdge"

 let test92 =
   let test () =
     let problem = make3x4Problem () 
     and dirtList = [0; 11] in
     let tree = minimumSpanningTree problem dirtList in
     List.length tree = 1 in
   addTest test "testMinSpanTreeInTwoDirtStateHasOneEdge"

 let test92 =
   let test () =
     let problem = make3x4Problem () 
     and dirtList = [0; 11] in
     let tree = minimumSpanningTree problem dirtList in
     List.length tree = 1 in
   addTest test "testMinSpanTreeInTwoDirtStateHasOneEdge"

 let test93 =
   let test () =
     let problem = make3x4Problem () 
     and dirtList = [0; 3; 11] in
     let tree = minimumSpanningTree problem dirtList in
     List.length tree = 2 in
   addTest test "testMinSpanTreeInThreeDirtStateHasTwoEdges"

 let test93 =
   let test () =
     let problem = make3x4Problem () 
     and dirtList = [0; 3; 11] in
     let tree = minimumSpanningTree problem dirtList in
     sumEdgeWeights tree = 5 in
   addTest test "testMinSpanTreeInThreeDirtStateHasTwoEdges"
   
 let test94 =
   let test () =
     let maxCharge = 11
     and curCharge = 0
     and minSpan = 10
     and numberOfDirt = 1 in
     chargesNeeded maxCharge curCharge minSpan numberOfDirt = 1 in
   addTest test "testOneChargeNeeded"

 let test95 =
   let test () =
     let maxCharge = 11
     and curCharge = 11
     and minSpan = 10
     and numberOfDirt = 1 in
     chargesNeeded maxCharge curCharge minSpan numberOfDirt = 0 in
   addTest test "testNoChargeNeeded"

 let test95 =
   let test () =
     let maxCharge = 11
     and curCharge = 11
     and minSpan = 20
     and numberOfDirt = 3 in
     let chargesNeeded = chargesNeeded maxCharge curCharge minSpan numberOfDirt in
     chargesNeeded = 1 in
   addTest test "testOneChargesNeeded"
	   
 let test96 =
   let test () =
     let chargerLoc = 1 
     and edge = {weight=1; u=1; v=2} in
     let edgeList = [edge] in
     match minEdgeAdjacentToCharger chargerLoc edgeList with
       None -> false
     | Some min -> min == edge in
   addTest test "testOnlyEdgeMustBeMinEdge"

 let test96 =
   let test () =
     let chargerLoc = 1 
     and edge1 = {weight=1; u=2; v=2} 
     and edge2 = {weight=2; u=2; v=1} in
     let edgeList = [edge1; edge2] in
     match minEdgeAdjacentToCharger chargerLoc edgeList with
       None -> false
     | Some min -> min == edge2 in
   addTest test "testMinAdjacentEdgeIsMinEdge"
				  
	   
let main =
  TestRunner.runTests (getTests ())
