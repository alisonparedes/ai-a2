(** Vacroplan
By Alison Paredes
An implementation of Vacuum Robot Planner *)

type state = {
  mutable cur : int ;
  mutable dirt : int list ;
  mutable charge : int
}

type action = {
  name : string ;
  operator : state -> state ;
  cost : int 
}
	       
type searchNode = {
  state : state ;
  action : string ;
  g : int ;
  parent : searchNode option;
  h : int ;
  depth : int
}

type problem = {
  blocked : bool array;
  recharger : int;
  charge : int;
  nrows : int;
  ncols : int
}

let calculateF node =
  node.g + node.h
		 
let compareNodesByF nodeA nodeB =
  (calculateF nodeA) - (calculateF nodeB)

let compareNodesByHOnly nodeA nodeB =
  nodeA.h - nodeB.h
			 
let printStateToStdout state =
  Printf.fprintf stdout "loc:%d %!" state.cur;
  List.iter (fun dirt ->
	     Printf.fprintf stdout "d:%d %!" dirt)
	    state.dirt;
  Printf.fprintf stdout "ch:%d %!" state.charge

(* Assumes two dirt lists if they were both sorted *)
let eqSetOfDirt dirtListA dirtListB =
    let rec comp listA listB =
      match listA, listB with
	[], [] -> true
      | [], _ -> false
      | _, [] -> false
      | hdA :: tlA, hdB :: tlB ->
	 if hdA != hdB then
	   false
	 else comp tlA tlB in
    comp dirtListA dirtListB

let eqState ?(bat:bool=false) stateA stateB =
  stateA.cur = stateB.cur
  && eqSetOfDirt stateA.dirt stateB.dirt
  && (not bat
     || (bat && stateA.charge = stateB.charge))
		 

let h1 state =
  List.length state.dirt

type edge = {
  weight : int;
  u : int;
  v : int
}

let manhatanDistance x1 y1 x2 y2 =
  abs (x1 - x2) + abs (y1 - y2)

let calculateX ncols position =
  position mod ncols

let calculateY ncols position =
  position / ncols

let distanceBetweenPositions problem u v =
  let u_x = calculateX problem.ncols u
  and u_y = calculateY problem.ncols u
  and v_x = calculateX problem.ncols v
  and v_y = calculateY problem.ncols v in
  manhatanDistance u_x u_y v_x v_y

	      
let makeEdges problem dirtList =
  let edges = ref [] in
  let rec edge u dirtListTail =
    match dirtListTail with
      [] -> ()
    | hd :: tl ->  List.iter (fun v ->
			      let newEdge = {
			       weight=distanceBetweenPositions problem u v;
			       u=u;
			       v=v } in
			     edges := newEdge :: !edges)
			    dirtListTail;
		   edge hd tl in
  match dirtList with
    [] -> !edges
  | hd :: tl -> edge hd tl;
		!edges
   
type 'a setNode = {
  data : 'a;
  mutable set : 'a setNode
}

let makeSetNode data =
  let rec setNode =
  {
    data=data;
    set=setNode
  } in
  setNode

let setFinder problem  =
  let dummySetNode = makeSetNode (-1) in
  let perfectHash = Array.make (problem.nrows * problem.ncols) dummySetNode in
  let addSet dirtPosition =
    let newSetNode = makeSetNode dirtPosition in
    perfectHash.(dirtPosition) <- newSetNode
  and findSet dirtPosition =
    let rec find setNode =
      if setNode.set == setNode then
	setNode
      else
	find setNode.set in
    find perfectHash.(dirtPosition)
  and unionSet setA setB =
    setB.set <- setA in
  addSet, findSet, unionSet

let makeArrayOfDummyEdges size =
  let edge = {weight=(-1);
	      u=(-1);
	      v=(-1)} in
  Array.make size edge

let compareEdgesByWeight edge1 edge2 =
  edge1.weight - edge2.weight

		   
let makeEdgesBetweenDirt problem dirtList =
  let edgeHeap = Fheap.makeEmptyOpenHeap (makeArrayOfDummyEdges 1)  (* Actually a generic heap, and not just for open nodes :D *)
  and edges = makeEdges problem dirtList in
  List.iter (fun edge -> let _ = Fheap.add edgeHeap edge makeArrayOfDummyEdges compareEdgesByWeight in () ) edges;
  edgeHeap

    
	     
let minimumSpanningTree problem dirtList =
  let edges = ref []
  and edgeHeap = makeEdgesBetweenDirt problem dirtList 
  and addSet, findSet, unionSet = setFinder problem in
  List.iter (fun dirt -> addSet dirt) dirtList;
  let rec addLightEdge ()  =
    match Fheap.pop edgeHeap compareEdgesByWeight with
      None -> ()
    | Some edge -> let uSet = findSet edge.u
		   and vSet = findSet edge.v in
		   if not (uSet == vSet) then (
		     edges := edge :: !edges;
		     unionSet uSet vSet;
		   );
		   addLightEdge () in
  addLightEdge ();
  !edges 

   
let sumEdgeWeights edgeList =
  List.fold_left (fun a b -> b.weight + a) 0 edgeList
   
let h2 problem state =
  sumEdgeWeights (minimumSpanningTree problem state.dirt)

let chargesNeeded maxCharge curCharge minSpanWithoutCharger numberOfDirt =
  (minSpanWithoutCharger + numberOfDirt - curCharge) / maxCharge

let minEdgeAdjacentToCharger chargerLoc edgeList =
  let edgeHeap = Fheap.makeEmptyOpenHeap (makeArrayOfDummyEdges 1) in
  List.iter (fun edge ->
	     if edge.u = chargerLoc
		|| edge.v = chargerLoc then
	       let _ = Fheap.add edgeHeap edge makeArrayOfDummyEdges compareEdgesByWeight in ())
	    edgeList;
  Fheap.pop edgeHeap compareEdgesByWeight
	    
let h3 problem state =
  let spanNoCharger = sumEdgeWeights (minimumSpanningTree problem (state.cur :: state.dirt)) in
  let chargesNeeded = chargesNeeded problem.charge state.charge spanNoCharger (List.length state.dirt) in
  h2 problem state + chargesNeeded
	    
let isGoal state =
  state.dirt = []

let costToCome parentNode actionCost =
  parentNode.g + actionCost

let h0 state =
  0
		   
let generateNode ?(h:(state -> int)=h0) parentNode action state  =
  {
    state=state;
    action=action.name;
    g=costToCome parentNode action.cost;
    parent=Some parentNode;
    h=h state; 
    depth= parentNode.depth + 1
  }

    
let startNode ?(h:(state -> int)=h0) state =
  {
    state=state;
    action="None";
    g=0;
    parent=None;
    h=h state; 
    depth=0;
  }

let counter () =
  let nodesGenerated = ref 0
  and nodesExpanded = ref 0 in
  let getNodesGenerated () = !nodesGenerated
  and getNodesExpanded () = !nodesExpanded
  and countAsGenerated () =
    nodesGenerated := 1 + !nodesGenerated;
			    
  and countAsExpanded () = nodesExpanded := 1 + !nodesExpanded in
  getNodesGenerated,
  getNodesExpanded,
  countAsGenerated,
  countAsExpanded

let recoverNodesOnPath node =
  let rec recover node path =
    let path = node :: path in
    match node.parent with
      None -> path
    | Some p -> recover p path in
  recover node []

let getActionNamesFromStartToEndNode nodePath = (* Assumes last node added to path list was initial state *)
  List.map (fun node -> node.action) nodePath

let noCycle ?(bat:bool=false) state pathHead =
  let rec traverse node =
    if eqState ~bat node.state state then
      false
    else
      match node.parent with
	None -> true
      | Some p -> traverse p in
  traverse pathHead

let getGen, getExp, countGen, countExp = counter ()
	   
let depthFirstSearch  ?(bat:bool=false) ~init:initialState ~exp:expand ~goal:isGoal ?(id:bool=false) ?(depth:int=0) () =
  countGen ();
  let start = startNode initialState
  and endNode = ref None in
  let stack = ref (start :: []) in
  let rec search () =
    match !stack with
      [] -> ()
    | node :: tl ->
       stack := tl;
       countExp ();
       if isGoal node.state then
	 endNode := Some node
       else 
	 let rec gen states =
	   match states with
	     [] -> search ()
	   | hd :: tl -> let action, successorState = hd in
			 if noCycle ~bat successorState node
			    && (not id (* Search depth is unbounded *)
				|| (id && node.depth < depth)) then ((* To support iterative deepening *)
			   countGen ();
			   let newNode = (generateNode node action successorState) in
			   stack := newNode :: !stack);
			 gen tl in
	 gen (expand node.state) in
  search ();
  !endNode

let iterativeDeepeningDepthFirstSearch ?(bat:bool=false) ~init:initialState ~exp:expand ~goal:isGoal ?(log:bool=false) () =
  let rec search depth =
    match depthFirstSearch ~bat:bat ~id:true ~depth:depth ~init:initialState ~exp:expand ~goal:isGoal () with
      None->
      if log then
	Printf.fprintf stdout "gen:%d exp:%d\n%!" (getGen ()) (getExp());
      search (depth + 1)
    | goalNode -> goalNode in
  search 0

let makeArrayOfDummyNodes length =
  let state = {cur=(-1);
	       dirt=[-1];
	       charge=(-1)} in
  let node = {state=state;
	      action="None";
	      g=(-1);
	      parent=None;
	      h=(-1);
	      depth=(-1)} in
  Array.make length node

let generated ?(bat:bool=false) () =
  let nodes = Hashtbl.create 1 in
  let add node = Hashtbl.add nodes node.state node
  and neverGenerated state = 
    try
      let nodeList = Hashtbl.find_all nodes state in
      let rec find list =
	match list with
	  [] -> true
	| hd :: tl -> if eqState ~bat:bat state hd.state then
			false
		      else
			find tl in
      find nodeList
    with Not_found ->
      true in
  add, neverGenerated

	 
let greedySearch ~init:initialState ~exp:expand ~goal:isGoal ?(log:bool=false) ?(bat:bool=false) () =
  countGen ();
  let start = startNode initialState
  and endNode = ref None in
  let openHeap = Fheap.makeEmptyOpenHeap (makeArrayOfDummyNodes 1) (* Actually a generic heap *)
  and addGen, neverGen = generated ~bat () in
  let _ = Fheap.add openHeap start makeArrayOfDummyNodes compareNodesByHOnly in
  addGen start;
  countGen ();
  let rec search () =
    match Fheap.pop openHeap compareNodesByHOnly with
      None -> ()
    | Some node -> countExp ();
		   if isGoal node.state then
		     endNode := Some node
		   else
		     let rec gen states =
		       match states with
			 [] -> search ()
		       | hd :: tl -> let action, successorState = hd in
				     if neverGen successorState then (
				       countGen ();
				       let newNode = (generateNode node action successorState) in
				       addGen newNode;
				       let _ = Fheap.add openHeap newNode makeArrayOfDummyNodes compareNodesByHOnly in ());
				     gen tl in
		     gen (expand node.state) in
  search ();
  !endNode
					 

	 
let astarSearch ?(bat:bool=false) ~init:initialState ~exp:expand ~goal:isGoal ~h:h ?(id:bool=false) ?(depth:int=0) () =
  countGen ();
  let start = startNode ~h:h initialState
  and endNode = ref None in
  let openHeap = Fheap.makeEmptyOpenHeap (makeArrayOfDummyNodes 1) 
  and addGen, neverGen = generated ~bat () in
  let _ = Fheap.add openHeap start makeArrayOfDummyNodes compareNodesByF in
  addGen start;
  let rec search () =
    match Fheap.pop openHeap compareNodesByF with
      None -> ()
    | Some node -> countExp ();
		   if isGoal node.state then
		     endNode := Some node
		   else
		     let rec gen states =
		       match states with
			 [] -> search ()
		       | hd :: tl -> let action, successorState = hd in
				     if neverGen successorState
					&& (not id
					    || id && node.depth < depth) then ( (* To support iterative deepening astar *)
				       countGen ();
				       let newNode = (generateNode ~h:h node action successorState) in
				       addGen newNode;
				       let _ = Fheap.add openHeap newNode makeArrayOfDummyNodes compareNodesByF in ());
				     gen tl in
		     gen (expand node.state) in
  search ();
  !endNode


let iterativeDeepeningAstarSearch  ~init:initialState ~exp:expand ~goal:isGoal ~h:h ?(bat:bool=false) ?(log:bool=false) () =
  let rec search depth =
    match astarSearch ~bat:bat ~init:initialState ~exp:expand ~goal:isGoal ~h:h ~id:true ~depth:depth () with
      None ->
            if log then
	      Printf.fprintf stdout "gen:%d exp:%d\n%!" (getGen ()) (getExp());
	    search (depth + 1) (* This will run forever is there is no solution even in a finite search space*)
    | goalNode -> goalNode in
  search 0

type 'a openHeap = {
  mutable size : int;
  mutable data : 'a array
}
		     
let makeEmptyOpenHeap () =
  {
    size = 0;
    data = makeArrayOfDummyNodes 1
  }
		      
let isEmpty openHeap =
  openHeap.size = 0

let parent i =
  match i with
    0 -> -1
  | i -> (i - 1) / 2

let left i =
  2 * i + 1
	    
let right i =
  2 * i + 2

let swap heap i j =
  let temp = heap.data.(i) in
  heap.data.(i) <- heap.data.(j);
  heap.data.(j) <- temp


let rec bubble_up heap i =
  match i with
    0 -> i
  | i -> let node = heap.data.(i)
	 and parentNode = heap.data.(parent i) in
	 if node.g < parentNode.g then (
	   swap heap i (parent i);
	   bubble_up heap (parent i))
	 else
	   i

let rec bubble_down heap i =
  if left i >= heap.size
     && right i >= heap.size then
    i
  else
    let node = heap.data.(i)
    and leftNode = heap.data.(left i)
    and rightNode = heap.data.(right i) in
    if right i >= heap.size then
      if leftNode.g < node.g then (
	swap heap (left i) i;
	bubble_down heap (left i))
      else
	i
    else
      if leftNode.g <= rightNode.g then
	if leftNode.g < node.g then (
	  swap heap (left i) i;
	  bubble_down heap (left i))
	else
	  i
      else
	if rightNode.g < node.g then (
	  swap heap (right i) i;
	  bubble_down heap (right i))
	else
	  i
    
      
let add heap node =
  let i = heap.size in 
  heap.size <- heap.size + 1;
  if heap.size = Array.length heap.data then (
    heap.data <- Array.append heap.data (makeArrayOfDummyNodes heap.size);
    heap.data.(i) <- node;
    bubble_up heap i)
  else (
    heap.data.(i) <- node;
    bubble_up heap i)

let pop heap =
  let minNode = heap.data.(0) in
  match heap.size with
    0 -> None
  | 1 -> heap.size <- heap.size - 1; 
	 Some minNode
  | _ -> heap.size <- heap.size - 1;
	 swap heap 0 heap.size; (* Assumes size will never be negative *)
	 let _ = bubble_down heap 0 in
	 Some minNode
	 
let uniformCostSearch  ?(bat:bool=false) ~init:initialState ~exp:expand ~goal:isGoal =
  countGen ();
  let start = startNode initialState
  and endNode = ref None in
  let openHeap = makeEmptyOpenHeap ()
  and addGen, neverGen = generated ~bat () in
  let _ = add openHeap start in
  addGen start;
  let rec search () =
    match pop openHeap with
      None -> () (* No solution *)
    | Some node -> countExp ();
		   if isGoal node.state then
		     endNode := Some node
		   else
		     let rec gen states =
		       match states with
			 [] -> search ()
		       | hd :: tl -> let action, successorState = hd in
				     if neverGen successorState then (
				       countGen ();
				       let newNode = (generateNode node action successorState) in
				       addGen newNode;
				       let _ = add openHeap newNode in ());
				     gen tl in
		     gen (expand node.state) in
  search ();
  !endNode
					      

let charge ncols nrows =
  2 * ncols + 2 * nrows + 1 - 4

    
let setCurrentLocation state i =
  state.cur <- i    
    
let initializeProblem ?(charger=(-1)) ncols nrows obstacleList =
  let problem = {
    blocked = Array.make (ncols * nrows) false;
    recharger = charger;
    charge = charge ncols nrows;
    nrows = nrows;
    ncols = ncols; 
  } in
  List.iter (fun a -> problem.blocked.(a) <- true ) obstacleList;
  problem

let initializeState ?(charge=(-1)) location dirtList = 
  {
    cur = location;
    dirt = dirtList;
    charge = charge
  }

let addDirt state i =
  state.dirt <- i :: state.dirt
			   
let isDirty dirtList i =
    List.exists (fun a -> a = i) dirtList

let clean dirt i =
  let rec clean cleaner tail =
    match tail with
      [] -> List.rev cleaner
    | hd :: tl -> if hd != i then
		   clean (hd :: cleaner) tl
		 else
		   clean cleaner tl in
  List.rev (clean [] dirt) (* Keep dirt in sorted order! *)

let readDimensions () =
  let ncols = read_int ()
  and nrows = read_int () in
  ncols, nrows

let readMap ncols nrows =
  let map = Array.make (ncols * nrows) '\000' in
  for i = 0 to nrows - 1 do
    let line = read_line () in
    for j = 0 to ncols - 1 do
      let pos = i * ncols + j in
      map.(pos) <- (String.get line j)
    done
  done;
  map


let extractFeatures map = (* test this *)
  let dirtList = ref []
  and robotLocation = ref (-1)
  and rechargerLocation = ref (-1)
  and obstacleList = ref [] in
  Array.iteri (fun i a ->
	       match a with
		 '*' -> dirtList := i :: !dirtList (* Position of dirt will be listed in reverse order *)
	       | ':' -> rechargerLocation := i
	       | '@' -> robotLocation := i
	       | '#' -> obstacleList := i :: !obstacleList
	       | _ -> ())
	      map;
  (List.rev !dirtList), !robotLocation, !rechargerLocation, !obstacleList (* Put dirt in ascending order for easier debugging *)

(** Returns a NEW state *)
let westLocation state =
  state.cur - 1

let goWest ?(bat:bool=false) state =
{
  cur = westLocation state;
  dirt = List.map (fun a -> a) state.dirt; 
  charge = match bat with true -> state.charge - 1 | _ -> state.charge
}

let eastLocation state =
  state.cur + 1
  
let goEast ?(bat:bool=false) state =
{
  cur = eastLocation state;
  dirt = List.map (fun a -> a) state.dirt; 
  charge = match bat with true -> state.charge - 1 | _ -> state.charge
}

let northLocation problem state =
  state.cur - problem.ncols

let goNorth ?(bat:bool=false) problem state =
{
  cur = northLocation problem state;
  dirt = List.map (fun a -> a) state.dirt; 
  charge = match bat with true -> state.charge - 1 | _ -> state.charge 
}

let southLocation problem state =
  state.cur + problem.ncols
  
let goSouth  ?(bat:bool=false) problem state =
{
  cur = southLocation problem state ;
  dirt = List.map (fun a -> a) state.dirt; 
  charge = match bat with true -> state.charge - 1 | _ -> state.charge
}

let doClean  ?(bat:bool=false) state =
  {
    cur = state.cur;
    dirt = clean state.dirt state.cur;
    charge = match bat with true -> state.charge - 1 | _ -> state.charge
  }
let doCharge problem state =
  {
    cur = state.cur;
    dirt = List.map (fun a -> a) state.dirt;
    charge = problem.charge
  }
						 

let isApplicableGoNorth problem state =
  state.cur / problem.ncols > 0 && not problem.blocked.(northLocation problem state)
				  
let isApplicableGoSouth problem state =
  state.cur / problem.ncols < (problem.nrows - 1) && not problem.blocked.(southLocation problem state)

let isApplicableGoWest problem state =
  state.cur mod problem.ncols > 0 && not problem.blocked.(westLocation state)

let isApplicableGoEast problem state =
  state.cur mod problem.ncols < (problem.ncols - 1) && not problem.blocked.(eastLocation state)

let isApplicableDoCharge problem state =
  state.cur = problem.recharger
				     

let applicableActions ?(bat:bool=false) problem state =
  let actionList =  ref [] in

  if not bat then (
  
    if isDirty state.dirt state.cur then
      actionList := {name="V";
		     operator=doClean;
		     cost=1}
		    :: !actionList;
    
    if isApplicableGoWest problem state then
      actionList := {name="W";
		     operator=goWest;
		     cost=1}
		    :: !actionList;
    
    if isApplicableGoNorth problem state then
      actionList := {name="N";
		     operator=goNorth problem;
		     cost=1}
		    :: !actionList;
    
    if isApplicableGoEast problem state then
      actionList := {name="E";
		     operator=goEast;
		     cost=1}
		  :: !actionList;
    
    if isApplicableGoSouth problem state then
      actionList := {name="S";
		     operator=goSouth problem;
		     cost=1}
		    :: !actionList;
  ) else ( (* bat *)

    if state.charge > 0 (* Assume all actions cost 1 *) then (
      
      if isDirty state.dirt state.cur then
	actionList := {name="V";
		       operator=doClean ~bat:bat;
		       cost=1}
		      :: !actionList;
    
      if isApplicableGoWest problem state then
	actionList := {name="W";
		       operator=goWest ~bat:bat;
		       cost=1}
		      :: !actionList;
    
      if isApplicableGoNorth problem state then
	actionList := {name="N";
		       operator=goNorth ~bat:bat problem;
		       cost=1}
		      :: !actionList;
    
      if isApplicableGoEast problem state then
	actionList := {name="E";
		       operator=goEast ~bat:bat;
		       cost=1}
		      :: !actionList;
    
      if isApplicableGoSouth problem state then
	actionList := {name="S";
		       operator=goSouth ~bat:bat problem;
		       cost=1}
		      :: !actionList;
    );
    
    if isApplicableDoCharge problem state then
      actionList := {name="R";
		     operator=doCharge problem;
		     cost=1} :: !actionList; (* Recharging still costs one but doesn't reduce charge *)
  );
  !actionList
   
				  
let transition state action =
  action.operator state

(** Generates successor states *)
let expand ?(bat:bool=false) problem state =
  let actionList = applicableActions ~bat problem state in
  List.map (fun action ->
	    action, action.operator state)
	   actionList

let printPlanToStdout node =
  let path = recoverNodesOnPath node in
  List.iter (fun node ->
	     if not (node.action = "None") then
	       Printf.fprintf stdout "%s\n%!" node.action)
	   path

let run ?(log:bool=false) algorithm battery initialState problem heuristic =
    match algorithm with
      "uniform-cost" -> uniformCostSearch
			  ~bat:battery
			  ~init:initialState
			  ~exp:(expand ~bat:battery problem)
			  ~goal:isGoal 
    | "depth-first" -> depthFirstSearch
			 ~bat:battery
			 ~init:initialState
			 ~exp:(expand ~bat:battery problem)
			 ~goal:isGoal
			 ()
    | "depth-first-id" -> iterativeDeepeningDepthFirstSearch
				    ~bat:battery
				    ~init:initialState
				    ~exp:(expand ~bat:battery problem)
				    ~goal:isGoal
				    ~log:log
				    ()
    | "greedy" -> greedySearch
		    ~bat:battery
		    ~init:initialState
		    ~exp:(expand ~bat:battery problem)
		    ~goal:isGoal
		    ~log:log
		    ()
		    
    | "a-star" -> astarSearch
		    ~bat:battery
		    ~init:initialState
		    ~exp:(expand ~bat:battery problem)
		    ~goal:isGoal
		    ~h:heuristic
		    ()
		    
    | "ida-star" -> iterativeDeepeningAstarSearch
		    ~bat:battery
		    ~init:initialState
		    ~exp:(expand ~bat:battery problem)
		    ~goal:isGoal
		    ~h:heuristic
		    ~log:log
		    ()
    | _ -> None
	   
let main () =

  let ncols, nrows = readDimensions () in
  let map = readMap ncols nrows in
  let dirtList, robotLocation, rechargerLocation, obstacleList = extractFeatures map in
  let problem =
    initializeProblem
      ~charger:rechargerLocation 
      ncols
      nrows
      obstacleList in
  let initialState =
    initializeState
      ~charge:problem.charge
      robotLocation
      dirtList in

  let goalNode = ref None 
  and battery = ref false
  and algorithm = ref "none"
  and heuristic = ref h0
  and log = ref false in (* For regression testing *)
  Arg.parse [("-battery", Arg.Set battery, "");
	    ("-log", Arg.Set log, "")]
	    (fun arg ->
	     match arg with
	     "depth-first" | "depth-first-id" | "uniform-cost" | "a-star" | "ida-star" | "greedy"  -> algorithm := arg;
	     | "h0" -> heuristic := h0
	     | "h1" -> heuristic := h1
	     | "h2" -> heuristic := h2 problem
	     | "h3" -> heuristic := h3 problem
	     | _ -> () )
	    "";

  if !log then
    Printf.fprintf stdout "%s %b\n%!" !algorithm !battery;
  
  goalNode := run ~log:!log !algorithm !battery initialState problem !heuristic;
	   
  match !goalNode with
    None -> Printf.fprintf stdout "No solution";
  | Some n -> printPlanToStdout n;
	      Printf.fprintf stdout "%d nodes generated\n%!" (getGen ());
	      Printf.fprintf stdout "%d nodes expanded\n%!" (getExp ())

let main =
      main () 
				

