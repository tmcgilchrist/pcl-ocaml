(***************************************)
(* CS 15212, Spring 2007               *)
(* Assignment 4                        *)
(* Due 03-28-2007, 2:12am, EST         *)
(* Andrew ID:                          *)
(* Section/TA:                         *)
(***************************************)


(* These are helpful for printing some of the more complicated data structures
 * in the later problems *)
Control.Print.printDepth := 10;
Control.Print.stringDepth := 300;


(* This loads the signatures, don't remove it. *)
use "/afs/andrew.cmu.edu/course/15/212sp/www/assignments/hw4/hw4.sig"
  handle _ => use "hw4.sig";
SMLofNJ.Internals.GC.messages false;


(* Remove these when finished to make sure you've got everything! *)
(*exception Unimplemented;
  datatype Unimplemented_type = DUMMY;*)


(*************)
(* Problem 0 *)
(*************)
exception WrongLength

fun map2 _ nil nil = nil
  | map2 f (x1::l1) (x2::l2) = f(x1, x2)::(map2 f l1 l2)
  | map2 _ _ _ = raise WrongLength


(***************)
(* Problem 1.1 *)
(***************)

(* val listToStr: ('a -> string) -> ('a list) -> string
 *     listToStr ts  l => a string representing the list l where (ts e)
 *                        is a string representing a single element e
 * Invariants: none
 * Effects: none
 *)
fun listToStr _ [] = ""
  | listToStr ts ([e]) = ts e
  | listToStr ts (e::tail) = ts e ^ ", " ^ (listToStr ts tail)

functor Signal (structure Time : ORDER
		structure Value : EQUAL)
  :> SIGNAL where type time = Time.t
            where type value = Value.t =
struct

  type time = Time.t
  type value = Value.t

  (* Signal Abstraction Function: A signal is represented as an initial
   * value along with a finite list of transitions, listing the time of
   * each transition and the new value of the signal at that time
   *)

  (* Representation Invariants:
   *
   *  Ordering: The time indices in the list are strictly increasing
   *            from left to right
   *
   *  Irredundancy: The initial value is different from the first
   *            value in the list (if such a value exists), and two
   *            consecutive values in the list are never the same
   *)
  type signal = value * (time * value) list

  val eq = Value.equal
  infix eq
  fun lt (t0,t1) = Time.compare(t0,t1)=LESS
  infix lt
  fun teq (t0,t1) = Time.compare(t0,t1)=EQUAL
  infix teq
    
  (* val fromList : value * (time * value) list -> signal
   * fromList l => l as a signal
   * Invariants: l satisfies the signal invariants
   * Effects: none
   *)
  fun fromList l = l

  (* val checkList : value * (time * value) list -> bool
   * checkList l => true iff l satisfies the signal invariants
   * Invariants: none
   * Effects: none
   *)
  fun checkList (_,[]) = true
    | checkList (v0,(t1,v1)::ts) =
      (not (v0 eq v1)) andalso checkTime (t1,(v1,ts))
  and checkTime (_,(_,[])) = true
    | checkTime (t0,(v0,ts as (t1,_)::_)) =
      (t0 lt t1) andalso checkList (v0,ts)

  (* val toList : signal -> value * (time * value) list
   * toList l => l as a list instead of a signal
   * Invariants: l satisfies the signal invariants
   * Effects: none
   *)
  fun toList s = s

  (* val map : (value -> value) -> signal -> signal
   * map f s => applies f to all values in s, and returns the resulting signal
   * Invariants: The signal satisfies all signal invariants
   * Effects: none
   *)
  fun map f (v0,[]) = (f v0, [])
    | map f (v0,(t1,v1)::ts) =
      let
	val v0' = f v0
	val TT as (v1',ts') = map f (v1,ts)
      in
	if v0' eq v1' then
	  TT
	else
	  (v0',(t1,v1')::ts')
      end

  (* val nextTrans (helper) : signal * signal -> time * signal * signal
   * nextTrans s1 s2 => takes two signals and returns the time and next 
   * transition of the next available time.  Will be of use in map2.
   * Invariants: the usual signal invariants; R & S can't have empty transition
   *  lists
   * Effects: none
   *)
  fun nextTrans (RR as (rv0,(rt1,rv1)::rs), S as (sv0,(st1,sv1)::ss)) =
    case Time.compare (rt1,st1) of
      LESS =>    (rt1,(rv1,rs),S)
    | GREATER => (st1,RR,(sv1,ss))
    | EQUAL =>   (rt1,(rv1,rs),(sv1,ss))



  (* val map2 : (value * value -> value) -> signal -> signal -> signal
   * map2 f s1 s2 => returns the resulting signal after applying the function f
   * to the pair of values at the corresponding times of s1 and s2
   * Invariants: signal invariants
   * Effects: none
   *)    
  fun map2 f (rv0,[]) (sv0,[]) = (f (rv0,sv0),[])
    | map2 f RR (sv0,[]) = map (fn n => f(n,sv0)) RR
    | map2 f (rv0,[]) S = map (fn n => f(rv0,n)) S
    | map2 f (RR as (rv0,_)) (S as (sv0,_)) =
      let
	      val v0' = f(rv0,sv0)
	      val (t1',R',S') = nextTrans(RR,S)
	      val TT as (v1',ts') = map2 f R' S'
      in
	      if v0' eq v1' then
	        TT
	      else
	        (v0',(t1',v1')::ts')
      end

  (* val fold : ((time*value)*'b -> 'b) -> (value -> 'b) -> signal -> 'b
   * for the signal s = (v0,[(t1,v1),(t2,v2),...,(tn,vn)]),
   * fold f b s = f((tn,vn), f( ... f( t2,v2), f((t1,v1), b v0))...))
   *
   * Invarians: signal invariants
   * Effects: none
   *)
  fun fold f b (i,l) = foldl f (b i) l
      

  (* val eval : signal -> (time -> value)
   * eval s => returns a function from time to value of the signal s
   * Invariants: signal invariants
   * Effects: none
   *)
  fun eval (v0, []) t = v0
    | eval (v0, (t1, v1)::s) t =
      if t lt t1 then
	v0
      else
	eval (v1, s) t

  (* val equal : signal*signal -> bool
   * equal (s1,s2) => true iff the signals are identical
   * Invariants: signal invariants
   * Effects: none
   *)
  fun equal ((v0, s),(v0', s')) =
    v0 eq v0' andalso equals (s, s')
  and equals ([], []) = true
    | equals ((t1,v1)::s2, (t1',v1')::s2') =
      t1 teq t1' andalso v1 eq v1' andalso equals (s2, s2')
    | equals _ = false


  (* val toString : signal -> string
   * toString s => a string representation of s
   * Invariants: none
   * Effects: none
   *)      
  fun toString (v0, l) = 
      let
	  fun entryStr (t, v) = "(" ^ (Time.toString t) ^ ", " ^ 
	      (Value.toString v) ^ ")"
      in
	  "(" ^ (Value.toString v0) ^ ", [" ^ (listToStr entryStr l) ^ "]" ^ ")"
      end
end


(***************)
(* Problem 1.2 *)
(***************)

signature RAIN_SIGNAL = SIGNAL where type time = int
                               where type value = real

structure IntOrder :> ORDER where type t = int =
struct
  type t = int
  val compare = Int.compare
  val toString = Int.toString
end

structure RealEqual :> EQUAL where type t = real =
struct
  type t = real
  val equal = Real.==
  val toString = Real.toString
end

structure RainSignal :> RAIN_SIGNAL = Signal (structure Time = IntOrder 
                                              structure Value = RealEqual)

(* val finddailymax : (value * (time * value) list) ->
                      (value * (time * value) list) ->
		      (value * (time * value) list)
 * finddailymax y1 y2 => finds the maximum rainfall on a daily basis of y1 and 
		         y2
 * Invariants: None
 * Effects: None
 *)
fun finddailymax year1 year2 = 
    RainSignal.toList (RainSignal.map2 Real.max (RainSignal.fromList year1)
		                                (RainSignal.fromList year2))




(***************)
(* Problem 2.1 *)
(***************)

structure RealOrder :> ORDER where type t = real =
struct
  type t = real
  val compare = Real.compare
  val toString = Real.toString
end

signature REAL_SIGNAL = SIGNAL where type time = real
                               where type value = real

structure RealSignal :> REAL_SIGNAL =
  Signal(structure Time = RealOrder
         structure Value = RealEqual)


(***************)
(* Problem 2.2 *)
(***************)

functor StepFunc (structure RS : REAL_SIGNAL)
  :> STEP_FUNC where type stepfunc = RS.signal =
struct

  (* Abstraction function:  We represent a simple step function as a
   * signal where the times are the x-values and the values are the
   * y-values.  We ignore the values at transition points
   *)

  (* Representation Invariants: A signal representing a step function
   * should have an initial value of 0 and should be 0 after the last
   * transition.
   *)

  type stepfunc = RS.signal
                 
  val zero = RS.fromList (0.0, nil)

  fun one (a,b) = RS.fromList (0.0,[(a,1.0),(b,0.0)])

  val plus = RS.map2 (fn (r1,r2) => r1 + r2)
  val absDiff = RS.map2 (fn (r1,r2) => abs (r1 - r2))
  val prod = RS.map2 (fn (r1,r2) => r1*r2)
  
  fun integrate s =
      (fn (x,y) => y)
        (RS.fold (fn ((x,h),((px,ph),s)) => ((x,h),ph*(x-px) + s))
                 (fn _ => ((0.0,0.0),0.0)) 
                 s)
      
  fun fromList (e,rlist) = 
      let
        (* val fixLastZero : (real*real) list -> (real*real) list
         *
         * fixLastZero tlst evaluates to a list which is identical
         * to tlst except that if second projection of the last
         * element of the list is 0.0, that list element will be
         * removed
         *
         * no effects, invariants
         *)

        fun fixLastZero [] = []
          | fixLastZero ((t,v)::[]) =
            if Real.== (v,0.0) then [(v,0.0)] else (t,v)::(e,0.0)::[]
          | fixLastZero (x::xs) =
            x::(fixLastZero xs)
      in
        RS.fromList (0.0,fixLastZero rlist)
      end

  val equal = RS.equal
  val toString = RS.toString

end;

structure StepFunc = StepFunc (structure RS = RealSignal)

(***************)
(* Problem 3.1 *)
(***************)

signature STEPFUNC_2D = SIGNAL where type time = real
                               where type value = StepFunc.stepfunc


structure StepFuncEqual :> EQUAL where type t = StepFunc.stepfunc = 
struct

  type t = StepFunc.stepfunc
  val equal = StepFunc.equal
  val toString = StepFunc.toString

end

structure StepFunc2d = Signal(structure Time = RealOrder
                              structure Value = StepFuncEqual)


(***************)
(* Problem 3.2 *)
(***************)

functor RApprox2d (structure SF2D : STEPFUNC_2D)
        :> RAPPROX_2D where type rapprox2d = SF2D.signal =
struct

type coord2d = real*real

(* Abstraction Function:  A 2d step function is represented as a
 * signal of 1d step functions.  The time values of this outer signal
 * are the initial y values of the step functions (the inner step
 * functions are strips along the x axis with values representing
 * height).
 *)

(* Representation Invariants:
 * The initial value of an rapprox2d is always StepFunc.zero, as is its
 * value after the final transition.
 *) 

type rapprox2d = SF2D.signal

fun genApprox2d f ((x,y),(x',y')) (nx,ny) pick =
    let
      val xdel = (x'-x)/(real nx)
      val ydel = (y'-y)/(real ny)
           
      (* No invariants/effects in these helpers unless otherwise specified *)
      
      (* val nthx : int -> real
       * nthx k finds the x coordinate of the start of the kth division 
       *   along the x axis
       *)
      fun nthx n = (real n)*xdel + x

      (* val nthy : int -> real
       * nthy k finds the y coordinate of the start of the kth division 
       *   along the y axis
       *)
      fun nthy n = (real n)*ydel + y
           

      (* val rem_dups : ('a * 'a -> bool) -> 'a*(('b*'a) list) ->
                        ('b*'a) list
       *
       * rem_dups eq (last,tlist) evaluates to a transition list
       *   representing the data in tlist, but with duplicate elements
       *   removed.  It uses eq to determine when values are equal
       *)
      fun rem_dups eq (_,[]) = []
        | rem_dups eq (last, ((t,v)::xs)) = 
          if eq (v,last) then rem_dups eq (last,xs)
          else (t,v)::(rem_dups eq (v,xs))
          

      (* val gen_row : int -> StepFunc.stepfunc
       *
       * gen_row kth creates a step function which represents the
       *   kth strip of the approximation to f with the appropriate
       *   range, number of subdivisions, and picking function.
       *   The strips run along the x-axis, so k reperesents the
       *   number of subdivisions up the y-axis where this horizontal
       *   strip is located.
       *)
      fun gen_row rownum = 
          let
            val ylow = nthy rownum
            val yhigh = nthy (rownum+1)
          in
            StepFunc.fromList 
              (nthx nx,
               rem_dups (Real.==)
                 (0.0,
                  (List.tabulate (nx,
                                  (fn n => 
                                      let
                                        val xlow = nthx n
                                        val xhigh = nthx (n+1)
                                      in
                                        (xlow, f (pick ((xlow,ylow),
                                                        (xhigh,yhigh))))
                                      end)))))
          end
    in
      SF2D.fromList
      (StepFunc.zero,
       rem_dups StepFunc.equal
                (StepFunc.zero,
                   List.tabulate 
                   ((ny+1),
                    (fn n => if n = ny then (nthy (ny),StepFunc.zero) else
                             (nthy n,gen_row n)))))
    end
    
fun lowerLeft (a,b) = a
fun upperRight (a,b) = b

val rPlus = SF2D.map2 (fn (f,g) => StepFunc.plus f g)
val rAbsDiff = SF2D.map2 (fn (f,g) => StepFunc.absDiff f g)
val rProd = SF2D.map2 (fn (f,g) => StepFunc.prod f g)

fun rSum2d s = 
    let
      val (_,int) = 
          SF2D.fold
            (fn ((t,v),((t0,i),s)) => ((t,StepFunc.integrate v),i*(t-t0)+s)) 
            (fn x => ((0.0,0.0),0.0)) s
    in
      int
    end
    
fun upperLowerDiff f range slices = 
    let
      val lowerApprox = genApprox2d f range slices lowerLeft
      val upperApprox = genApprox2d f range slices upperRight
    in
      rAbsDiff lowerApprox upperApprox
    end

end


structure RApprox2d = RApprox2d(structure SF2D = StepFunc2d)



(***************)
(* Problem 3.3 *)
(***************)

(* (5,5) - 50.0
 * (20,20) - 12.5
 * (100,100) - 2.5
 *
 * As we expect, a finer grained approximation allows a smaller range
 * of solutions.
 *)
