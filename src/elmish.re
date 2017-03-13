/* type dispatch 'action =
     | Action ('action => unit)
     | Event (ReactRe.event => 'action => unit);

   type component 'model 'action 'props = {
     init: 'model,
     update: 'model => 'action => 'model,
     view: 'props => 'model => dispatch 'action => ReactRe.reactElement
   };

   type model = {count: int};

   type action =
     | Increment
     | Decrement int;

   type props = {decBy: int};

   let counter: component model action props = {
     init: {count: 0},
     update: fun state action =>
       switch action {
       | Increment => {count: state.count + 1}
       | Decrement n => {count: state.count - n}
       },
     view: fun {decBy} state dispatch =>
       <div>
         <button onClick=(dispatch (Action Increment))> (ReactRe.stringToElement "-") </button>
         <span> (ReactRe.stringToElement (string_of_int state.count)) </span>
         <button onClick=(dispatch (Action (Decrement decBy)))>
           (ReactRe.stringToElement "+")
         </button>
       </div>
   }; */
/*
 module ElmishCounter (Props: CounterProps) => {
   type model = {count: int};
   type action =
     | Increment
     | Decrement int;
   type dispatch = action => unit;
   let init = {count: 0};
   let update state action =>
     switch action {
     | Increment => {count: state.count + 1}
     | Decrement n => {count: state.count - n}
     };
   module CounterClass = {
     include ReactRe.Component;
     let name = "Counter";
     type props = {decBy: int, state: model, dispatch};
     let inc {props} _ => {
       props.dispatch Increment;
       None
     };
     let dec {props} _ => {
       props.dispatch (Decrement props.decBy);
       None
     };
     let render {props, updater} => {
       Js.log "render Counter";
       <div>
         <button onClick=(updater dec)> (ReactRe.stringToElement "-") </button>
         <span> (ReactRe.stringToElement (string_of_int props.state.count)) </span>
         <button onClick=(updater inc)> (ReactRe.stringToElement "+") </button>
       </div>
     };
   };
   include ReactRe.CreateComponent CounterClass;
   let createElement ::state ::dispatch => wrapProps {decBy: Props.decBy, state, dispatch};
 }; */
/* let rec firstN n l =>
   if (n === 0 || List.length l === 0) {
     []
   } else {
     let h = List.hd l;
     let t = List.tl l;
     List.append [h] (firstN (n - 1) t)
   }; */
/* type result 'a = {mutable list: list 'a};

   let firstN (n: int) (l: list 'a) :list 'a => {
     let result: result 'a = {list: []};
     let start = 0;
     let stop = min (n - 1) (List.length l - 1);
     for i in start to stop {
       result.list = List.append result.list [List.nth l i]
     };
     result.list
   }; */
let rec nth l n =>
  switch l {
  | [] => None
  | [h, ...t] => n === 0 ? Some h : nth t (n - 1)
  };

let x = nth [0, 1, 2, 3] 999;

switch x {
| Some i => print_int i
| None => print_string "not found!"
};

let y = nth [0, 1, 2, 3] 1;

switch y {
| Some i => print_int i
| None => print_string "not found!"
};

let rec forever n => forever n;

let rec make length n =>
  switch length {
  | 0 => []
  | i => [n, ...make (i - 1) n]
  };

make max_int 0;
