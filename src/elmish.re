/* type effect 'props 'state 'action =
     | Html ('state => 'action => ReactRe.reactElement);

   type node 'props 'state 'action = {
     init: 'state,
     update: 'state => 'action => 'state,
     effects: 'props => 'state => ('action => unit) => list (effect 'props 'state 'action)
   };

   module Actions = Map.Make Int64;

   type props = {decBy: int};

   type state = int;

   type action =
     | Increment
     | Decrement int;

   let counter: node props state action = {
     init: 0,
     update: fun state action =>
       switch action {
       | Increment => state + 1
       | Decrement n => state - n
       },
     effects: fun {decBy} state dispatch =>
       <div>
         <button onClick=(updater dec)> (ReactRe.stringToElement "-") </button>
         <span> (ReactRe.stringToElement (string_of_int props.state.count)) </span>
         <button onClick=(updater inc)> (ReactRe.stringToElement "+") </button>
       </div>
   }; */
type effect =
  | Html ReactRe.reactElement;

module type Node = {
  type state;
  type action;
  let init: state;
  let update: state => action => state;
  type props;
  let effects:
    props => state => ((props => ReactRe.event => action) => ReactRe.event => unit) => list effect;
};

/* let effects : 'a => state => ((props => 'b => action) => ReactRe.event => unit) => list effect */
/* let effects : props => state => ((props => ReactRe.event => action) => unit) => list effect */
module Counter = {
  module Node: Node = {
    type state = {count: int};
    type action =
      | Increment
      | Decrement int;
    let init = {count: 0};
    let update state action =>
      switch action {
      | Increment => {count: state.count + 1}
      | Decrement n => {count: state.count - n}
      };
    type props = {decBy: int};
    let inc _ _ => Increment;
    let dec {decBy} _ => Decrement decBy;
    let effects (_: props) state updater => [
      Html <div>
             <button onClick=(updater dec)> (ReactRe.stringToElement "-") </button>
             <span> (ReactRe.stringToElement (string_of_int state.count)) </span>
             <button onClick=(updater inc)> (ReactRe.stringToElement "+") </button>
           </div>
    ];
  };
};

module TwoOf (Node: Node) => {
  type state = {one: Node.state, two: Node.state};
  type action =
    | One Node.action
    | Two Node.action;
  let init = {one: Node.init, two: Node.init};
  let update state action =>
    switch action {
    | One a => {...state, one: Node.update state.one a}
    | Two a => {...state, two: Node.update state.two a}
    };
  type props = Node.props;
  let one _ action => One action;
  let two _ action => Two action;
  let effects props state updater => {
    let eone = Node.effects props state (updater one);
    let etwo = Node.effects props state (updater two);
    List.concat [eone, etwo]
  };
};
/* module TwoCounters = TwoOf Counter; */
