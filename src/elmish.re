module Counter = {
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
  let createElement ::decBy ::state ::dispatch => wrapProps {decBy, state, dispatch};
};

module App = Counter;

module Core = {
  module CoreClass = {
    include ReactRe.Component.Stateful;
    let name = "Core";
    type props 'model 'action = {
      init: 'model,
      update: 'model => 'action => 'model,
      view: ('action => unit) => 'model => ReactRe.reactElement
    };
    type state 'model = 'model;
    let getInitialState props => props.init;
    let dispatcher {props, state} action => Some (props.update state action);
    let render {props, state, updater} => props.view (updater dispatcher) state;
  };
  include ReactRe.CreateComponent CoreClass;
  let createElement ::init ::update ::view => wrapProps {init, update, view};
};
/*
 module TwoOf = {
   type model = {one: Component.model, two: Component.model};
   type action =
     | One Component.action
     | Two Component.action;
   type dispatch = action => unit;
   let init = {one: Component.init, two: Component.init};
   let update state action =>
     switch action {
     | One a => {...state, one: Component.update state.one a}
     | Two a => {...state, two: Component.update state.two a}
     };
   module TwoOfClass = {
     include ReactRe.Component;
     let name = "TwoOf";
     type props = {state: model, dispatch};
     let one {props} action => {
       props.dispatch (One action);
       None
     };
     let two {props} action => {
       props.dispatch (Two action);
       None
     };
     let render {props, updater} =>
       <div>
         <Component dispatch=(updater one) state=props.state.one />
         <Component dispatch=(updater two) state=props.state.two />
       </div>;
   };
   include ReactRe.CreateComponent TwoOfClass;
   let createElement ::state ::dispatch => wrapProps {state, dispatch};
 }; */
/*
 /* Here we're composing with the TwoOf combinator! */
 module TwoOfElmishCounterApp = Core (TwoOf (ElmishCounter CounterProps));

 /* The List methods aren't all the full featured so we have to make this ourselves. */
 let rec firstN n l =>
   switch (n, l) {
   | (0, _) => []
   | (_, []) => []
   | (n, [h, ...t]) => [h, ...firstN (n - 1) t]
   };

 /* This combinator makes a component undoable! */
 module Undoable (Component: Component) => {
   type model = {time: int, states: list Component.model};
   let init = {time: 0, states: [Component.init]};
   type action =
     | Undo
     | Redo
     | Child Component.action;
   type dispatch = action => unit;
   let update state action => {
     let {time, states} = state;
     switch action {
     | Undo => {states, time: time - 1}
     | Redo => {states, time: time + 1}
     | Child a => {
         time: time + 1,
         states: {
           let currentState = List.nth states time;
           let nextState = Component.update currentState a;
           let history = firstN (time + 1) states;
           List.append history [nextState]
         }
       }
     }
   };
   module UndoableClass = {
     include ReactRe.Component;
     let name = "Undoable";
     type props = {state: model, dispatch};
     let undo {props} _ => {
       props.dispatch Undo;
       None
     };
     let redo {props} _ => {
       props.dispatch Redo;
       None
     };
     let child {props} a => {
       props.dispatch (Child a);
       None
     };
     let render {props, updater} => {
       let {time, states} = props.state;
       let canUndo = time > 0;
       let canRedo = time < List.length states - 1;
       <div>
         <button onClick=(updater undo) disabled=(Js.Boolean.to_js_boolean (not canUndo))>
           (ReactRe.stringToElement "undo")
         </button>
         <button onClick=(updater redo) disabled=(Js.Boolean.to_js_boolean (not canRedo))>
           (ReactRe.stringToElement "redo")
         </button>
         <Component state=(List.nth states time) dispatch=(updater child) />
       </div>
     };
   };
   include ReactRe.CreateComponent UndoableClass;
   let createElement ::state ::dispatch => wrapProps {state, dispatch};
 }; */
