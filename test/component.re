let component = ReasonReact.statelessComponent("foo");

let make = (~prop1=1, ~prop2, children) => {
  ...component,
  render: _self => <div />,
};

[@bs.module "fooModule"] external component2: ReasonReact.reactClass = "";

[@bs.deriving abstract]
type props = {
  prop1: int,
  prop2: string,
  prop3: float,
};

let make2 = (~prop1, ~prop2=?, ~prop3=Foo.bar, children) =>
  ReasonReact.wrapJsForReason(
    ~reactClass=component2,
    ~props=props(~prop1, ~prop2, ~prop3=Foo.convertBar(prop3)),
    children,
  );

module Nested = {
  let make3 = (~prop1, ~prop2=?, ~prop3=Foo.bar, children) =>
    ReasonReact.wrapJsForReason(
      ~reactClass=component2,
      ~props=props(~prop1, ~prop2, ~prop3=Foo.convertBar(prop3)),
      children,
    );
  /**
 * This is a wrapper created to let this component be used from the new React api.
 * Please convert this component to a [@react.component] function and then remove this wrapping code.
 */
  let make3 =
    ReasonReactCompat.wrapReasonReactForReact(
      ~component=ReasonReact.statelessComponent("TemporaryRefactorComponent"),
      (
        reactProps: {
          .
          "prop3": option('prop3),
          "prop2": option('prop2),
          "prop1": 'prop1,
          "children": 'children,
        },
      ) =>
      make3(
        ~prop3=?reactProps##prop3,
        ~prop2=?reactProps##prop2,
        ~prop1=reactProps##prop1,
        reactProps##children,
      )
    );
  [@bs.obj]
  external make3Props:
    (
      ~children: 'children,
      ~prop1: 'prop1,
      ~prop2: 'prop2=?,
      ~prop3: 'prop3=?,
      unit
    ) =>
    {
      .
      "prop3": option('prop3),
      "prop2": option('prop2),
      "prop1": 'prop1,
      "children": 'children,
    } =
    "";
};

module Nested = {
  let make4 = (~prop1, ~prop2=?, ~prop3=Foo.bar, children) =>
    ReasonReact.wrapJsForReason(
      ~reactClass=component2,
      ~props={"prop1": prop1},
      children,
    );
  /**
 * This is a wrapper created to let this component be used from the new React api.
 * Please convert this component to a [@react.component] function and then remove this wrapping code.
 */
  let make4 =
    ReasonReactCompat.wrapReasonReactForReact(
      ~component=ReasonReact.statelessComponent("TemporaryRefactorComponent"),
      (
        reactProps: {
          .
          "prop3": option('prop3),
          "prop2": option('prop2),
          "prop1": 'prop1,
          "children": 'children,
        },
      ) =>
      make4(
        ~prop3=?reactProps##prop3,
        ~prop2=?reactProps##prop2,
        ~prop1=reactProps##prop1,
        reactProps##children,
      )
    );
  [@bs.obj]
  external make4Props:
    (
      ~children: 'children,
      ~prop1: 'prop1,
      ~prop2: 'prop2=?,
      ~prop3: 'prop3=?,
      unit
    ) =>
    {
      .
      "prop3": option('prop3),
      "prop2": option('prop2),
      "prop1": 'prop1,
      "children": 'children,
    } =
    "";
};

React.string("foo");
React.array([|foo|]);
React.null;

module type Nested = {
  [@react.component]
  let make5:
    (~prop1: string, ~prop2: int=?, ~prop3: string=?, ~children: float) =>
    React.element;
};

module Alex = {
  module One = {
    let component = ReasonReact.statelessComponent(__MODULE__);
    let make = children => {
      ...component,
      render: _ => <button> ...children </button>,
    };
    /**
 * This is a wrapper created to let this component be used from the new React api.
 * Please convert this component to a [@react.component] function and then remove this wrapping code.
 */
    let make =
      ReasonReactCompat.wrapReasonReactForReact(
        ~component, (reactProps: {. "children": 'children}) =>
        make(reactProps##children)
      );
    [@bs.obj]
    external makeProps:
      (~children: 'children, unit) => {. "children": 'children} =
      "";
  };
  module Two = {
    let component = ReasonReact.statelessComponent(__MODULE__);
    let make = _ => {...component, render: _ => <Button />};
    /**
 * This is a wrapper created to let this component be used from the new React api.
 * Please convert this component to a [@react.component] function and then remove this wrapping code.
 */
    let make =
      ReasonReactCompat.wrapReasonReactForReact(
        ~component, (reactProps: {. "children": 'children}) =>
        make(reactProps##children)
      );
    [@bs.obj]
    external makeProps:
      (~children: 'children, unit) => {. "children": 'children} =
      "";
  };
};
/**
 * This is a wrapper created to let this component be used from the new React api.
 * Please convert this component to a [@react.component] function and then remove this wrapping code.
 */
let make =
  ReasonReactCompat.wrapReasonReactForReact(
    ~component,
    (
      reactProps: {
        .
        "prop2": 'prop2,
        "prop1": option('prop1),
        "children": 'children,
      },
    ) =>
    make(
      ~prop2=reactProps##prop2,
      ~prop1=?reactProps##prop1,
      reactProps##children,
    )
  );
[@bs.obj]
external makeProps:
  (~children: 'children, ~prop1: 'prop1=?, ~prop2: 'prop2, unit) =>
  {
    .
    "prop2": 'prop2,
    "prop1": option('prop1),
    "children": 'children,
  } =
  "";
/**
 * This is a wrapper created to let this component be used from the new React api.
 * Please convert this component to a [@react.component] function and then remove this wrapping code.
 */
let make2 =
  ReasonReactCompat.wrapReasonReactForReact(
    ~component=ReasonReact.statelessComponent("TemporaryRefactorComponent"),
    (
      reactProps: {
        .
        "prop3": option('prop3),
        "prop2": option('prop2),
        "prop1": 'prop1,
        "children": 'children,
      },
    ) =>
    make2(
      ~prop3=?reactProps##prop3,
      ~prop2=?reactProps##prop2,
      ~prop1=reactProps##prop1,
      reactProps##children,
    )
  );
[@bs.obj]
external make2Props:
  (
    ~children: 'children,
    ~prop1: 'prop1,
    ~prop2: 'prop2=?,
    ~prop3: 'prop3=?,
    unit
  ) =>
  {
    .
    "prop3": option('prop3),
    "prop2": option('prop2),
    "prop1": 'prop1,
    "children": 'children,
  } =
  "";
