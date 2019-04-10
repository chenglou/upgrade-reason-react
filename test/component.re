let component = ReasonReact.statelessComponent("foo");

let make = (~prop1=1, ~prop2, children) => {
  ...component,
  render: _self => <div />,
};

[@bs.module "fooModule"]
external component2 : ReasonReact.reactClass = "";

[@bs.deriving abstract]
type props = {
  prop1: int,
  prop2: string,
  prop3: float
};

let make2 =
    (
      ~prop1,
      ~prop2=?,
      ~prop3=Foo.bar,
      children,
    ) =>
  ReasonReact.wrapJsForReason(
    ~reactClass=component2,
    ~props=props(~prop1, ~prop2, ~prop3=Foo.convertBar(prop3)),
    children,
  );

module Nested = {
  let make3 =
    (
      ~prop1,
      ~prop2=?,
      ~prop3=Foo.bar,
      children,
    ) =>
  ReasonReact.wrapJsForReason(
    ~reactClass=component2,
    ~props=props(~prop1, ~prop2, ~prop3=Foo.convertBar(prop3)),
    children,
  );
};

module Nested = {
  let make4 =
    (
      ~prop1,
      ~prop2=?,
      ~prop3=Foo.bar,
      children,
    ) =>
  ReasonReact.wrapJsForReason(
    ~reactClass=component2,
    ~props={"prop1": prop1},
    children,
  );
};

ReasonReact.string("foo");
ReasonReact.array([|foo|]);
ReasonReact.null;

module type Nested = {
  let make5:
    (
      ~prop1: string,
      ~prop2: int=?,
      ~prop3: string=?,
      float,
    ) =>
    ReasonReact.component(int, string, int);
};