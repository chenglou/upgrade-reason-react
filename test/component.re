let make = () => {
  ...component,
  render: 1,
  didMount: a => {
    foo();
    bar();
    NoUpdate;
  }
};

module Foo = {
  let make = () => {
    ...component,
    render: 1,
    didMount: a => {
      foo();
      bar();
      ReasonReact.NoUpdate;
    }
  };
}
