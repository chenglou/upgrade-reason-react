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
    ...componenta,
    didMount: a => {
      foo();
      bar();
      ReasonReact.NoUpdate;
    }
  };
  let make = () => {
    ...component,
    didMount: a =>
      if (a) {
        foo;
        ReasonReact.NoUpdate;
      } else {
        bar;
        ReasonReact.Update(a);
      }
  };
  let make = () => {
    ...component,
    didMount: a => {
      asd;
      switch (a) {
      | A => ReasonReact.NoUpdate
      | B =>
        foo;
        NoUpdate;
      };
    }
  };
};

/* don't change */
let make = () => {
  render: 1,
  didMount: a => {
    foo();
    bar();
    ReasonReact.NoUpdate;
  }
};

let make = () => {
  ...component,
  render: 1,
  didMounta: a => {
    foo();
    bar();
    ReasonReact.NoUpdate;
  }
};

let make = () => {
  ...component,
  render: 1,
  didMounta: a => {
    foo();
    ReasonReact.NoUpdate;
    ReasonReact.Update(s);
  }
};

let make = () => {
  ...component,
  render: 1,
  didMounta: a => {
    foo();
    ok(ReasonReact.NoUpdate);
  }
};

let make = () => {...component, render: 1, didMount: foo};
