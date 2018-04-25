let asd = <div>(ReasonReact.stringToElement("foo"))</div>;

let asd2 = ReasonReact.arrayToElement;

let asd3 = foo ? <div /> : ReasonReact.nullElement;

let asd4 = ReasonReact.arrayToElement([|ReasonReact.nullElement|]);

let make = () => {
  ...component,
  render: 1,
  didMount: a => {
    foo();
    bar();
    NoUpdate;
  }
};

let make = () => {
  ...component,
  render: 1,
  didMount: a => {
    bar();
    Update(a);
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

let asd2 = ReasonReact2.stringToElement("foo");
