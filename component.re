let asd = self.reduce(foo);

let asd = self.reduce__pleaseInlineTheArgumentAndRunTheScriptAgain(() => Foo);

self.reduce(foo, ());

let asd =
  self.reduce(() => {
    doSomething;
    Foo
  });

let asd = self.reduce((_) => Foo);

let asd =
  self.reduce((a) => {
    open Foo;
    doSomeEventFirst(a);
    let b = a;
    doSomeEvent(a);
    doSomeEvent2(a);
    if (a) {
      more();
      Foo(a);
    } else {
      more();
      Foo(a);
    }
  });

let asd = self.ReasonReact.reduce((a) => Foo(a));

let asd = reduce(bar);

let asd = reduce((_) => Foo);
