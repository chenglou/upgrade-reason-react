let asd12 = self.reduce(fooa);

let asd = self.reduce__pleaseInlineTheArgumentAndRunTheScriptAgain(() => Foo);

self.reduce(foo, ());

() => self.reduce(foo, ());

reduce(foo, ());

let asd =
  self.reduce(() => {
    doSomething;
    Foo
  });

let asd = self.reduce((_) => Foo);

let asd = () => self.send(FooThisShouldntChange);

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

foo(reduce((_) => DesktopNotifsDenied));

let handleNotificationsChange = (_event, {ReasonReact.state, reduce}) =>
  /* foo(bar, reduce((_) => DesktopNotifsDenied)); */
  foo(bar, reduce((_) => DesktopNotifsDenied));
