let asd12 = self.reduce__pleaseInlineTheArgumentAndRunTheScriptAgain(fooa);

let asd = () => self.send(Foo);

self.reduce__pleaseInlineTheArgumentAndRunTheScriptAgain(foo, ());

reduce__pleaseInlineTheArgumentAndRunTheScriptAgain(foo, ());

self.send(Foo);

send(Foo);

self.send(Bar);

send(Bar);

let asd = () => {
  doSomething;
  self.send(Foo);
};

let asd = (_) => self.send(Foo);

let asd = () => self.send(FooThisShouldntChange);

let asd = a => {
  open Foo;
  doSomeEventFirst(a);
  let b = a;
  doSomeEvent(a);
  doSomeEvent2(a);
  self.send(
    if (a) {
      more();
      Foo(a);
    } else {
      more();
      Foo(a);
    }
  );
};

let asd = a => self.ReasonReact.send(Foo(a));

let asd = reduce(bar);

let asd = (_) => send(Foo);

foo((_) => send(DesktopNotifsDenied));

let handleNotificationsChange = (_event, {ReasonReact.state, send}) =>
  /* foo(bar, reduce((_) => DesktopNotifsDenied)); */
  foo(bar, (_) => send(DesktopNotifsDenied));


/***
 * Copyright 2004-present Facebook. All Rights Reserved.
 */
[@bs.module]
external centeredContainer : ReasonReact.reactClass =
  "CenteredContainer.react";
