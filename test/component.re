/* ReactEventRe => ReactEvent */
open ReactEventRe;
module Foo = ReactEventRe.UI;
ReactEventRe.Keyboard.stopPropagation(e);

/* _type => type_ */
let asd = ReactEventRe.Form._type;
let asd = ReactEventRe.(ReactEventRe.Form._type);

/* normalize pipe usage for events (avoid bs.send.pipe, deprecated) */
e |> ReactEventRe.Mouse.preventDefault;
e -> ReactEventRe.Mouse.preventDefault;

/* remove ReactDOMRe.domElementToObj in these cases + normalize pipe usage like above */
event |> ReactEventRe.Form.target |> ReactDOMRe.domElementToObj;
(event |> ReactEventRe.Form.target |> ReactDOMRe.domElementToObj)##value;
(ReactEventRe.Form.target(event) |> ReactDOMRe.domElementToObj)##value;
ReactDOMRe.domElementToObj(ReactEventRe.Form.target(event))##value;

/* don't accidentally transform things like this */
event |> ReactEventRe.target |> ReactDOMRe.domElementToObj;


/* ReasonReact.createDomElement("div", {"a": b}, bar) => ([@JSX] div(~a=b, ~children=bar, ())) aka <div a=b> ...bar </div> */
ReasonReact.createDomElement("div", {"a": b, "aria-label": d}, children);
ReasonReact.createDomElement("div", Js.Obj.empty(), children);
ReasonReact.createDomElement("div", {"data-foo": ReactEventRe.Keyboard.target}, children);
ReasonReact.createDomElement("span", props, bar);
ReasonReact.createDomElement("div", {"a": b, "aria-label": d}, (
  ReasonReact.createDomElement("div", {"a": b, "onClick": (e) => ReactEventRe.Mouse.preventDefault(e)}, children)
));
