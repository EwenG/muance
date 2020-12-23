# Muance

A virtual dom library for Clojurescript.

- Muance supports stateful components
- Lifecycle hooks can be set on any component or DOM node
- Lifecycle hooks have a defined parent/children execution order
- Event handlers can be passed arbitrary parameters
- No synthetic event system
- Svg support
- Asynchronous rendering by default
- DOM node removal hook for easy fade out animation

Muance exposes a side effectful API which mutates an in memory representation of a virtual DOM.
Muance does not recreate a new virtual DOM on every render, which may reduce the pressure on the garbage collector.

Muance size is around 8k when minified, before gzip.

Muance is compatible with IE10+ and requires [requestAnimationFrame](https://developer.mozilla.org/en-US/docs/Web/API/window/requestAnimationFrame) to be polyfilled for IE9.

[Here](https://eweng.github.io/muance/examples/dbmonster/public/) is a dbmonster implementation using Muance.

## Quick start

The following code snippet creates a component and renders it on the page:

```
(ns muance.example
  (:require [muance.core :as m :include-macros true])
  (:require-macros [muance.h :as h]))
  
(m/defcomp example-component []
  (h/div "my first component"))
  

(defonce vtree (m/vtree)) ;; Creates a virtual DOM tree 
(m/append-child vtree (.-body js/document)) ;; Appends the virtual DOM tree to the page
(m/patch vtree example-component) ;; Renders the component
```

Muance API is defined in the `muance.core` namespace. HTML tag names are defined in the `muance.h` namespace.

## Examples

Source code for the examples is in the [examples](https://github.com/EwenG/muance/tree/master/examples/) directory.

- [todo app](https://eweng.github.io/muance/examples/todo/public/)


## Top level API

- `(muance.core/vtree)`: Creates a new vtree
- `(muance.core/vtree async)`: Creates a new vtree, the vtree is rendered asynchronously if async is true, synchronously otherwise. See [asynchronous rendering](#asynchronous-rendering).
- `(m/append-child vtree parent-node)`: Append the DOM node associated with the `vtree` to the children of the `parent-node`
- `(m/insert-before vtree ref-node)`: Insert the DOM node associated with the `vtree` before the `ref-node`
- `(m/remove vtree)`: Remove the DOM node associated with the vtree from the DOM. It can be added back to the DOM using one of the above method
- `(m/patch vtree component)`: Patch the `vtree` using `component`
- `(m/patch vtree component props)`: Patch the `vtree` using `component`, passing the parameter `props` to `component`
- `m/defcomp`: Define a component. Use it like `defn`, with the limitation that `defcomp` takes zero or one parameter
- `(m/vnode)`: Get the current vnode. Must be called inside a render loop.
- `(m/state)`: Get the local state of the current component. Must be called inside a render loop.


### Asynchronous rendering

By default, Muance renders vtrees asynchronously, using [requestAnimationFrame](https://developer.mozilla.org/en-US/docs/Web/API/window/requestAnimationFrame).
Rendering can be made synchronous by passing false to `muance.core/vtree`.

A function can be registered to be executed after the next 
Muance render pass using the `(post-render vnode f arg1 arg2 arg3)` function.
The `post-render` function takes a vnode or a vtree as first argument. The
second argument is the function to be executed after the next render pass.
The third to fifth arguments are optional and are additional parameters passed
to the `f` function.

## Components

Components are defined using the `muance.core/defcomp` macro. Components can take zero or one parameter. Components parameters are called *props*. 
There is no limitation in the number of DOM nodes that a component can create. It can be zero, one or multiple nodes.

```
;; Creates two div nodes
(m/defcomp foo [props]
  (h/div)
  (h/div))
```

Components are called like functions and take a *key* as an optional first parameter. The *key* is used during [child nodes reconciliation](#child-nodes-reconciliation).

```
(foo key props) ;; Calls the component foo with a key and some props
```

Components are stateful. A Component is re-rendered when one of its props or local state changes.
The value of components local state can be retrieved using the `(muance.core/state)` function and can be used in the component body or one of its [lifecycle hooks](#lifecycle-hooks) methods.

Components local state is an [atom](https://clojure.org/reference/atoms). The atom is passed as a parameter to [event handlers](#event-handlers) and several of the component [lifecycle hooks](#lifecycle-hooks). Changing the value of the atom marks the component as needed to be re-rendered.


## Nodes

HTML elements and svg elements are defined as macros in the `muance.h` namespace. Their parameters are a variable number of key/value pairs attributes and a body:

```
(h/div 
  :class "div-class"                      ;; a class attribute
  :style {:color "black"}                 ;; css styles
  :muance.core/key "div-key"              ;; the key used during child nodes reconciliation
  :muance.core/on [:click click-handler]  ;; an event handler
  :muance.core/hooks {:did-mount (fn [])} ;; lifecycle hooks
  (h/p)                                   
  (h/p))                                  ;; other nodes
```

Node macros can only be used during a render pass started by the `muance.core/patch` function.

### Attributes

Attributes are a set of keyword/value pairs. 

The following attributes have special meaning: 

#### :class
Sets the *class* attribute on the node. The value can be a string (a unique *class*), a literal vector (multiple *classes*) or nil (no *class*).
```
(h/div :class [c1 c2])
```

#### :style

Sets the css styles of the node. The value must be a literal map.

```
(h/div :style {:color "black"})
```

#### :muance.core/key

A string used during [child nodes reconciliation](#child-nodes-reconciliation).

#### :muance.core/on

Sets one or multiple event handlers on the node. The value must be a literal vector (one event handler), or a collection of literal vectors (multiple event handlers).

See [event handlers](#event-handlers).

#### :muance.core/hooks

Sets the node lifecycle hooks. The value must be a literal map.

See [lifecycle hooks](#lifecycle-hooks).

#### Removing attributes

Use the `nil` value to unset an attribute:
 
 ```
 (h/div :class (when set-class? "div-class")) ;; Conditionally set the node class
 ```
 
#### Custom attributes
 
Muance attributes are set either as properties, or as attributes of DOM nodes.
The choice between a property and an attribute is automatically made by Muance based on the attribute name and whether it is used in an svg element or not. 

If you want to force a key/value pair to be set as an attribute instead of a property, you must namespace the attribute keyword with the `muance.attribute` namespace.

```
(h/div :muance.attribute/my-custom-attribute "foo") ;; <div my-custom-attribute="foo"></div>
```

#### Custom DOM nodes

The `muance.h` namespace defines macros for several standard HTML elements.
If you want to create an element that is not already in the `muance.h` namespace, you must define a custom element using the `muance.core/make-element-macro` macro. `muance.core/make-element-macro` defines a new macro and as such, must be used in a Clojure file.

```
;; foo.clj

(ns foo
  (:require [muance.core :as m]))

(m/make-element-macro custom-tag) ;; Defines a "custom-tag" macro
```

```
;; bar.cljs

(ns bar
  (:require [muance.core :as m])
  (:require-macros [foo]))

(m/defcomp component []
  (foo/custom-tag)) ;; <custom-tag></custom-tag>
```

### Virtual node API

The following functions can be used to retrieve informations about a virtual node. 
They expect the current virtual node, which can be accessed using the `(muance.core/vnode)` function when patching the virtual DOM.
 - `(muance.core/component-name vnode)`: Returns the fully qualified name of the node's component, as a string. This may be useful for logging.
 
 ```
 (muance.core/component-name (m/vnode)) ;; "cljs.user/foo"
 ```
 
 - `(muance.core/dom-node vnode)`: Returns the DOM node associated with the current virtual node. For components creating multiple nodes, this returns the DOM node of its first child. 
 - `(muance.core/dom-nodes vnode)`: Returns an array of all the DOM nodes associated with the current virtual node. This is only useful for components that create multiple nodes.
  
  ```
  (muance.core/dom-nodes (m/vnode)) ;; #js [#object[HTMLDivElement [object HTMLDivElement]]]
  ```
  
 - `(muance.core/key vnode)`: Returns the key of the current virtual node. See [child nodes reconciliation](#child-nodes-reconciliation).
 - `(muance.core/set-timeout vnode f millis)`: Execute f after a delay expressed in milliseconds. The first argument of f is the local state reference of the vnode component.
 - `(muance.core/set-interval vnode f millis)`: Periodically execute f. The period is expressed in milliseconds. The first argument of f is the local state reference of the vnode component.

### Text nodes

`(muance.core/text & text)`: Creates a text node with the string concatenation of its arguments.

```
(h/p (m/text "foo " "bar")) ;; <p>foo bar</p>
```

String literals inside DOM nodes macros are implicitly converted to text nodes:
```
(h/p "text-context") ;; <p>text-content</p>
```

```
;; This is NOT a string literal
(h/p (when true "text-context")) ;; <p></p>
```

## Child nodes reconciliation

When reordering a set of child nodes, Muance destroys the nodes and recreates them at their new position.
Recreating nodes has the following consequences:
 
 - The local state of reordered components is lost
 - The state of the reordered DOM nodes (focus state, scroll position ...) may be lost
 - Performance may suffer
 
Recreating child nodes can be avoided by providing them a special *key* parameter.
The *key* is used by Muance to match child nodes identity between render pass.

The *key* parameter can be set on nodes using the `:muance.core/key` attribute:
 
```
(h/div :muance.core/key "some-key")
```
 
The *key* parameter can be set on components using an optional first parameter:
  
```
(foo-component "some-key" props)
```

Child nodes reconciliation only works amongs nodes that share the same parent.

Duplicate keys are forbidden. Muance prints an error message when duplicate keys are detected:

```
(h/div
  (h/p ::m/key "1")
  (h/p ::m/key "1")) ;; prints: "Duplicate key: 1 in component cljs.user/foo"
```

Child nodes reconciliation will not work in the following situations, even if keys are provided:

- A *key* cannot be set on two components of different types
- A *key* cannot be set on two nodes that do not share the same definition

To illustrate the second point:

```
(h/div
  (if x
    (do (h/p ::m/key "1") (h/p ::m/key "2"))
    (do (h/p ::m/key "2") (h/p ::m/key "1"))) ;; Child nodes reconciliation does not apply
```

```
(let [child (fn [k] (h/p ::m/key k))]
  (h/div
    (if x
      (do (child "1") (child "2"))
      (do (child "2") (child "1")))) ;; Child nodes reconciliation applies
```

Child nodes reconciliation is often used with `doseq` loops:

```
(doseq [k node-keys]
  (h/p ::m/key k))
```


## Event handlers

The `:muance.core/on` [attribute](#attributes) attaches one or multiple event handlers on a node. 
Its value must be a literal vector (one event handler) or a collection of literal vectors (multiple event handlers).

An event handler is a literal vector which first element is the name of the event, as a keyword, and second element the event handler function.
The other vector elements are additional parameters passed to the event handler.

```
[:click (fn [e state-ref param1 param2 param3]) "param1" "param2" "param3"]
```

- `e` is the event object
- `state-ref` is the atom representing the local state of the node's component

The event handler function can take up to three parameters, in addition to the event object and the local state reference.

Use a collection of event handlers to attach multiple event handlers:

```
(h/input ::m/on [[:click click-handler]
                 [:blur blur-handler]])
```

Setting the event handler function to `nil` removes the event handler:

```
(h/div ::m/on [:click (when listen-click? click-handler)]) ;; conditionally attach a click handler
```

A component's local state value can be modified in an event handler:

```
(h/div ::m/on [:click (fn [e state-ref] (swap! state-ref inc))])
```

## Lifecycle hooks

The `:muance.core/hooks` [attribute](#attributes) sets a set of lifecycle hooks on a node.

Hooks can also be defined on components with the following syntax. Note that the map of hooks must be a literal map:
```
(m/defcomp foo-component
  ::m/hooks {...}
  []
  (h/div))
```
All nodes and components support the following lifecycle hooks:

#### did-mount

Called after the component or node has been created and attached to the DOM. 
Parents `did-mount` hooks are called *before* their children's.

```
(m/h ::m/hooks {:did-mount (fn [props state]))})
```

```
(m/defcomp foo-component
  ::m/hooks {:did-mount (fn [props state])}
  []
  (h/div))
```

- `props`: the props of the node's component
- `state`: the local state value of the node's component'

#### will-update

Called before the node or component is updated.

```
(m/h ::m/hooks {:will-update (fn [props state]))})
```

```
(m/defcomp foo-component
  ::m/hooks {:will-update (fn [props state])}
  []
  (h/div))
```

- `props`: the props of the node's component
- `state`: the local state value of the node's component

#### did-update

Called after the node or component is updated.

```
(m/h ::m/hooks {:did-update (fn [props state]))})
```

```
(m/defcomp foo-component
  ::m/hooks {:did-update (fn [props state])}
  []
  (h/div))
```

- `props`: the props of the node's component
- `state`: the local state value of the node's component

#### will-unmount

Called before the component or node is removed from the DOM. 
Parents `will-unmount` hooks are called *after* their children's.

```
(m/h ::m/hooks {:will-unmount (fn [props state]))})
```

```
(m/defcomp foo-component
  ::m/hooks {:will-unmount (fn [props state])}
  []
  (h/div))
```

- `props`: the props of the node's component
- `state`: the local state value of the node's component

#### remove-hook

Animating the removal of a DOM node requires the DOM node to be kept 
in the DOM for the duration of the animation. Use this hook to prevent
from Muance to remove a DOM node. Removing the DOM node is left as the developer
responsability.

```
(m/h ::m/hooks {:remove-hooks (fn [rem-node]))})
```

```
(m/defcomp foo-component
  ::m/hooks {:remove-hooks (fn [rem-node]))}
  []
  (h/div))
```

- `rem-node`: The DOM node that would normally have been removed by Muance if the hook
would not have been implemented. Note that this node is not necessarily the same than
the one the hook is attached to. It can be a node higher in the DOM tree.

`remove-hook` does nothing if another `remove-hook` hook as already been implemented
by one of the ancestors of the vnode.

The helper function `remove-node` can be used to remove a node from the DOM.

- (m/remove-node dom-node)

### Components lifecycle hooks

The following lifecycle hooks can be set on components only:

#### get-initial-state

Called before the component has been created.
The value returned by `get-initial-state` is used as the initial value of the component's local sate. 
The local state initial value is `nil` if `get-initial-state` is not defined.

```
(m/defcomp foo-component
  ::m/hooks {:get-initial-state (fn [props] "initial-state")}
  []
  (h/div))
```

- `props`: the props of the node's component

#### will-receive-props

Called before the node or component is updated.
Use `will-receive-props` to update the component's local state in response to props change.

```
(m/defcomp foo-component
  ::m/hooks {:will-receive-props (fn [prev-props props state-ref]
                                   (reset! state-ref "state-value"))}
  []
  (h/div))
```

- `prev-props`: the previous props of the node's component
- `props`: the props of the node's component
- `state-ref`: the local state of the node's component (an atom)

## Side effectful API pitfalls

The muance API is side effectful. Watch out for potential issues:

### Avoid lazyness

All the calls to the API must be executed eagerly.

```
(h/div
  (for [x xs]         ;; Wrong!
    (h/p (m/text x)))
```

```
(h/div
  (doseq [x xs]       ;; Right
    (h/p (m/text x)))
```

### Wrap child nodes parameters inside functions
 
Function parameters are evaluated before being passed to functions.

```
(defn foo [child]
  (h/div child))

(foo (h/p)) ;; Wrong !
```

```
(defn foo [child]
  (h/div (child)))

(foo #(h/p)) ;; Right
```

---

## License

Copyright 2017 Ewen Grosjean.

The use and distribution terms for this software are covered by the
Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0)
which can be found in the file [epl-v10.html](epl-v10.html) at the root of this distribution.

By using this software in any fashion, you are agreeing to be bound by
the terms of this license.

You must not remove this notice, or any other, from this software.