# Muance

A virtual dom library for Clojurescript, featuring:

- Stateful components
- Lifecycle hooks can be set on any node
- Deterministic parent/children call order of lifecycle hooks
- No synthetic event system
- svg support
- Asynchronous rendering

Muance exposes a side effectful API which mutates an in memory representation of a virtual dom. 

## Quick start

Creates a component and renders it on the page:

```
(ns muance.example
  (:require [muance.core :as m :include-macros true])
  (:require-macros [muance.h :as h]))
  
(m/defcomp example-component []
  (h/div "my first component"))
  

(def vtree (m/vtree)) ;; Creates a virtual DOM tree 
(m/append-child vtree (.-body js/document)) ;; Appends the virtual DOM tree to the page
(m/patch vtree example-component) ;; Renders the component
```

Muance API is defined in the `muance.core` namespace. DOM tag names are defined in the `muance.h` namespace.
- `(muance.core/vtree)`: Creates a new vtree
- `(m/append-child vtree parent-node)`: Append the real node associated with the `vtree` to the children of the `parent-node`
- `(m/insert-before vtree ref-node)`: Insert the real node associated with the `vtree` before the `ref-node`
- `(m/patch vtree component)`: Patch the `vtree` using the `component`

## Todo app

See the [todo example](https://github.com/EwenG/muance/tree/master/examples/todo).

## Components

Components can take zero or one parameter. Components parameters are called *props*. Components can create zero, one or multiple DOM nodes.

```
;; Creates two div nodes
(m/defcomp foo [props]
  (h/div)
  (h/div))
```

Components are stateful. The value of components local state is bound to the `muance.core/*state*` var and can be accessed in the component or one of its lifecycle hooks method.

Components takes a *key* as an optional first parameter. The *key* is used during [child nodes reconciliation]().
 
```
(foo key props)
```

## Nodes

DOM elements and svg elements are defined as macros in the `muance.h` namespace. DOM node macros are passed attributes (key/value pairs) and a body:

```
(h/div 
  :class "div-class"                      ;; a class attribute
  :style {:color "black"}                 ;; css styles
  :muance.core/key "div-key"              ;; the key is used during child nodes reconciliation
  :muance.core/on [:click click-handler]  ;; an event handler
  :muance.core/hooks {:did-mount (fn [])} ;; lifecycle hooks
  (h/p)                                   
  (h/p))                                  ;; other nodes
```

DOM nodes macros can only be used inside a render loop created by the `muance.core/patch` function.

### Attributes

Attributes are set as keyword/value pairs. 

The following attributes have a special meaning: 

#### :class
Sets the *class* attribute on the node. The value can be a string (a unique *class*) or a literal vector (multiple *classes*). 
```
(h/div :class [c1 c2])
```

#### :style

Sets the css styles of the node. Takes a literal map as value.

#### :muance.core/key

Used during [child nodes reconciliation]().

#### :muance.core/on

Sets one or multiple event handlers on the node. Takes a literal vector (one event handler) or a collection of literal vectors (multiple event handlers).
An event handlers must be defined as follows:

```
[event-name handler-function param1 param2 param3]
```
Where:


`event-name` is a keyword which name is the event name, as used by `addEventListener`

`handler-function` is the event handler function. The event handler function takes the following parameters:
 

```

```

#### :muance.core/hooks

Sets the node lifecycle hooks. See [lifecycle hooks]().

#### Removing attributes

Use `nil` as an attribute value when you want to unset an attribute:
 
 ```
 (h/div :class (when set-class? "div-class")) ;; Conditionally set the node class
 ```

### Virtual node API

### Text nodes

`(muance.core/text & text)`: Creates a text node with the string concatenation of its arguments.

String literals inside DOM nodes macros are implicitly converted to text nodes:
```
(h/p "text-context")
```

### Hooks



