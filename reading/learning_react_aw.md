# [Learning React][homepage] by [Kirupa Chinnathambi][author], Addison-Wesley (2016)

[almost all book content and examples][almost_all_book_content]<br>
[React Training from the creators of React Router][react_training]

[homepage]: http://www.informit.com/store/learning-react-9780134546315
[author]: https://www.kirupa.com/
[almost_all_book_content]: https://www.kirupa.com/react/
[react_training]: https://reacttraining.com/

## 1. Introducing React

When building single-page apps, there are three major issues that you'll
 encounter:
* In a single-page application, the bulk of your time will be spent keeping your
  data in sync with your UI.
* Manipulating the DOM is really REALLY slow.
* Working with HTML templates can be a pain.

With React, you need to worry only about one thing: *the final state your UI is
 in*. React takes care of everything else.<br>
React is not a full-fledged framework that has an opinion on how everything in
 your app should behave. Instead, React works primarily in the View layer where
 all of its worries and concerns revolve around your visual elements and keeping
 them up to date. This means you are free to use whatever you want for the M and
 C part of your MVC architecture.<br>
[Sites Using React][sites_using_react]

[sites_using_react]: https://github.com/facebook/react/wiki/Sites-Using-React

## 2. Building Your First React App

To build a web app using React, we need a way to take our JSX and convert it
 into plain old JavaScript that your browser can understand.
* Set up a development environment around Node and a handful of build-tools.
* Let your browser rely on a JavaScript library to automatically convert JSX to
  something it understands.

## 3. Components in React

In React, you can't output multiple adjacent elements as shown in the following.

```javascript
var HelloWorld = React.createClass({
  render: function() {
    return (
      <p>Hello, {this.props.greetTarget}!</p>
    );
  }
});

ReactDOM.render(
  <HelloWorld greetTarget="Batman"/>,
  document.querySelector("#container")
);
```

If your child element is the root of a deeply nested structure, the
 `this.props.children` property will return an array. If your child element is
 just a single element, the `this.props.children` property returns a single
 component NOT wrapped inside an array.

## 4. Styling in React

React favors an inline approach for styling content that doesn't use CSS.<br>
When specifying a unit-less CSS value as a string, a future version will not add
 `px` automatically. ([React v15.0][react_v15_0])

```javascript
var Letter = React.createClass({
  render: function() {
      var letterStyle = {
        padding: 10, // React will add the `px` suffix automatically
        margin: 10,
        backgroundColor: "#ffde00", // background-color
        color: "#333",
        display: "inline-block",
        fontFamily: "monospace",
        fontSize: "32",
        textAlign: "center"
      };

      // instead of <div className="letter">
      return {
        <div style={letterStyle}>
          {this.props.children}
        </div>
      };
    }
});
```

[react_v15_0]: https://facebook.github.io/react/blog/2016/04/07/react-v15.html 

## 5. Creating Complex Components

The spread operator enabled you to unwrap an array into its individual elements.
 `printStuff(...items)` == `printStuff(items[0], items[1], items[2])`<br>
As designed by the ES6/ES2015 committee, the spread operator is designed to work
 only on arrays and array-like creatures (aka that which has a `Symbol.iterator`
 property). The fact that it works on object literal like our `props` object is
 due to React extending the standard. As of now, no browser currently supports
 using the spread object on object literals.

```javascript
var Shirt = React.createClass({
  render: function() {
      return (
        <div>
          <Label {...this.props}/>
        </div>
      );
    }
});
```

## 6. Transferring Properties (Props)

## 7. Meet JSX--Again!

In JSX, the `style` attribute can't contain CSS inside it. Instead, it needs to
 refer to an object that contains styling information instead.<br>
[the full list of supported tags and attributes][dom_elements_reference]
> ... our thinking is that JSX's primary advantage is the symmetry of matching
> closing tags which make code easier to read, not the direct resemblance to
> HTML or XML. It's convenient to copy/paste HTML directly, but other minor
> differences (in self-closing tags, for example) make this a losing battle and
> we have a [HTML to JSX converter][html_to_jsx_converter] to help you anyway.
> Finally, to translate HTML to idiomatic React code, a fair amount of work is
> usually involved in breaking up the markup into components that make sense, so
> changing `class` to `className` is only a small part of that anyway. --
> [source][ben_alpert_answer]

To represent HTML elements, ensure the HTML tag is lower-case. When wishing to
 represent components, the component name must be capitalized, both in JSX as
 well as when you define them.<br>
The most important thing to remember is that **JSX is not HTML**. It looks like
 HTML and behaves like it in many common scenarios, but it is ultimately
 designed to be translated into JavaScript.

```javascript
ReactDOM.render(
  <div class="slideIn">
    <p class="emphasis">Gabagool!</p>
    {/* I am a child comment /*}
    <Label
      /* This comment
          goes across
          multiple lines */
          clssName="colorCard" // end of line
    />
  </div>,
  document.querySelector("#container")
);
```

[dom_elements_reference]: https://facebook.github.io/react/docs/dom-elements.html
[html_to_jsx_converter]: http://magic.reactjs.net/htmltojsx.htm
[ben_alpert_answer]: https://www.quora.com/Why-do-I-have-to-use-className-instead-of-class-in-ReactJs-components-done-in-JSX/answer/Ben-Alpert?srid=QYyI&share=1ec9dffa

## 8. Dealing with State

```javascript
var LightningCounter = React.createClass({
  getInitialState: function() {
    return {
      strikes: 0
    };
  },
  timerTick: function() {
    this.setState({
      strikes: this.state.strikes + 100
    });
  },
  componentDidMount: function() {
    setInterval(this.timerTick, 1000);
  },
  render: function() {
    return (
      <h1>{this.state.strikes}</h1>
    );
  }
});
```

In regular JavaScript, the timerTick function won't maintain context. You have
 to do extra work to support that. The reason it works in the React world is
 because of something known as **autobinding**.<br>
`Number.prototype.toLocaleString()`

## 9. Going from Data to UI

When you create elements dynamically, these identifiers are not automatically
 set. On each component, we specify our `key` prop and set its value to a
 combination of color and index position inside the `colors` array. This ensures
 that each component we dynamically create ends up getting a unique identifier
 that React can then use to optimize any future UI updates.

## 10. Working with Events

Your event handlers always get event arguments of type `SyntheticEvent` that
 wrap your browser's native event instead. Refer to the
 [React Event System document][react_event_system].<br>
You simply can't listen for events on them directly. The reason is because
 components are wrappers for DOM elements.<br>
Not all DOM events have `SyntheticEvent` equivalents. For those events that
 aren't officially recognized by React, you have to use the traditional approach
 that uses `addEventListener` with a few extra hoops to jump through.

```javascript
var Something = React.createClass({
  handleMyEvent: function(e) {
    // do something
  },
  componentDidMount: function() {
    window.addEventListener("someEvent", this.handleMyEvent);
  },
  componentWillUnmount: function() {
    window.removeEventListener("someEvent", this.handleMyEvent);
  },
  render: function() {
      return (
        <div>Hello!</div>
      );
    }
});
```

In the non-React world, the value of `this` inside an event handler refers to
 the element that your event is listening on. In the React world, the value of
 `this` inside your event handler always refers to the *component the event
 handler lives in*. This autobinding behavior only applies when your component
 is created using `React.createClass`. If you are using ES6 classes to define
 your components, the value of `this` inside your event handler is going to be
 undefined unless you explicitly bind it yourself:
 `<button onClick={this.increase.bind(this)}>+</button>`<br>
Let's talk about why React decided to deviate from how we've worked with events
 in the past. There are two reasons: Browser Compatibility and Improved
 Performance. *It uses one event handler at the root of your document* that is
 responsible for listening to all events and calling the appropriate event
 handler as necessary.

[react_event_system]: https://facebook.github.io/react/docs/events.html

## 11. The Component Lifecycle

`getDefaultProps` → `getInitialState` → (component is created) →
 `componentWillMount` → `render` → (gets placed on the DOM) →
`componentDidMount`<br>
If you don't wish to `render` anything (for some fancy optimization you might be
 going for), simply return `null` or `false`. With the exception of the `render`
 method, all of these lifecycle methods **can fire only once**.

`componentWillReceiveProps` (props change only) → `shouldComponentUpdate` →
 `componentWillUpdate` → `render` → `componentDidUpdate`<br>
You can't change your state by calling `this.setState` from the
 `componentWillUpdate` method.

`componentWillUnmount` → component is removed from the DOM

```javascript
shouldComponentUpdate: function(newProps, newState) {
  if (newState.count < 5) {
    return true;
  } else {
    ReactDOM.unmountComponentAtNode(destination);
    return false;
  }
},
```

## 12. Accessing DOM Elements

To provide a bridge between JSX and the final HTML elements in the DOM, React
 provides us with something funnily known as *refs* (short for
 *references*): `<button refs="myButton">Click me!</button>`. You could then
 access this element after the component was mounted by doing something like
 `this.refs.myButton`. This string-based approach is likely to be
 deprecated.<br>
What you typically set as the `ref` attribute's value is a JavaScript callback
 function. This function gets called automatically when the component housing
 this render method gets mounted.

```javascript
var self = this;
return (
  <input
      ref={
            function(el) {
              self._input = el;
            }
          }
      onChange={this.colorValue}
      placeholder="Enter a color value">
  </input>
);

// simplifying further with ES6 arrow functions,
//  because of how arrow functions deal with scope
return (
  <input
      ref={
            (el) => this.input = el
          }
      onChange={this.colorValue}
      placeholder="Enter a color value">
  </input>
);
```

## 13. Creating a Single-Page App Using [React Router][react_router]

```javascript
// ES6 trick where you can manually specify which values will automatically get prefixed
var { Router,
      Route,
      IndexRoute,
      IndexLink,
      Link } = ReactRouter;

ReactDOM.render(
  <Router>
    <Route path="/" component={App}>
      <IndexRoute component={Home}/>
      <Route path="stuff" component={Stuff} />
      <Route path="contact" component={Contact} />
    </Route>
  </Router>,
  destination
);

var App = React.createClass({
  render: function() {
    return (
      <div>
        <h1>Simple SPA</h1>
        <ul className="header">
          <li><IndexLink to="/" activeClassName="active">Home</IndexLink></li>
          <li><Link to="/stuff" activeClassName="active">Stuff</Link></li>
          <li><Link to="/contact" activeClassName="active">Contact</Link></li>
        </ul>
        <div className="content">
          {this.props.children}
        </div>
      </div>
    )
  }
});
```

[react_router]: https://github.com/ReactTraining/react-router

## 14. Building a Todo List App

```javascript
addItem: function(e) {
  var itemArray = this.state.items;

  itemArray.push(
    {
      text: this._inputElement.value,
      key: Date.now() // to uniquely identify each generated UI element
    }
  );

  this.setState({
    items: itemArray
  });

  e.preventDefault(); // override the default onSubmit event
}
```

## 15. Setting Up Your React Development Environment

[online version of this article][setting_up_react_environment]

```javascript
import React from "react";
import ReactDOM from "react-dom";

// webpack.config.js
var webpack = require("webpack");
var path = require("path");

var DEV = path.resolve(__dirname, "dev");
var OUTPUT = path.resolve(__dirname, "output");

var config = {
  entry: DEV + "/index.jsx",
  output: {
    path: OUTPUT,
    filename: "myCode.js"
  },
  module: {
    loaders: [{
        include: DEV,
        loader: "babel",
    }]
  }
};

module.exports = config;
```

```
npm install babel-loader babel-preset-es2015 babel-preset-react --save

in package.json:
  "babel": {
    "presets": [
      "es2015",
      "react"
    ]
  }
```

[setting_up_react_environment]: https://www.kirupa.com/react/setting_up_react_environment.htm

## 16. The End

## 17. (Web Edition) Working With External Data

[online version][working_with_external_data]<br>
two types of components:
 [Presentational and Container Components][presentational_and_container_components]
 (that perform some under-the-covers processing)

```javascript
var xhr;

var IPAddressContainer = React.createClass({
  getInitialState: function() {
    return {
      ip_address: "..."
    };
  },
  componentDidMount: function() {
    xhr = new XMLHttpRequest();
    xhr.open('GET', "https://ipinfo.io/json", true);
    xhr.send();
    xhr.addEventListener("readystatechange", this.processRequest, false);
  },
  processRequest: function() {
    if (xhr.readyState == 4 && xhr.status == 200) {
        var response = JSON.parse(xhr.responseText);
        this.setState({
          ip_address: response.ip
        });
    }
  },
  render: function() {
    return (
        <IPAddressDisplay ip={this.state.ip_address}/>
    );
  }
});
```

[working_with_external_data]: https://www.kirupa.com/react/working_with_external_data.htm
[presentational_and_container_components]: https://medium.com/@dan_abramov/smart-and-dumb-components-7ca2f9a7c7d0

## 18. (Web Edition) Creating a Sliding Menu Using React Motion

[online version][smooth_sliding_menu_react_motion],
 [React-Motion blog post][react_motion_blogpost],
 [Learn Web Animation][learn_web_animation] (by the author)

```
#roundButton {
    background-color: #96D9FF;
    margin-bottom: 20px;
    width: 50px;
    height: 50px;
    border-radius: 50%;
    border: 10px solid #0065A6;
    outline: none;
    transition: all .2s cubic-bezier(0, 1.26, .8, 1.28);
}

<Motion style={{x: spring(visible ? -100 : 0)}}>
  {
    function({x}) {
      return (
          <div style={{ transform: "translate3d(" + x + "vw, 0vw, 0)" }}>
          </div>
      );
    }
  }
</Motion>
```

[smooth_sliding_menu_react_motion]: https://www.kirupa.com/react/smooth_sliding_menu_react_motion.htm
[react_motion_blogpost]: https://medium.com/@chenglou/react-motion-and-animated-4b3edf671cba
[learn_web_animation]: https://www.kirupa.com/html5/learn_animation.htm

