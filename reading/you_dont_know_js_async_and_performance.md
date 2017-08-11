# [You Don't Know JS: Async & Performance][homepage] by Kyle Simpson, O'Reilly (2015)

[read online][read_online], under the [Creative Commons Attribution-NonCommercial-NoDerivs 4.0 Unported][cc_noncommercial_nonderivatives_4_0] license

[homepage]: http://shop.oreilly.com/product/0636920033752.do
[read_online]: https://github.com/getify/You-Dont-Know-JS/tree/master/async%20&%20performance
[cc_noncommercial_nonderivatives_4_0]: https://creativecommons.org/licenses/by-nc-nd/4.0/

## Series Preface

Where it takes a pretty in-depth knowledge of a language like C or C++ to write
 a full-scale program, full-scale production JavaScript can, and often does,
 barely scratch the surface of what the language can do.

## Chapter 1: Asynchrony: Now & Later

It's important to note that `setTimeout(..)` doesn't put your callback on the
 event loop queue. What it does is set up a timer; when the timer expires, the
 environment places your callback into the event loop.<br>
We use the `setTimeout(..0)` (hack) for async scheduling. `setTimeout(..0)` is
 not technically inserting an item directly onto the event loop queue. The timer
 will insert the event at its next opportunity. In Node.js, a similar approach
 is `process.nextTick(..)`.<br>
As of ES6, there's a new concept layered on top of the event loop queue, called
 the "Job queue." The "Job queue" is a queue hanging off the end of every tick
 in the event loop queue.

## Chapter 2: Callbacks

If you have code that uses callbacks, especially but not exclusively with
 third-party utilities, and you're not already applying some sort of mitigation
 logic for all these *inversion of control* trust issues, your code *has* bugs
 in it right now even though they may not have bitten you yet.<br>
What about the trust issue of never being called? If this is a concern (and it
 probably should be!), you likely will need to set up a timeout that cancels the
 event.<br>
[Don't Release Zalgo!][dont_release_zalgo] (always be asyncing.)

```javascript
function timeoutify(fn,delay) {
	var intv = setTimeout( function(){
			intv = null;
			fn( new Error( "Timeout!" ) );
		}, delay )
	;

	return function() {
		// timeout hasn't happened yet?
		if (intv) {
			clearTimeout( intv );
			fn.apply( this, [ null ].concat( [].slice.call( arguments ) ) );
		}
	};
}

function asyncify(fn) {
	var orig_fn = fn,
		intv = setTimeout( function(){
			intv = null;
			if (fn) fn();
		}, 0 )
	;

	fn = null;

	return function() {
		// firing too quickly, before `intv` timer has fired to
		// indicate async turn has passed?
		if (intv) {
			fn = orig_fn.bind.apply(
				orig_fn,
				// add the wrapper's `this` to the `bind(..)`
				// call parameters, as well as currying any
				// passed in parameters
				[this].concat( [].slice.call( arguments ) )
			);
		}
		// already async
		else {
			// invoke original function
			orig_fn.apply( this, arguments );
		}
	};
}
```

[dont_release_zalgo]: https://github.com/oren/oren.github.io/blob/master/posts/zalgo.md

## Chapter 3: Promises

What if we could uninvert that *inversion of control*? This paradigm is called
 **Promises**. The Promise itself is time-independent, and thus Promises can be
 composed (combined) in predictable ways regardless of the timing or outcome
 underneath.  Moreover, once a Promise is resolved, it becomes an *immutable
 value*.<br>
[revealing constructor pattern][revealing_constructor_pattern]

```javascript
// a utility for timing out a Promise
function timeoutPromise(delay) {
	return new Promise( function(resolve,reject){
		setTimeout( function(){
			reject( "Timeout!" );
		}, delay );
	} );
}

// setup a timeout for `foo()`
Promise.race( [
	foo(),					// attempt `foo()`
	timeoutPromise( 3000 )	// give it 3 seconds
] ).then( // ...

var p1 = Promise.resolve( 42 );
var p2 = Promise.resolve( p1 );
p1 === p2; // true

function delay(time) {
	return new Promise( function(resolve,reject){
		setTimeout( resolve, time );
	} );
}
```

`Promise.resolve(..)` will accept any thenable, and will unwrap it to its
 non-thenable value. Another beneficial side effect of wrapping
 `Promise.resolve(..)` around any function's return value (thenable or not) is
 that it's an easy way to normalize that function call into a well-behaving
 async task.<br>
An error/exception is on a per-Promise basis, which means it's possible to catch
 such an error at any point in the chain, and that catching acts to sort of
 "reset" the chain back to normal operation at that point. The assumed (if
 omitted or any other non-function value passed) rejection handler simply
 rethrows the error. The default fulfillment handler simply passes whatever
 value it receives along to the next step.<br>
`try..catch` construct is synchronous-only, so it fails to help in async code
 patterns. Promises don't use the popular "error-first callback" design style,
 but instead use "split callbacks" style.<br>
Browsers can track Promise objects, and whenever they get garbage collected, if
 they have a rejection in them, the browser knows for sure this was a legitimate
 "uncaught error," and can thus confidently know it should report it to the
 developer console. At the time of this writing, both Chrome and Firefox have
 early attempts at that sort of "uncaught rejection" capability, though support
 is incomplete at best.<br>
Promises cannot be canceled so they can only be silently ignored.

```javascript
// polyfill-safe guard check
if (!Promise.observe) {
	Promise.observe = function(pr,cb) {
		// side-observe `pr`'s resolution
		pr.then(
			function fulfilled(msg){
				// schedule callback async (as Job)
				Promise.resolve( msg ).then( cb );
			},
			function rejected(err){
				// schedule callback async (as Job)
				Promise.resolve( err ).then( cb );
			}
		);

		// return original promise
		return pr;
	};
}

Promise.race( [
	Promise.observe(
		foo(),					// attempt `foo()`
		function cleanup(msg){
			// clean up after `foo()`, even if it
			// didn't finish before the timeout
		}
	),
	timeoutPromise( 3000 )	// give it 3 seconds
] )

// polyfill-safe guard check
if (!Promise.first) {
	Promise.first = function(prs) {
		return new Promise( function(resolve,reject){
			// loop through all promises
			prs.forEach( function(pr){
				// normalize the value
				Promise.resolve( pr )
				// whichever one fulfills first wins, and
				// gets to resolve the main promise
				.then( resolve );
			} );
		} );
	};
}

if (!Promise.map) {
	Promise.map = function(vals,cb) {
		// new promise that waits for all mapped promises
		return Promise.all(
			// note: regular array `map(..)`, turns
			// the array of values into an array of
			// promises
			vals.map( function(val){
				// replace `val` with a new promise that
				// resolves after `val` is async mapped
				return new Promise( function(resolve){
					cb( val, resolve );
				} );
			} )
		);
	};
}

var p1 = Promise.resolve( 21 );
var p2 = Promise.resolve( 42 );
var p3 = Promise.reject( "Oops" );

// double values in list even if they're in
// Promises
Promise.map( [p1,p2,p3], function(pr,done){
	// make sure the item itself is a Promise
	Promise.resolve( pr )
	.then(
		// extract value as `v`
		function(v){
			// map fulfillment `v` to new value
			done( v * 2 );
		},
		// or, map to promise rejection message
		done
	);
} )
.then( function(vals){
	console.log( vals );	// [42,84,"Oops"]
} );

// polyfill-safe guard check
if (!Promise.wrap) {
	Promise.wrap = function(fn) {
		return function() {
			var args = [].slice.call( arguments );

			return new Promise( function(resolve,reject){
				fn.apply(
					null,
					args.concat( function(err,v){
						if (err) {
							reject( err );
						}
						else {
							resolve( v );
						}
					} )
				);
			} );
		};
	};
}
```

Many Promise abstraction libraries provide facilities to cancel Promises, but
 this is a terrible idea! This violates the future-value's trustability
 (external immutability), but moreover is the embodiment of the
 "[action at a distance][action_at_a_distance]" anti-pattern. Regardless of how
 useful it seems, it will actually lead you straight back into the same
 nightmares as callbacks. No individual Promise should be cancelable, but it's
 sensible for a *sequence* to be cancelable, because you don't pass around a
 sequence as a single immutable value like you do with a Promise.

[revealing_constructor_pattern]: https://blog.domenic.me/the-revealing-constructor-pattern/
[action_at_a_distance]: https://en.wikipedia.org/wiki/Action_at_a_distance_%28computer_programming%29

## Chapter 4: Generators

The `it = foo()` operation does not execute the `*foo()` generator yet, but it
 merely constructs an iterator that will control its execution.<br>
(`Object.keys(..)` returns an `array`, which can then be used like
 `for (var k of Object.keys(obj)) { ..`. Such a `for..of` loop over an object's
 keys would be similar to a `for..in` loop, except that `Object.keys(..)` does
 not include properties from the `[[Prototype]]` chain.)<br>
The natural way to get the most out of Promises and generators is **to `yield` a
 Promise**, and wire that Promise to control the generator's *iterator*.

```javascript
var something = (function(){
	var nextVal;

	return {
		// needed for ES6 `for..of` loops
		[Symbol.iterator]: function(){ return this; }, // computed property name

		// standard iterator interface method
		next: function(){
			if (nextVal === undefined) {
				nextVal = 1;
			}
			else {
				nextVal = (3 * nextVal) + 6;
			}

			return { done:false, value:nextVal };
		}
	};
})();

function *something() {
	try {
		var nextVal;

		while (true) {
			if (nextVal === undefined) {
				nextVal = 1;
			}
			else {
				nextVal = (3 * nextVal) + 6;
			}

			yield nextVal;
		}
	}
	// cleanup clause
	finally {
		console.log( "cleaning up!" );
	}
}

var it = something();
for (var v of it) {
	console.log( v );

	// don't let the loop run forever!
	if (v > 500) {
		console.log(
			// complete the generator's iterator
			it.return( "Hello World" ).value // the iterator is set to `done:true`.
		);
		// no `break` needed here
	}
}
// 1 9 33 105 321 969
// cleaning up!
// Hello World

function foo(x,y) {
	ajax(
		"http://some.url.1/?x=" + x + "&y=" + y,
		function(err,data){
			if (err) {
				// throw an error into `*main()`
				it.throw( err );
			}
			else {
				// resume `*main()` with received `data`
				it.next( data );
			}
		}
	);
}

function *main() {
	try {
		var text = yield foo( 11, 31 );
		console.log( text );
	}
	catch (err) {
		console.error( err );
	}
}

var it = main();

// start it all up!
it.next();

// thanks to Benjamin Gruenbaum (@benjamingr on GitHub) for
// big improvements here!
function run(gen) {
	var args = [].slice.call( arguments, 1), it;

	// initialize the generator in the current context
	it = gen.apply( this, args );

	// return a promise for the generator completing
	return Promise.resolve()
		.then( function handleNext(value){
			// run to the next yielded value
			var next = it.next( value );

			return (function handleResult(next){
				// generator has completed running?
				if (next.done) {
					return next.value;
				}
				// otherwise keep going
				else {
					return Promise.resolve( next.value )
						.then(
							// resume the async loop on
							// success, sending the resolved
							// value back into the generator
							handleNext,

							// if `value` is a rejected
							// promise, propagate error back
							// into the generator for its own
							// error handling
							function handleErr(err) {
								return Promise.resolve(
									it.throw( err )
								)
								.then( handleResult );
							}
						);
				}
			})(next);
		} );
}
```

## Chapter 5: Program Performance

## Chapter 6: Benchmarking & Tuning

## Appendix A: asynquence Library

## Appendix B: Advanced Async Patterns

