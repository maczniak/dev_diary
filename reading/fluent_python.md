# [Fluent Python][homepage] by Luciano Ramalho, O'Reilly (2015)

Python 3.4, [errata][errata], [source code][source_code]

[homepage]: http://shop.oreilly.com/product/0636920032519.do
[errata]: http://www.oreilly.com/catalog/errata.csp?isbn=9781491946008
[source_code]: https://github.com/fluentpython/example-code

## I. Prologue

### Chapter 1: The Python Data Model

["Data Model" chapter of *The Python Language Reference*][data_model_ref]<br>
special methods - dunder-getitem, dunder methods<br>
`# doctest: +ELLIPSIS`<br>
adding `__len__` and `__getitem__` -> indexing, slicing, iteration, `len`, `random.choice`, `reversed`, `sorted`<br>
in Python 3, objects inherits from `object` by default.<br>
when no custom `__str__` is available, Python will call `__repr__` as a fallback.<br>
`__bool__` -> `__len__` -> `True`

* String/bytes representation - `__repr__`, `__str__`, `__format__`, `__bytes__`
* Conversion to number - `__abs__`, `__bool__`, `__complex__`, `__int__`, `__float__`, `__hash__`, `__index__`
* Emulating collections - `__len__`, `__getitem__`, `__setitem__`, `__delitem__`, `__contains__`
* Iteration - `__iter__`, `__reversed__`, `__next__`
* Emulatin callables - `__call__`
* Context management - `__enter__`, `__exit__`
* Instance creation and destruction - `__new__`, `__init__`, `__del__`
* Attribute management - `__getattr__`, `__getattribute__`, `__setattr__`, `__delattr__`, `__dir__`
* Attribute descriptors - `__get__`, `__set__`, `__delete__`
* Class services - `__prepare__`, `__instancecheck__`, `__subclasscheck__`
* Unary numeric operators - `__neg__` `-`, `__pos__` `+`, `__abs__` `abs()`
* Rich comparison operators - `__lt__` `<`, `__le__` `<=`, `__eq__` `==`, `__ne__` `!=`, `__gt__` `>`, `__ge__` `>=`
* Arithmetic operators - `__add__` `+`, `__sub__` `-`, `__mul__` `*`, `__truediv__` `/`, `__floordiv__` `//`, `__mod__` `%`, `__divmod__` `divmod()`, `__pow__` `**` or `pow()`, `__round__` `round()`
* Reversed arithmetic operators - `__radd__`, `__rsub__`, `__rmul__`, `__rtruediv__`, `__rfloordiv__`, `__rmod__`, `__rdivmod__`, `__rpow__`
* Augmented assignment arithmetic operators - `__iadd__`, `__isub__`, `__imul__`, `__itruediv__`, `__ifloordiv__`, `__imod__`, `__ipow__`
* Bitwise operators - `__invert__` `~`, `__lshift__` `<<`, `__rshift__` `>>`, `__and__` `&`, `__or__` `|`, `__xor__` `^`
* Reversed bitwise operators - `__rlshift__`, `__rrshift__`, `__rand__`, `__rxor__`, `__ror__`
* Augmented assignment bitwise operators - `__ilshift__`, `__irshift__`, `__iand__`, `__ixor__`, `__ior__`

[data_model_ref]: https://docs.python.org/3/reference/datamodel.html

## II. Data Structures

### Chapter 2: An Array of Sequences

* container sequences - list, tuple (iterable, immutable), collections.deque
* flat sequences - str (immutable), bytes (immutable), bytearray, memoryview (slicing without copying, [tutorial][memoryview_tutorial]), array.array (number only)

list comprehension (listcomp) for `list`, generator expression (genexp) for other sequences<br>
tuple unpacking or iterable unpacking ([PEP 3132][pep_3132], [PEP 448][pep_448])<br>
`a, *body, c, d = range(5)`<br>
tuples as records -> `collections.namedtuple` (`_fields`, `_make(iterable)`, `_asdict`)<br>
`namedtuple('Card', ['rank', 'suit'])`, `namedtuple('City', 'name country population coordinates')`<br>
`DESCRIPTION = slice(6, 40)`, `item[DESCRIPTION]`<br>
[Tentative NumPy Tutorial][tentative_numpy_tutorial]<br>
[Pyhton Tutor][python_tutor] (visualize execution steps of Python/Java/JavaScript/Ruby/C/C++ programs)<br>
even `reverse` does not change the order of a stable sort.<br>
[SortedCollection][sortedcollection] wraps `bisect`.<br>
[Sorting HOW TO][sorting_howto]<br>
[Why Numbering Should Start at Zero][why_numbering_should_start_at_zero] by Edsger W. Dijkstra<br>
[Timsort][timsort] (switches from insertion sort to merge sort) by Tim Peters, a Python core developer

[memoryview_tutorial]: http://eli.thegreenplace.net/2011/11/28/less-copies-in-python-with-the-buffer-protocol-and-memoryviews/
[pep_3132]: https://www.python.org/dev/peps/pep-3132/
[pep_448]: https://www.python.org/dev/peps/pep-0448/
[tentative_numpy_tutorial]: http://scipy.github.io/old-wiki/pages/Tentative_NumPy_Tutorial
[python_tutor]: http://www.pythontutor.com/
[sortedcollection]: http://code.activestate.com/recipes/577197-sortedcollection/
[sorting_howto]: https://docs.python.org/3/howto/sorting.html
[why_numbering_should_start_at_zero]: https://www.cs.utexas.edu/users/EWD/transcriptions/EWD08xx/EWD831.html
[timsort]: https://en.wikipedia.org/wiki/Timsort

### Chapter 3: Dictionaries and Sets

hashable -> immutable<br>
dict comprehension (dictcomp), set comprehension (setcomp)<br>
The `default_factory` of a `defaultdict` is only invoked to provide default
 values for `__getitem__` calls, and not for the other methods such as `get`.
 For example, if `dd` is a `defaultdict`, and `k` is a missing key, `dd[k]`
 will call the `default_factory` to create a default value, but `dd.get(k)`
 still returns `None`.<br>
[Pingo][pingo] (uniform API to program devices like Raspberry Pi)<br>
create a new mapping type by extending `collections.UserDict` (has a dict named `data`) rather than `dict`.<br>
TransformDict ([PEP 455][pep_455])<br>
set - extremely fast membership test (due to the underlying hash table)<br>
Set elements must be hashable. The `set` type is not hashable, but `frozenset` is, so you can have `frozenset` elements unside a `set`.<br>
[dict implementation][dict_implementation] and *Beautiful Code*, Chapter 18 "Python's Dictionary Implementation: Being All Things to All People", [The Mighty Dictionary][mighty_dictionary] (PyCon 2010 Atlanta)<br>
Starting with Python 3.3, a random salt value is added to the hashes of `str`, `bytes`, and `datetime` objects.

[pingo]: http://www.pingo.io/docs/
[pep_455]: https://www.python.org/dev/peps/pep-0455/
[dict_implementation]: https://hg.python.org/cpython/file/tip/Objects/dictobject.c
[mighty_dictionary]: http://pyvideo.org/video/276/the-mighty-dictionary-55

### Chapter 4: Text versus Bytes

[Character Encoding and Unicode in Python][character_encoding_and_unicode_in_python] (PyCon 2014),
 [Pragmatic Unicode][pragmatic_unicode] (PyCon 2012),
 [Character encoding and Unicode in Python][character_encoding_and_unicode_in_python] (PyCon 2014),
 [Unicode HOWTO][unicode_howto],
 W3C [Case Folding][case_folding] and [Character Model for the World Wide Web: String Matching and Searching][character_model_for_the_world_wide_web_string_matching_and_searching],
 [Unconfusing Unicode: What is Unicode?][unconfusing_unicode],
 [The Updated Guide to Unicode on Python][updated_guide_to_unicode_on_python],
 [Text vs. Data Instead Of Unicode Vs. 8-bit][text_vs_data_in_new_3_0],
 [Python 3 and ASCII Compatible Binary Protocols][python_3_and_ascii_compatible_binary_protocols] and [Processing Text Files in Python 3][processing_text_files_in_python_3],
 [Programming with Unicode][programming_with_unicode] (CC),
 [UTF-8 Everywhere][utf8_everywhere]<br>
there are two basic built-in types for binary sequences: the immutable `bytes`
 type introduced in Python 3 and the mutable `bytearray`, added in Python 2.6.
 you can use familiar string methods like `endswith`, `replace`, `strip`,
 `translate`, `upper`, and dozens of others with binary sequences. the regular
 expression functions also work on binary sequences. From Python 3.5, the `%`
 operator works with binary sequences ([PEP 461][pep_461]).<br>
`bytes('café', encoding='utf_8')`, `bytes.fromhex('31 4B CE A9')`<br>
To extract structured information from binary sequences, the `struct` module is
 invaluable.<br>
`encode('...encoding...', errors='strict|ignore|replace|xmlcharrefreplace')`, `codecs.register_error`<br>
UTF-8 is the default source encoding for Python 3, just as ASCII was the
 default for Python 2 (starting with 2.5). Pyhton 3 allows non-ASCII
 identifiers in source code.<br>
The standard says that if a file is UTF-16 and has no BOM, it should be assumed
 to be UTF-16BE. However, the Intel x86 architecture is little-endian, so there
 is plenty of little-endian UTF-16 with no BOM in the wild. UTF-8 does not need
 BOM. Nevertheless, some Windows applications (notably Notepad) add the BOM to
 UTF-8 files anyway-and Excel depends on the BOM to detect a UTF-8 file,
 otherwise it assumes the content is encoded with a Windows codepage. The
 character U+FEFF encoded in UTF-8 is the three-byte sequence `b'\xef\xbb\xbf'`.
 So if a file starts with those three bytes, it is likely to be a UTF-8 file
 with a BOM. However, Python does not automatically assume this file is
 UTF-8.<br>
[PyUCA][pyuca] pure-Python implementation of the Unicode Collation Algorithm
 (UCA) instead the standard `locale` module that may depend on tricky locale
 configurations<br>
If you build a regular expression with `bytes`, patterns such as `\d` and `\w`
 only match ASCII characters; in contrast, if these patterns are given as `str`
 (without `re.ASCII`), they match Unicode digits or letters beyond ASCII.<br>
in `os` module, `str` -> `str`, `bytes` -> `bytes`<br>
`sys.getfilesystemencoding()` for file names, `fsencode()` and `fsdecode()`<br>
`'surrogateescape'` encode and decode by using "Low Surrogate Area" (U+DC00 -
 U+DCFF) ([PEP 383][pep_383])<br>
[/Tools/unicode/listcodecs.py][tools_unicode_listcodecs_py]<br>
[Changing the Python default encoding considered harmful][changing_the_python_default_encoding_considered_harmful] and [sys.setdefaultencoding is evil][sys_setdefaultencoding_is_evil]<br>
[Unicode normalization][unicode_normalization], FAQ [1][unicode_normalization_faq_1], [2][unicode_normalization_faq_2]<br>
AsciiDoc is used by O'Reilly's Atlas book publishing platform.<br>
Since Python 3.3, it chooses the most economic memory layout that is suitable
 for that particular `str` ([PEP 393][pep_393], for example, 1, 2 or 4 bytes
 per code point) like `int`. Before Python 3.3, there are "narrow build" (2
 bytes per code point, `sys.maxunicode` is 65535) and "wide build" (4 bytes per
 code point).

[character_encoding_and_unicode_in_python]: http://www.slideshare.net/fischertrav/character-encoding-unicode-how-to-with-dignity-33352863
[pragmatic_unicode]: http://nedbatchelder.com/text/unipain.html
[character_encoding_and_unicode_in_python]: https://www.youtube.com/watch?v=Mx70n1dL534
[unicode_howto]: https://docs.python.org/3/howto/unicode.html
[case_folding]: https://www.w3.org/International/wiki/Case_folding
[character_model_for_the_world_wide_web_string_matching_and_searching]: https://www.w3.org/TR/charmod-norm/
[unconfusing_unicode]: https://regebro.wordpress.com/2011/03/23/unconfusing-unicode-what-is-unicode/
[updated_guide_to_unicode_on_python]: http://lucumr.pocoo.org/2013/7/2/the-updated-guide-to-unicode/
[text_vs_data_in_new_3_0]: https://docs.python.org/3.0/whatsnew/3.0.html#text-vs-data-instead-of-unicode-vs-8-bit
[python_3_and_ascii_compatible_binary_protocols]: http://python-notes.curiousefficiency.org/en/latest/python3/binary_protocols.html
[processing_text_files_in_python_3]: http://python-notes.curiousefficiency.org/en/latest/python3/text_file_processing.html
[programming_with_unicode]: http://unicodebook.readthedocs.io/index.html
[utf8_everywhere]: http://utf8everywhere.org/
[pep_461]: https://www.python.org/dev/peps/pep-0461/
[pyuca]: https://pypi.python.org/pypi/pyuca/
[pep_383]: https://www.python.org/dev/peps/pep-0383/
[tools_unicode_listcodecs_py]: https://hg.python.org/cpython/file/tip/Tools/unicode/listcodecs.py
[changing_the_python_default_encoding_considered_harmful]: https://blog.startifact.com/posts/older/changing-the-python-default-encoding-considered-harmful.html
[sys_setdefaultencoding_is_evil]: https://ziade.org/2008/01/08/syssetdefaultencoding-is-evil/
[unicode_normalization]: http://unicode.org/reports/tr15/
[unicode_normalization_faq_1]: http://www.unicode.org/faq/normalization.html
[unicode_normalization_faq_2]: http://www.macchiato.com/unicode/nfc-faq
[pep_393]: https://www.python.org/dev/peps/pep-0393/

## III. Functions as Objects

### Chapter 5: First-Class Functions

[Functional Programming HOWTO][functional_programming_howto], [Origins of Python's "Functional" Features][origins_of_pythons_functional_features] by Guido van Rossum<br>
The `apply` function was deprecated in Python 2.3 and removed in Python 3
 because it's no longer necessary. In Python 3, `map` and `filter` return
 generators so their direct substitute is now a generator expression (in Python
 2, these functions returned lists, therefore their closest alternative is a
 listcomp). The `reduce` function was demoted from a built-in in Python 2 to
 the `functools` module in Python 3. Its most common use case, summation, is
 better served by the `sum` built-in available since Python 2.3.<br>
`callable()` built-in function, `__new__` method (for classes) and `__call__` method (for class instances)<br>
`__annotations__` (dict), `__call__` (method-wrapper), `__closure__` (tuple),
 `__code__` (code), `__defaults__` (tuple), `__get__` (method-wrapper),
 `__globals__` (dict), `__kwdefaults__` (dict, for keyword-only arguments),
 `__name__` (str), `__qualname__` (str, [PEP 3155][pep_3155])<br>
Keyword-only arguments ([PEP 3102][pep_3102]) are a new feature in Python 3. To
 specify keyword-only arguments when defining a function, name them after the
 argument prefixed with `*`. The keyword-only arguments do not need to have a
 default value. (for example, `def tag(name, *content, cls=None, **attrs)`,
 `def f(a, *, b)`)<br>
`inspect.signature` ([PEP 362][pep_362]) instead of fn`.__defaults__`, fn`.__code__.co_varnames` and
 fn`.__code__.co_argcount`, `inspect.Signature.bind()` like partial apply<br>
Python 3's function annotations ([PEP 3107][pep_3107]) only store them in
 fn`.__annotations__`. (`def clip(text:str, max_len:'int > 0'=80) -> str:`,
 Stack Overflow [1][stackoverflow_function_annotations_1]
 [2][stackoverflow_function_annotations_2])<br>
`operator` (`mul`, `itemgetter(1)`, `attrgetter('name', 'coord.lat')`,
 `methodcaller('replace', ' ', '-')`, ...) and `functools` (`partial` that is
 implemented in C, partial object, new `partialmethod` in Python 3.4,
 [Stack Overflow][stackoverflow_partial]) packages for functional programming<br>
[fn.py][fn_py] (with `@recur.tco`), [the early history of Bobo][early_history_of_bobo] ([six][six] Python 2 and 3 compatibility library), [Teaching Programming Languages in a Post-Linnaean Age][teaching_programming_languages_in_a_post_linnaean_age], Guido van Rossum [Tail Recursion Elimination][tail_recursion_elimination]

[functional_programming_howto]: https://docs.python.org/3/howto/functional.html
[origins_of_pythons_functional_features]: http://python-history.blogspot.com/2009/04/origins-of-pythons-functional-features.html
[pep_3155]: https://www.python.org/dev/peps/pep-3155/
[pep_3102]: https://www.python.org/dev/peps/pep-3102/
[pep_362]: https://www.python.org/dev/peps/pep-0362/
[pep_3107]: https://www.python.org/dev/peps/pep-3107/
[stackoverflow_function_annotations_1]: http://stackoverflow.com/questions/3038033/what-are-good-uses-for-python3s-function-annotations
[stackoverflow_function_annotations_2]: http://stackoverflow.com/questions/13784713/what-good-are-python-function-annotations
[stackoverflow_partial]: http://stackoverflow.com/questions/3252228/python-why-is-functools-partial-necessary
[fn_py]: https://github.com/kachayev/fn.py
[early_history_of_bobo]: http://discuss.fogcreek.com/joelonsoftware/default.asp?cmd=show&ixPost=94006
[six]: https://pypi.python.org/pypi/six
[teaching_programming_languages_in_a_post_linnaean_age]: http://cs.brown.edu/~sk/Publications/Papers/Published/sk-teach-pl-post-linnaean/
[tail_recursion_elimination]: http://neopythonic.blogspot.com/2009/04/tail-recursion-elimination.html

### Chapter 6: Design Patterns with First-Class Functions

[GOTO 2014 • Root Cause Analysis of some Faults in Design Patterns • Ralph E. Johnson][root_cause_analysis_of_some_faults_in_design_patterns], Peter Norvig [Design Patterns in Dynamic Languages][design_patterns_in_dynamic_languages], [Deisng Patterns in Python][design_patterns_in_python] ([video][design_patterns_in_python_video], EuroPython 2011), [Python 3 Patterns, Recipes and Idioms][python3_patterns_recipes_and_idioms]<br>
In Python 3.4, `class Promotion(abc.ABC):`, In Python 3.0-3.3, `class Promotion(metaclass=ABCMeta):`<br>
`inspect.getmembers(my_module, inspect.isfunction)`<br>
every Python callable implements a single-method interface, and that method is named `__call__`.<br>

[root_cause_analysis_of_some_faults_in_design_patterns]: https://www.youtube.com/watch?v=ImUcJctyUQ0
[design_patterns_in_dynamic_languages]: http://norvig.com/design-patterns/
[design_patterns_in_python]: http://www.aleax.it/gdd_pydp.pdf
[design_patterns_in_python_video]: https://www.youtube.com/watch?v=bPJKYrZjq10
[python3_patterns_recipes_and_idioms]: http://www.mindviewinc.com/Books/Python3Patterns/Index.php

### Chapter 7: Function Decorators and Closures

PEP 318<br>
A decorator is a callable that takes another function as argument, and is executed as soon as the module is imported. `@decorate def target(): ...` = `def target(): ... target = decorate(target)`<br>
a variable assigned in the body of a function is local, then you may need `global`. the nature of the variable--whether it is local or not--cannot change in the body of the function.<br>
a closure is a function with an extended scope that encompasses nonglobal variables referenced in the body of the function but not defined there. the `nonlocal` declaration (introduced in Python 3, [PEP 3104][pep_3104]) lets you flag a variable as a free variable even when it is assigned a new value within the function. fn.`__code__.co_freevars` (vs fn.`__code__.co_varnames`), fn.`__closure__[0].cell_contents`<br>
built-in `property`, `classmethod`, `staticmethod`<br>
`functools.wraps` (helper that copies the relevant attributes from a decorated function, to make a wrapper function to look like the wrapped function), `.lru_cache` (memoization), `.singledispatch` (generic function, depending on the type of the first argument, `@<base_function>.register(<type>)`, Python 3.4 [PEP 443][pep_443], old singledispatch package)<br>
parameterized decorators - use a decorator factory, `@clock()` instead of `@clock`<br>
Some argue that decorators are best coded as classes implementing `__call__`, and not as functions like the examples in this book.<br>
Graham Dumpleton's [decorator blog posts][graham_dumpleton_decorator_blog] and [wrapt][wrapt] module, [decorator package][decorator_package], Python Wiki [PythonDecoratorLibrary][python_decorator_library], [Five-minute Multimethods in Python][five_minute_multimethods_in_python] about single-dispatch generic functions, [Reg][reg] ([Morepath][morepath] web framework) for multiple-dispatch generic functions, [Closures in Python][closures_in_python], [PEP 227][pep_227] Statically Nested Scopes<br>
dynamic scope vs lexical scope (requires closures, since Algol), [The Roots of Lisp][roots_of_list] is an accessible explanation of [Recursive Functions of Symbolic Expressions and Their Computation by Machine, Part I][recursive_functions_of_symbolic_expressions_and_their_computation_by_machine]

[pep_3104]: https://www.python.org/dev/peps/pep-3104/
[pep_443]: https://www.python.org/dev/peps/pep-0443/
[graham_dumpleton_decorator_blog]: https://github.com/GrahamDumpleton/wrapt/blob/develop/blog/README.md
[wrapt]: http://wrapt.readthedocs.io/en/latest/
[decorator_package]: https://pypi.python.org/pypi/decorator
[python_decorator_library]: https://wiki.python.org/moin/PythonDecoratorLibrary
[five_minute_multimethods_in_python]: http://www.artima.com/weblogs/viewpost.jsp?thread=101605
[reg]: http://reg.readthedocs.io/en/latest/usage.html
[morepath]: http://morepath.readthedocs.io/en/latest/
[closures_in_python]: http://effbot.org/zone/closure.htm
[pep_227]: https://www.python.org/dev/peps/pep-0227/
[roots_of_list]: http://www.paulgraham.com/rootsoflisp.html
[recursive_functions_of_symbolic_expressions_and_their_computation_by_machine]: http://www-formal.stanford.edu/jmc/recursive/recursive.html

## IV. Object-Oriendted Idioms

### Chapter 8: Object References, Mutability, and Recycling

non-overloadable `is` (identity) is faster than `==` (equality, in fact, `__eq__()`).<br>
The `deepcopy` function remembers the objects already copied to handle cyclic references gracefully.<br>
`__copy__()`, `__deepcopy__()`<br>
Beware when a default value is a mutable object. Each default value is
 evaluated when the function is defined--i.e., usually when the module is
 loaded--and the default values become attributes (like
 HauntedBus`.__init__.defaults__`) of the function object.<br>
In CPython, the primary algorithm for garbage collection is reference counting.
 In CPython 2.0, a generational garbage collection algorithm was added to
 detect groups of objects involved in reference cycles.
 [PyPy, Garbage Collection, And A Deadlock][pypy_garbage_collection_and_a_deadlock]<br>
The Python console automatically binds the `_` variables to the result of
 expressions that not `None`. When trying to micro-manage memory we are often
 suprised by hidden, implicit assignment that create new references to our
 objects. The `_` console variable is one example. Traceback objects are
 another common source of unexpected references.<br>
The `weakref.ref` class is actually a low-level interface intended for advanced
 uses, and most programs are better served by the use of the `weakref`
 collections (`WeakKeyDictionary`, `WeakValueDictionary`, `WeakSet`) and
 `finalize`. If you need to build a class that is aware of every one of its
 instanes, a good solution is to create a class attribute with a `WeakSet` to
 hold the references to the instances. Basic `list` and `dict` instances may
 not be referents, but a plain subclass of either can solve this probrem
 easily. But `int` and `tuple` instances cannot be targets of weak references,
 even if subclasses of those types are created. Most of these limitations are
 implementation details of CPython that may not apply to other Python
 interpreters.<br>
CPython implementation details: `t[:]`, `tuple()`, `str`, `bytes` and
 `frozenset.copy()` return a reference to the same object. Note that CPython
 does not intern all strings or integers.<br>
Actually the type of an object may be changed by merely assigning a different
 class to its `__class__` attribute, but that is pure evil.<br>
[Python 103: Memory Model & Best Practices][python_103_memory_model_and_best_practices] (longer [video][python_103_memory_model_and_best_practices_video]),
 [Python 3 Module of the Week][python_3_module_of_the_week] (old [Python Module of the Week][python_module_of_the_week]),
 [How does Python manage memory?][how_does_python_manage_memory],
 [PEP 442][pep_442] for `__del__`, [String interning][string_interning],
 [Python Garbage Collector Implementations: CPython, PyPy and Gas][python_garbage_collector_implementations_cpython_pypy_gas],
 "Programming Language Pragmatics, Third Edition"

[pypy_garbage_collection_and_a_deadlock]: https://emptysqua.re/blog/pypy-garbage-collection-and-a-deadlock/
[python_103_memory_model_and_best_practices]: http://conferences.oreilly.com/oscon/oscon2013/public/schedule/detail/29374
[python_103_memory_model_and_best_practices_video]: https://www.youtube.com/watch?v=HHFCFJSPWrI
[python_3_module_of_the_week]: https://pymotw.com/3/
[python_module_of_the_week]: https://pymotw.com/2/
[how_does_python_manage_memory]: http://effbot.org/pyfaq/how-does-python-manage-memory.htm
[pep_442]: https://www.python.org/dev/peps/pep-0442/
[string_interning]: https://en.wikipedia.org/wiki/String_interning
[python_garbage_collector_implementations_cpython_pypy_gas]: https://thp.io/2012/python-gc/python_gc_final_2012-01-22.pdf

### Chapter 9: A Pythonic Object

```python
class Vector2d:
    typecode = 'd'

    def __iter__(self): # x, y = my_vector
        return (i for i in (self.x, self.y))

    def __repr__(self):
        class_name = type(self).__name__
        return '{}({!r}, {!r})'.format(class_name, *self)

    def __bytes__(self):
        return (bytes([ord(self.typecode)]) +
               bytes(array(self.typecode, self)))

    @classmethod
    def frombytes(cls, octets):
        typecode = chr(octets[0])
        memv = memoryview(octets[1:]).cast(typecode)
        return cls(*memv)

    def __format__(self, fmt_spec=''):
        components = (format(c, fmt_spec) for c in self)
        return '({}, {})'.format(*components)

    @property
    def x(self):
        reutrn self.__x

    def __hash__(self):
        return hash(self.x) ^ hash(self.y)

    ...
```

`@classmethod` (takes cls) and `@staticmethod` (has no special first argument),
 [The definitive guide on how to use static, class or abstract methods in Python][definitive_guide_on_static_class_or_abstract_methods_in_python]<br>
[Format Specification Mini-Language][format_specification_mini_language]<br>
If you name an instance attribute in the form `__mood` (two leading underscores
 and zero or at most one trailing underscore), Python stores the name in the
 instance `__dict__` prefixed with a leading underscore and the class name, so
 in the `Dog` class, `__mood` becomes `_Dog__mood`, and in `Beagle` it's
 `_Beagle__mood`. This language feature goes by the lovely name of *name
 mangling*. (In modules, a single `_` in front of a top-level name does have an
 effect: if you write `from mymod import *` the names with a `_` prefix are not
 imported from `mymod`. However, you can still write
 `from mymod import _privatefunc`.<br>
`__slots__ = ('__x', '__y')` Python stores these instance attributes in a
 tuple-like structure in each instance, avoiding the memory overhead of the
 per-isntance `__dict__`. A `__slots__` attribute inherited from a superclass
 has no effect. Python only takes into account `__slots__` attributes defined
 in each class individually. When `__slots__` is specified in a class, its
 instances will not be allowed to have any other attributes apart from those
 named in `__slots__`. `__weakref__` attribute is present by default in 
 instances of user-defined classes. However, if the class defines `__slots__`,
 and you need the instances to be targets of weak references, then you need to
 include `'__weakref__'` among the attributes named in `__slots__`.<br>
Class attributes can be used as default values for instance attributes. If you
 write to an instance attribute that does not exist, you create a new instance
 attribute and the class attribute by the same name is untouched.<br>
`__index__` was created to solve a need in NumPy. If we decide to write a new
 numeric data type, and we want it to be usable as argument to `__getitem__`,
 we need to implement `__index__`.
 [explanation in What's New in Python 2.5][pep_357_in_python_2_5],
 [PEP 357][pep_357]<br>
[How to display an object as a string: printString and displayString][how_to_display_an_object_as_a_string_printstring_and_displaystring] (in Smalltalk),
 [The Simplest Thing that Could Possibly Work][simplest_thing_that_could_possibly_work],
 Java [SecurityManager][java_tutorial_security_manager]

[definitive_guide_on_static_class_or_abstract_methods_in_python]: https://julien.danjou.info/blog/2013/guide-python-static-class-abstract-methods
[format_specification_mini_language]: https://docs.python.org/3/library/string.html#formatspec
[pep_357_in_python_2_5]: https://docs.python.org/2.5/whatsnew/pep-357.html
[pep_357]: https://www.python.org/dev/peps/pep-0357/
[how_to_display_an_object_as_a_string_printstring_and_displaystring]: http://esug.org/data/HistoricalDocuments/TheSmalltalkReport/ST07/04wo.pdf
[simplest_thing_that_could_possibly_work]: http://www.artima.com/intv/simplest3.html
[java_tutorial_security_manager]: http://docs.oracle.com/javase/tutorial/essential/environment/security.html

### Chapter 10: Sequence Hacking, Hashing, and Slicing

[gensim][gensim] (package that implements [vector space modeling][vector_space_model] for natural language processing and information retrieval)<br>
Use `reprlib.repr()` to get a limited-length representation.<br>
In the context of object-oriented programming, a protocol is an informal interface, defined only in documentation and not in code.<br>
undocumented `slice.indices` ([C API][pyslice_getindicesex])<br>
very often when you implement `__getattr__` you need to code `__setattr__` as
 well, to avoid inconsistent behavior in your objects.<br>
`def __eq__(self, other): return len(self) == len(other) and all(a == b for a, b in zip(self, other))`, `itertools.zip_longest(fillvalue=None)`, `enumerate`, `itertools.chain`<br>
hyperspherical coordinates ([hypersphere][hypersphere], [n-sphere][n_sphere])<br>
[Fold (higher-order function)][fold_higher_order_function],
 [KISS principle][kiss_principle],
 [polymorphism (was Re: Type checking in python?)][polymorphism_type_checking_in_python],
 [Duck typing][duck_typing],
 [Pythonic way to sum n-th list element?][pythonic_way_to_sum_nth_list_element]

[gensim]: https://pypi.python.org/pypi/gensim
[vector_space_model]: https://en.wikipedia.org/wiki/Vector_space_model
[pyslice_getindicesex]: https://docs.python.org/3/c-api/slice.html#c.PySlice_GetIndicesEx
[hypersphere]: http://mathworld.wolfram.com/Hypersphere.html
[n_sphere]: https://en.wikipedia.org/wiki/N-sphere
[fold_higher_order_function]: https://en.wikipedia.org/wiki/Fold_(higher-order_function)
[kiss_principle]: https://en.wikipedia.org/wiki/KISS_principle
[polymorphism_type_checking_in_python]: https://mail.python.org/pipermail/python-list/2000-July/046184.html
[duck_typing]: https://en.wikipedia.org/wiki/Duck_typing
[pythonic_way_to_sum_nth_list_element]: https://mail.python.org/pipermail/python-list/2003-April/218568.html

### Chapter 11: Interfaces: From Protocols to ABCs

Protocols are independent of inheritance. A class may implement several
 protocols, enabling its instances to fulfill several roles. Protocols are
 interfaces, but because they are informal--defined only by documentation and
 conventions--protocols cannot be enforced like formal interface (such as
 `abc.Sequence`) can. A protocol may be partially implemented in a particular
 class, and that's OK.<br>
discussions that replace "object that support the buffer protocol/interface/API" with "bytes-like object" [1][buffer_protocol_glossary_1], [2][buffer_protocol_glossary_2]

```python
class Struggle:
    def __len__(self): return 23

isinstance(Struggle(), collections.abc.Sized) # True, due to Sized.__subclasshook__()
issubclass(Struggle, collections.abc.Sized) # True
```

in the absence `__iter__` and `__containers__`, Python still manages to make
 iteration and the `in` operator work by invoking `__getitem__`.<br>
ABCs are meant to encapsulate very general concepts, abstractions, introduced
 by a framework--things like "a sequence" and "an exact number." [Readers] most
 likely don't need to write any new ABCs, just use existing ones correctly, to
 get 99.9% of the benefits without serious risk of misdesign.<br>
Python does not check for the implmentation of the abstract methods at import
 time, but only at runtime when we actually try to instantiate `FrenchDeck2`.<br>
abstract methods and concrete methods<br>
`Container` (`__contains__`) + `Iterable` (`__iter__`) + `Sized` (`__len__`) <-
 `Sequence` (`__getitem__`\*, `__reversed__`, `index`, `count`) <-
 `MutableSequence` (`__delitem__`\*,  `insert`\*, `append`, `reverse`,
 `extend`, `pop`, `remove`, `__iadd__`)<br>
[collections.abc ABC relationship table][abc_relationship_table], [numbers package][numbers_package]<br>
`isinstance(x, numbers.Real)` takes `bool`, `int`, `float`, `fractions.Fraction`
 or any other noncomplex numerical type provided by an external library, such
 as NumPy. `decimal.Decimal` is not registered as a virtual subclass of
 `numbers.Real`.<br>
implementing ABC - subclass `abc.ABC` (Python 3.4), `@abc.abstractmethod` and
 docstring (an abstract method can actually have an implmentation. Even if it
 does, subclasses will still be forced to override it.)<br>
(old Python 3) `class Tombola(metaclass=abc.ABCMeta)`,
 (Python 2) `class Tombola(object): __metaclass__ = abc.ABCMeta`<br>
When `@abstractmethod` is applied in combination with other method descriptors
 (`@classmethod`, `@staticmethod`, `@property`), it should be applied as the
 innermost decorator.<br>
`@Tombola.register` (in Python 3.3 or earlier, `Tombola.register(TomboList)`),
 the registered class becomes a virtual subclass of the ABC, and will be
 recognized as such by functions like `issubclass` and `isinstance`. virtual
 subclasses do not inherit their registered ABCs, and are not checked for
 conformance to the ABC interface at any time, not even when they are
 instantiated.<br>
`__subclasses__()` (immediate subclasses), `_abc_registry` (`WeakSet`)<br>
`doctest.testfile()`<br>
"all non-leaf classes should be abstract" *More Effective C++* [item 33][more_effective_cpp_33],
 [collections.abc source code][collections_abc_src], [abc.ABC source code][abc_abc_src],
 [PyMOTW abc module][pymotw_abc_module],
 [PEP 3119][pep_3119] Introducing Abstract Base Classes,
 [PEP 3141][pep_3141] A Type Hierarchy for Numbers,
 [Contracts in Python: A Conversation with Guido van Rossum, Part IV][contracts_in_python] (about the pros and cons of dynamic typing),
 [zope.interface package][zope_interface] (used in Twisted, Pyramid and Plone, [A Python component architecture][python_component_architecture], [A Comprehensive Guide to Zope Component Architecture][comprehensive_guide_to_zope_component_architecture]),
 [Mypy][mypy], [Optional static typing -- the crossroads][optional_static_typing], [PEP 484][pep_484] Type Hints, [PEP 482][pep_482] Literature Overview for Type Hints,
 Java [Default Methods][java_default_methods],
 [Ruby Kaigi 2014: Day 2][ruby_kaigi_2014_day_2]

[buffer_protocol_glossary_1]: http://bugs.python.org/issue16518
[buffer_protocol_glossary_2]: http://bugs.python.org/issue22581
[abc_relationship_table]: https://docs.python.org/3/library/collections.abc.html#collections-abstract-base-classes
[numbers_package]: https://docs.python.org/3/library/numbers.html
[more_effective_cpp_33]: http://ptgmedia.pearsoncmg.com/images/020163371x/items/item33.html
[collections_abc_src]: https://hg.python.org/cpython/file/3.4/Lib/_collections_abc.py
[abc_abc_src]: https://hg.python.org/cpython/file/3.4/Lib/abc.py
[pymotw_abc_module]: https://pymotw.com/2/abc/index.html
[pep_3119]: https://www.python.org/dev/peps/pep-3119/
[pep_3141]: https://www.python.org/dev/peps/pep-3141/
[contracts_in_python]: http://www.artima.com/intv/pycontract.html
[zope_interface]: http://zopeinterface.readthedocs.io/en/latest/
[python_component_architecture]: https://regebro.wordpress.com/2007/11/16/a-python-component-architecture/
[comprehensive_guide_to_zope_component_architecture]: http://muthukadan.net/docs/zca.html
[mypy]: http://www.mypy-lang.org
[optional_static_typing]: https://mail.python.org/pipermail/python-ideas/2014-August/028742.html
[pep_484]: https://www.python.org/dev/peps/pep-0484/
[pep_482]: https://www.python.org/dev/peps/pep-0482/
[java_default_methods]: https://docs.oracle.com/javase/tutorial/java/IandI/defaultmethods.html
[ruby_kaigi_2014_day_2]: http://brewhouse.io/blog/2014/09/19/ruby-kaigi-2014-day-2

### Chapter 12: Inheritance: For Good or For Worse

[We] started to push on the inheritance idea as a way to let novices build on frameworks that could only be designed by experts. ... we needed a better theory about inheritance entirely (and still do). For example, inheritance and instancing (which is a kind of inheritance) muddles both pragmatics (such as factoring code to save space) and semantics (used for way too many tasks such as: specialization, generalization, speciation, etc.). -- Alan Kay, [The Early History of Smalltalk][early_history_of_smalltalk]<br>
the code of the built-ins (written in C) does not call special methods overridden by user-defined classes.<br>
Classes have an attribute called `__mro__` holding a tuple of references to the
 superclasses in MRO (Method Resolution Order, C3 algorithm,
 [The Python 2.3 Method Resolution Order][python_2_3_method_resolution_order]),
 from the current class all the way to the `object` class.<br>
`A.ping(self)`, instead of `super().ping()` (in Python 2, `super(D, self).ping()`), explicit `self`<br>
`io.BytesIO` and `io.TextIoWrapper` (returned by `open()`)<br>

* distinguish interface inheritance (subtype, "is-a" relationship, backbone of a framework) from implementation inheritance (avoid code duplication, can be replaced by composition and delegation)
* make interfaces explicit with ABCs
* use mixins for code reuse (a mixin does not define a new type; it merely bundles few and very closely related methods for reuse. a mixin should never be instantiated, and concrete classes should not inherit only from a mixin.)
* make mixins explcit by naming (`Mixin` suffix)
* an ABC may also be a mixin; the reverse is not true (concrete methods in an ABC are always for convenience)
* don't subclass from more than one concrete class
* provide aggregate classes to users (if some combination of ABCs or mixins is particularly useful to client code, provide a class that brings them together in a sensible way, *Object Oriented Analysis and Design, 3E*, example: `tkinter.Widget`, Django `ListView`)
* "favor object composition over class inheritance.", *Design Patterns*

example: tkinter module (since Python 1.1) widget hierarchy, tkinter.ttk package (since Python 3.1)<br>
example: Django, [Classy Class-Based Views][classy_class_based_views]<br>
If, while working as an application developer, you find yourself building multilevel class hierarchies, it's likely that one or more of the following applies: ...<br>
in soapbox, inheritance case study: Smalltalk (Squeak, Pharo), C++, Java, Scala, PHP, Groovy, Rust, Perl 6, Ruby, Go, Julia<br>
[Differences between PyPy and CPython][differences_between_pypy_and_cpython],
 [template method pattern][template_method_pattern],
 [Python's Super is nifty, but you can't use it (a.k.a. Python's Super Considered Harmful)][pythons_super_is_nifty_but_you_cant_use_it], [Python’s super() considered super!][pythons_super_considered_super],
 [Setting Multiple Inheritance Straight][setting_multiple_inheritance_straight] (traits, a constrainted form of mixins that originated in the Self language, blog post [1][micheles_1], [2-1][micheles_2_1], [2-2][micheles_2_2], [3-1][micheles_3_1], [3-2][micheles_3_2], [3-3][micheles_3_3]

[early_history_of_smalltalk]: http://propella.sakura.ne.jp/earlyHistoryST/EarlyHistoryST.html
[differences_between_pypy_and_cpython]: http://pypy.readthedocs.io/en/latest/cpython_differences.html
[python_2_3_method_resolution_order]: https://www.python.org/download/releases/2.3/mro/
[classy_class_based_views]: http://ccbv.co.uk
[template_method_pattern]: https://en.wikipedia.org/wiki/Template_method_pattern
[pythons_super_is_nifty_but_you_cant_use_it]: https://fuhm.net/super-harmful/
[pythons_super_considered_super]: https://rhettinger.wordpress.com/2011/05/26/super-considered-super/
[setting_multiple_inheritance_straight]: http://www.artima.com/weblogs/viewpost.jsp?thread=246488
[micheles_1]: http://www.artima.com/weblogs/viewpost.jsp?thread=281127
[micheles_2_1]: http://www.artima.com/weblogs/viewpost.jsp?thread=246341
[micheles_2_2]: http://www.artima.com/weblogs/viewpost.jsp?thread=246483
[micheles_3_1]: http://www.artima.com/weblogs/viewpost.jsp?thread=236275
[micheles_3_2]: http://www.artima.com/weblogs/viewpost.jsp?thread=236278
[micheles_3_3]: http://www.artima.com/weblogs/viewpost.jsp?thread=237121

### Chapter 13: Operator Overloading: Doing It Right

limitations:

* we cannot overload operators for the built-in types.
* we cannot create new operators, only overload existing ones.
* a few operators can't be overloaded: `is`, `and`, `or`, `not` (but the bitwise `&`, `|`, `~`, can).

unary operators - `__neg__`, `__pos__`, `__invert__`, `__abs__`, always return
 a new object<br>
`a + b`: `a.__add__(b)` -> if does not exist or returns `NotImplemented`,
 `b.__radd__(a)` -> if does not exist or returns `NotImplemented`, raise
 `TypeError`<br>
`NotImplemented` (a special singleton value that an infix operator special
 method should return to tell the interpreter it cannot handle a given operand)
 vs `NotImplementedError` (an exception that stub methods in abstract classes
 raise to warn that they must be overwritten by subclasses)<br>
by definition, the reverse method will only be invoked when dealing with an
 operand of a different type.

```python
def __add__(self, other):
    try:
        pairs = itertools.zip_longest(self, other, fillvalue=0.0)
        return Vector(a + b for a, b in pairs)
    except TypeError:
        return NotImplemented

def __radd__(self, other):
    return self + other
```

`@` (matrix multiplication, Python 3.5, [PEP 465][pep_465]), `__matmul__`, `__rmatmul__`, `__imatmul__`<br>
`object.__eq__` and `object.__ne__` is implemented in function object_richcompare in [Objects/typeobject.c][objects_typeobject_c]<br>
augmented assignement operators are expected to change the lefthand operand in
 place for mutable types.<br>
The `+=` operator is more liberal than `+` with regard to the second operand.
 With `+`, we want both operands to be of the same type, because if we accpets
 different types this might cause confusion as to the type of the result. With
 the `+=`, the situation is clearer: the left hand object is updated in place,
 so there's no doubt about the type of the result. For sequence types, `+`
 usually requires that both operands are of the same type, while `+=` often
 accepts any iterable as the righthand operand.<br>
the [`functools.total_ordering` function][functools_total_ordering_function] is
 a class decorator that automatically generates methods for all rich comparison
 operators in any class that defines at least a couple of them.<br>
Probably about 20 to 30 percent of the population think of operator overloading
 as the spawn of the devil; somebody has done something with operator
 overloading that has just readlly ticked them off, because they've used like +
 for list insertion and it makes life really, really confusing. A lot of that
 problem stems from the fact that there are only about half a dozen operators
 you can sensily overload, and yet there are thousands or millions of operators
 that people would like to define--so you have to pick, and often the choices
 conflict with your sense of intuition. ... Then there's a community of about
 10 percent that have actually used operator overloading appropriately and who
 really care about it, and for whom it's actually really important; this is
 almost exclusively people who do numerical work, where the notation is very
 important to appealing to people's intuition, because they come into it with
 an intuition about what the + means, and the ability yo say "a + b" where a
 and b are complex numbers or matrices or something really does make sense. --
 James Gosling<br>
[The C Family of Languages: Interview with Dennis Ritchie, Bjarne Stroustrup, and James Gosling][c_family_of_languages_interview],
 [A Simple Technique for Handling Multiple Polymorphism][simple_technique_for_handling_multiple_polymorphism] (paper),
 [Arithmetic and Double Dispatching in Smalltalk-80][arithmetic_and_double_dispatching_in_smalltalk_80] (paper),
 [CHICKEN Scheme][chicken_scheme]

[c_family_of_languages_interview]: http://www.gotw.ca/publications/c_family_interview.htm
[pep_465]: https://www.python.org/dev/peps/pep-0465/
[objects_typeobject_c]: https://hg.python.org/cpython/file/c0e311e010fc/Objects/typeobject.c
[functools_total_ordering_function]: https://docs.python.org/3/library/functools.html#functools.total_ordering
[simple_technique_for_handling_multiple_polymorphism]: https://wiki.cites.illinois.edu/wiki/download/attachments/273416327/ingalls.pdf
[arithmetic_and_double_dispatching_in_smalltalk_80]: https://wiki.cites.illinois.edu/wiki/download/attachments/273416327/double-dispatch.pdf
[chicken_scheme]: http://www.call-cc.org/

## V. Control Flow

### Chapter 14: Iterables, Iterators, and Generators

When I see patterns in my programs, I consider it a sign of trouble. The shape
 of a program should reflect only the problem it needs to solve. Any other
 regularity in the code is a sign, to me at least, that I'm using abstractions
 that aren't powerful enough--often that I'm generating by hand the expansions
 of some macro that I need to write. -- Paul Graham,
 [Revenge of the Nerds][revenge_of_the_nerds]<br>
Python does not have macros like Lisp, so abstracting away the Interator
 pattern required changing the language: the `yield` keyword was added in Python
 2.2 (`__future__` in Python 2.1). The yield keyword allows the construction of
 generators, which work as iterators (that retrieves items from a collection).
 Every generator is an iterator: generators fully implement the iterator
 interface.<br>
Every collection in Python is iterable. (for example, tuple unpacking and
 unpacking actual parameters with `*` in function calls)<br>
Whenever the interpreter needs to iterate over an object `x`, it automatically
 calls `iter(x)`. The `iter` built-in function:
1. Checks whether the object implements `__iter__`, and calls that to obtain an
   iterator.
1. If `__iter__` is not implemented, but `__getitem__` is implemented, Python
   creates an iterator that attempts to fetch items in order, starting from
   index 0.
1. If that fails, Python raises `TypeError`, usually saying "*C* object is not
   iterable."

The standard interface for an iterator has two methods. `__next__` returns the
 next avaiable item, raising `StopIteration` when there are no more itms. This
 exception is handled internally in `for` loops and other iteration contexts
 like list comprehensions, tuple unpacking, etc. `__iter__` returns `self`; this
 allows iterators to be used where an iterable is expected, for example, in a
 `for` loop.<br>
A generator expression (`(x*3 for x in gen_AB())`) can be understood as a lazy
 version of a list comprehension. When a generator expression is passed as the
 single argument to a function or constructor, you don't need to write a set of
 parentheses.<br>
`itertools.takewhile(lambda n: n < 3, itertools.count(1, .5))`<br>
`itertools.islice(it, stop)` or `itertools.islice(it, start, stop, step=1)` is
 similar to `s[:stop]` or `s[start:stop:step]` except `it` can be any iterable,
 and the operation is lazy.<br>
`itertools.combinations('ABC', 2)`,
 `itertools.combinations_with_replacement('ABC', 2)`, (with the order)
 `itertools.permutations('ABC', 2)`, `itertools.product('ABC', repeat=2)`<br>
`iter` can be called with two arguments to create an iterator from a regular
 function or any callable object. In this usage, the first argument must be a
 callable to be invoked repeatedly (with no arguments) to yield values, and the
 second argument is a sentinel: a marker value which, when returned by the
 callable, causes the iterator to raise `StopIteration` instead of yielding the
 sentinel.<br>
Like `.__next__()`, `.send()` causes the generator to advance to the next
 `yield`, but it also allows the client using the generator to send data into
 it: whatever argument is passed to `.send()` becomes the value of the
 corresponding `yield` expression inside the generator function body. This is
 such a major "enhancement" that it actually changes the nature of generators:
 when used in this way, they become *coroutines*. Despite some similarities,
 generators and coroutines are basically two different concepts.<br>
Beazley's
 [A Curious Course on Coroutines and Concurrency][curious_course_on_coroutines_and_concurrency] (video
 [1][curious_course_on_coroutines_and_concurrency_video_1],
 [2][curious_course_on_coroutines_and_concurrency_video_2],
 [3][curious_course_on_coroutines_and_concurrency_video_3])
 [Itertools Recipes][itertools_recipes],
 [Python: The Full Monty: A Tested Semantics for the Python Programming Language][python_the_full_monty]

[revenge_of_the_nerds]: http://www.paulgraham.com/icad.html
[curious_course_on_coroutines_and_concurrency]: http://www.dabeaz.com/coroutines/
[curious_course_on_coroutines_and_concurrency_video_1]: http://pyvideo.org/video/213
[curious_course_on_coroutines_and_concurrency_video_2]: http://pyvideo.org/video/215
[curious_course_on_coroutines_and_concurrency_video_3]: http://pyvideo.org/video/214
[itertools_recipes]: https://docs.python.org/3/library/itertools.html#itertools-recipes
[python_the_full_monty]: https://cs.brown.edu/~sk/Publications/Papers/Published/pmmwplck-python-full-monty/

### Chapter 15: Context Managers and else Blocks

The semantics of `for/else`, `while/else`, and `try/else` are closely related,
 but very different from `if/else`. The `else` clause is skipped if an exception
 or a `return`, `break`, or `continue` statement causes control to jump out of
 the main block of the compound statement.<br>
EAFP (easier to ask for forgiveness than permission, by Grace Hopper). This
 common Python coding style assumes the existence of valid keys or attributes
 and catches exceptions if the assumption proves false. The technique contrasts
 with the LBYL (look before you leap) style common to many other languages such
 as C.<br>
The context manager protocol consists of the `__enter__` and `__exit__` methods.
 `with` blocks don't define a new scope, as functions and modules do. The
 context manager object is the result of evaluating the expression after `with`,
 but the value bound to the target variable (in the `as` clause) is the result
 of called `__enter__` on the context manager object. When control flow exists
 the `with` block in any way, the `__exit__` method is invoked on the context
 manager object, not on whatever is returned by `__enter__`. If `__exit__`
 returns `None` or anything but `True`, any exception raised in the `with` block
 will be propagated. The three arguments are exactly what you get if you call
 `sys.exc_info()` in the `finally` block. This makes sense, considering that the
 `with` statement is meant to replace most uses of `try/fianlly`.<br>
`contextlib` module:
* `redirect_stdout` - just pass it the file-like object that will stand in for
  `sys.stdout`
* `closing` - build context managers out of objects that provide a `close()`
  method
* `suppress` - a context manager to temporarily ignore specified exceptions
* `@contextmanager` - a [decorator][contextmanager_implementation] that lets you
  build a context manager from a simple generator function. you just implement a
  generator with a single `yield` that should produce whatever you want the
  `__enter__` method to return. Having a `try/finally` (or a `with` block)
  around the `yield` is an unavoidable price of using `@contextmanager`, because
  you never know what the users of your context manager are going to do inside
  their `with` block. The `@contextmanager` decorator is an elegant and
  practical tool that brings together three distinctive Python features: a
  function decorator, a generator, and the `with` statement.
* `ExitStack` - a context manager that lets you enter a variable number of
  context managers. When the `with` block ends, `ExitStack` calls the stanked
  context managers' `__exit__` methods in LIFO order.

*Python Cookbook, 3E* "Recipe 8.3" implements a `LazyConnection` class whose
 instances are context managers that open and close network connections
 automatically in `with` blocks. "Recipe 9.22" introduces a context manager for
 timing code, and another for making transactional changes to a `list`
 object.<br>
You can factor out B in a subroutine. But what if you want to factor out the
 bread, to make sandwiches with wheat bread, using a different filling each
 time? That's what the `with` statement offers. It's the complement of the
 subroutine.<br>
[What Makes Python Awesome][what_makes_python_awesome] (PyCon US 2013 keynote,
 [slide][what_makes_python_awesome_slide]),
 [Easy in-place file rewriting][easy_inplace_file_rewriting] example,
 [Transforming code into Beautiful, Idiomatic Python][transforming_code_into_beautiful_idiomatic_python],
 [The Python "with" Statement by Example][python_with_statement_by_example]

[contextmanager_implementation]: https://hg.python.org/cpython/file/3.4/Lib/contextlib.py#l34
[what_makes_python_awesome]: http://pyvideo.org/pycon-us-2013/keynote-3.html
[what_makes_python_awesome_slide]: https://speakerdeck.com/pyconslides/pycon-keynote-python-is-awesome-by-raymond-hettinger
[easy_inplace_file_rewriting]: http://www.zopatista.com/python/2013/11/26/inplace-file-rewriting/
[transforming_code_into_beautiful_idiomatic_python]: https://speakerdeck.com/pyconslides/transforming-code-into-beautiful-idiomatic-python-by-raymond-hettinger-1
[python_with_statement_by_example]: http://preshing.com/20110920/the-python-with-statement-by-example/

### Chapter 16: Coroutines

In addition to `.send(...)`, PEP 342 (Python 2.5, 2006) also added `.throw(...)`
 and `.close(...)` methods. [PEP 380][pep_380] (Python 3.3, 2012) made two
 syntax changes to generator function, to make them more useful as coroutines. A
 generator can now `return` a value; previously, providing a value to the
 `return` statement inside a generator raised a `SyntaxError`. (The value of the
 `return` expression is smuggled to the caller as the `value` attribute of the
 `StopIteration` exception.) The `yield from` syntax enabled complex generators
 to be refactored into smaller, nested generators while avoiding a lot of
 boilerplate code previously required for a generator to delegate to
 subgenerators. (In the case of `yield from`, the interpreter not only consumes
 the `StopIteration`, but its `value` attribue becomes the value of the
 `yield from` expression itself.)<br>
A coroutine can be in one of four states. You can determine the current state
 using the `inspect.getgeneratorstate(...)` function.<br>
The initial call `next(my_coro)` is often described as "priming" the coroutine.
 You can also call `my_coro.send(None)`, and the effect is the same.<br>
If the exception is handled by the generator, flow advances to the next `yield`,
 and the value yielded becomes the value of the `generator.throw` call. If the
 expression is not handled by the generator, it propagates to the context of the
 caller. One of the main reasons why the `yield from` construct was added has to
 do with throwing exceptions into nested coroutines. `generator.close()` causes
 the `yield` expression where the generator was paused to raise a
 `GeneratorExit` exception. No error is reported to the caller if the generator
 does not handle that exception or raises `StopIteration`--usually by running to
 completion. When receiving a `GeneratorExit`, the generator must not yield a
 value, otherwise a `RuntimeError` is raised. If any other exception is raised
 by the generator, it propagates to the caller.<br>
The first thing to know about `yield from` is that it is a completely new
 language construct. Similar constructs in other languages are called `await`,
 and that is a much better name because it conveys a crucial point: when a
 generator `gen` calls `yield from subgen()`, the `subgen` takes over and will
 yield values to the caller of `gen`; the caller will in effect drive `subgen`
 directly. Meanwhile `gen` will be blocked, waiting until `subgen` terminates.
 The main feature of `yield from` is to open a bidirectional channel from the
 outmost caller to the innermost subgenerator, so that values can be sent and
 yielded back and forth directly from them, and exceptions can be thrown all the
 way in without adding a lot of exception handling boilerplate code in the
 intermediate coroutines.<br>
caller (client) - delegating generator - subgenerator<br>
Almost all he `yield from` examples I've seen are tied to asynchronous
 programming with the `asyncio` module, so they depend on an active event loop
 to work.<br>
[SimPy][simpy] is a DES ([discrete event simulation][discrete_event_simulation])
 package for Python that uses one coroutine to represent each process in the
 simulation. Priority queues are a fundamental building block of discrete event
 simulations.
 ([Writing a Discrete Event Simulation: ten easy lessons][writing_a_discrete_event_simulation])
 / [SymPy][sympy] is a library for symbolic mathematics.<br>
In the console, the `_` variable is bound to the last result.<br>
`asyncio` coroutines are (usually) decorated with an `@asyncio.coroutine`
 decorator, and they are always driven by `yield from`, not by calling
 `.send(...)` directly on them.<br>
[PEP 492][pep_492] (Python 3.5) proposes the addition of two keywords: `async`
 and `await`. The former will be used with other existing keywords to define new
 language constructs. For example, `async def` will be used to define a
 coroutine, and `async for` to loop over asynchronous iterables with
 asynchronous iterators (implementing `__aiter__` and `__anext__`, coroutine
 versions of `__iter__` and `__next__`). To avoid conflict with the upcoming
 `async` keyword, the essential function `asyncio.async()` will be renamed
 `asyncio.ensure_future()` in Python 3.4.4. The `await` keyword will do
 something similar to `yield from`, but will only be allowed inside coroutines
 defined with `async def`--where the use of `yield` and `yield from` will be
 forbidden. With new syntax, the PEP establishes a clear separation between the
 legacy generators that evolved into coroutine-like objects and a new breed of
 native coroutine objects.

[ipython-yf][ipython_yf] (iPython extension that enables evaluating `yield from`
 directly in the iPython console, it's a syntax error to use `yield` outside of
 a function), [flatten example][flatten_example],
 [How Python 3.3 "yield from" construct works][how_python_3_3_yield_from_construct_works],
 Beazley's
 [Generator Tricks for Systems Programmers][generator_tricks_for_system_programmers]
 and [Generators: The Final Frontier][generators_the_final_frontier],
 [Greedy algorithms with coroutines][greedy_algorithms_with_coroutines],
 [Popular recipes tagged *coroutine*][popular_recipes_tagged_coroutine],
 [MicroPython][micropython], [Greg Ewing's examples][greg_ewing_examples],
 [Effective Python, Item 40: Consider Coroutines to Run Many Functions Concurrently][effective_python_item_40]
 ([source code][effective_python_item_40_src]),
 [Comparing two CSV files using Python][comparing_two_csv_files_using_python],
 [Iterables, Iterators and Generators][iterables_iterators_and_generators],
 [The difference between yield and yield-from][difference_between_yield_and_yield_from],
 [PEP 380 (yield from a subgenerator) comments][pep_380_comments],
 [The Value Of Syntax?][the_value_of_syntax] from
 [Lambda the Ultimate][lambda_the_ultimate],
 [What Color is Your Function?][what_color_is_your_function]

[pep_380]: https://www.python.org/dev/peps/pep-0380/
[simpy]: https://simpy.readthedocs.io/en/latest/
[discrete_event_simulation]: https://en.wikipedia.org/wiki/Discrete_event_simulation
[writing_a_discrete_event_simulation]: http://www.cs.northwestern.edu/~agupta/_projects/networking/QueueSimulation/mm1.html
[sympy]: http://www.sympy.org/en/index.html
[pep_492]: https://www.python.org/dev/peps/pep-0492/
[ipython_yf]: https://github.com/tecki/ipython-yf
[flatten_example]: https://github.com/dabeaz/python-cookbook/blob/master/src/4/how_to_flatten_a_nested_sequence/example.py
[how_python_3_3_yield_from_construct_works]: http://flupy.org/resources/yield-from.pdf
[generator_tricks_for_system_programmers]: http://www.dabeaz.com/generators/
[generators_the_final_frontier]: http://www.dabeaz.com/finalgenerator/
[greedy_algorithms_with_coroutines]: http://seriously.dontusethiscode.com/2013/05/01/greedy-coroutine.html
[popular_recipes_tagged_coroutine]: https://code.activestate.com/recipes/tags/coroutine/
[micropython]: http://micropython.org/
[greg_ewing_examples]: http://www.cosc.canterbury.ac.nz/greg.ewing/python/yield-from/yield_from.html
[effective_python_item_40]: http://www.effectivepython.com/2015/03/10/consider-coroutines-to-run-many-functions-concurrently/
[effective_python_item_40_src]: https://gist.github.com/ramalho/da5590bc38c973408839
[comparing_two_csv_files_using_python]: https://mail.python.org/pipermail/tutor/2015-February/104200.html
[iterables_iterators_and_generators]: http://nbviewer.jupyter.org/github/wardi/iterables-iterators-generators/blob/master/Iterables,%20Iterators,%20Generators.ipynb
[difference_between_yield_and_yield_from]: https://groups.google.com/forum/#!msg/python-tulip/bmphRrryuFk/aB45sEJUomYJ
[pep_380_comments]: https://mail.python.org/pipermail/python-dev/2009-March/087382.html
[the_value_of_syntax]: http://lambda-the-ultimate.org/node/4295
[lambda_the_ultimate]: http://lambda-the-ultimate.org/
[what_color_is_your_function]: http://journal.stuffwithstuff.com/2015/02/01/what-color-is-your-function/

### Chapter 17: Concurrency with Futures

`concurrent.futures` library (Python 3.2), [futures][futures_package] package
 (Python 2.5)<br>
The main features of the `concurrent.futures` package are the
 `ThreadPoolExecutor` and `ProcessPoolExecutor` classes. The `executor.__exit__`
 method will call `executor.shutdown(wait=True)`, which will block until all
 threads are done. If any of the threaded calls raised an exception, that
 exception would be raised here (`list(res)`) as the implicit `next()` call
 tried to retrieve the corresponding return value from the iterator.<br>
Futures are essential components in the internals of `concurrent.futures` and of
 `asyncio`, but as users of these libraries we sometimes don't see them. As of
 Python 3.4, there are two classes named `Future` in the standard library:
 `concurrent.futures.Future` and `asyncio.Future`. This is similar to the
 `Deferred` class in Twisted, the `Future` class in Tornado, and `Promise`
 objects in various JavaScript libraries. Both types of `Future` have a
 `.done()` method that is nonblocking and returns a Boolean that tells you
 whether the callable linked to that future has executed or not. Instead of
 asking whether a future is done, client code usually asks to be notified.
 That's why both `Future` classes have an `.add_done_callback()` method. When
 the future is not done, the behavior of the `result` method is very different
 between the two flavors of `Future`. In a `concurrent.futures.Future` instance,
 invoking `f.result()` will block the caller's thread until the result is ready.
 An optional `timeout` argument can be passed, and if the future is not done in
 the specified time, a `TimeoutError` exception is raised. We'll see that the
 `asyncio.Future.result` method does not support timeout, and the preferred way
 to get the result of futures in that library is to use `yield from`--which
 doesn't work with `concurrent.futures.Future` instances.<br>
The CPython interpreter is not thread-safe internally, so it has a Global
 Interpreter Lock (GIL), while allows only one thread at a time to execute
 Python bytecodes. That's why a single Python process usually cannot use
 multiple CPU cores at the same time. (This is a limitation of the CPython
 interpreter, not of the Python language itself. Jython and IronPython are not
 limited in this way; but Pypy, the fastest Python interpreter available, also
 has a GIL.) When we write Python code, we have no control over the GIL, but a
 built-in function or an extension written in C can release the GIL while
 running time-consuming tasks. In fact, a Python library coded in C can manage
 the GIL, launch its own OS threads, and take advantage of all available CPU
 cores. Every blocking I/O function in the Python standard library releases the
 GIL, allowing other threads to run. The `time.sleep()` function also releases
 the GIL.<br>
The set of futures you pass to `futures.as_completed` may come from more than
 one executor--perfhaps some were created by a `ThreadPoolExecutor` instance
 while others are from a `ProcessPoolExecutor`.<br>
In Python 3, the original `thread` module was deprecated (renamed to `_thread`
 to highlight the face that it's just a low-level implementation detail) in
 favor of the higher-level `threading` module. The `multiprocessing` package
 emulates the `threading` API but delegates jobs to multiple processes.
 `multiprocessing` also offers facilities to solve the biggest challenge faced
 by collaborating processes: how to pass around data.<br>
In PEP 3148, Quinlan wrote that the `concurrent.futures` library was "heavily
 influenced by the Java `java.util.concurrent` package." `multiprocessing` is
 the basis for the `concurrent.futures.ProcessPoolExecutor`. The [`lelo`][lelo]
 package defines a `@parallel` decorator that you can apply to any function to
 magically make it unblocking: when you call the decorated function, its
 execution is started in another process.
 [python-parallelize][python_parallelize] package provides a `parallelize`
 generator that you can use to distribute the execution of a `for` loop over
 multiple CPUs. Both packages use the `multiprocessing` module under the covers.

[Threads, processes and concurrency in Python: some thoughts][threads_processes_and_concurrency_in_python],
 [Generators: The Final Frontier][generators_the_final_frontier],
 [tqdm][tqdm] (add a progress meter to your loops),
 [PyConAU 2010: The future is soon!][the_future_is_soon],
 [Can’t we get rid of the Global Interpreter Lock?][cant_we_get_rid_of_the_gil],
 [It isn't Easy to Remove the GIL][it_isnt_easy_to_remove_the_gil],
 [Python Threads and the Global Interpreter Lock][python_threads_and_the_global_interpreter_lock],
 [Understanding the Python GIL][understanding_the_python_gil]
 ([slide][understanding_the_python_gil_slide]),
 [embarrassingly parallel][embarrassingly_parallel] (Wikipedia)

[futures_package]: https://pypi.python.org/pypi/futures/
[lelo]: https://pypi.python.org/pypi/lelo
[python_parallelize]: https://github.com/npryce/python-parallelize
[threads_processes_and_concurrency_in_python]: http://www.artima.com/weblogs/viewpost.jsp?thread=299551
[generators_the_final_frontier]: http://www.dabeaz.com/finalgenerator/
[tqdm]: https://github.com/noamraph/tqdm
[the_future_is_soon]: http://pyvideo.org/pycon-au-2010/pyconau-2010--the-future-is-soon.html
[cant_we_get_rid_of_the_gil]: https://docs.python.org/3/faq/library.html#id18
[it_isnt_easy_to_remove_the_gil]: http://www.artima.com/weblogs/viewpost.jsp?thread=214235
[python_threads_and_the_global_interpreter_lock]: http://jessenoller.com/2009/02/01/python-threads-and-the-global-interpreter-lock/
[understanding_the_python_gil]: http://www.dabeaz.com/GIL/
[understanding_the_python_gil_slide]: http://www.dabeaz.com/python/UnderstandingGIL.pdf
[embarrassingly_parallel]: https://en.wikipedia.org/wiki/Embarrassingly_parallel

### Chapter 18: Concurrency with asyncio

This chapter introduces `asyncio`, a package that implements concurrency with
 coroutines driven by an event loop. It's one of the largest and most ambitious
 libraries ever add to Python. Guido van Rossum developed `asyncio` outside of
 the Python repository and gave the project a code name of "Tulip". The main
 discussion group is still called python-tulip. Tulip was renamed to `asyncio`
 when it was added to the standard library in Python 3.4. It's also compatible
 with Python 3.3. Because it uses `yield from` expressions extensively,
 `asyncio` is incompatible with older versions of Python. The Trollius project
 is a backport of `asyncio` to Python 2.6 and newer, replacing `yield from` with
 `yield` and clever callables named `From` and `Return`.<br>
`asyncio` uses a stricter definition of "coroutine". A coroutine suitable for
 use with the `asyncio` API must use `yield from` and not `yield` in its body.
 Also, an `asyncio` coroutine should be driven by a caller invoking it through
 `yield from` or by passing the coroutine to one of the `asyncio` function such
 as `asyncio.async(...)` and others. Finally, the `@asyncio.coroutine` decorator
 should be applied to coroutines.<br>
The use of the `@asyncio.coroutine` decorator is not mandatory, but highly
 recommended: it makes the coroutines stand out among regular functions, and
 helps with debugging by issuing a warning when a coroutine is garbage collected
 without being yielded from. This is not a *priming decorator*.<br>
A `Task` object can be cancelled safely; `spinner.cancel()` raises
 `asyncio.CancelledError` at the `yield` line where the coroutine is currently
 suspended. The coroutine may catch the exception and delay or even refuse to
 cancel.<br>
Never use `time.sleep(...)` in `asyncio` coroutines unless you want to block the
 main thread, therefore freezing the event loop and probably the whole
 application as well. If a coroutine needs to spend some time doing nothing, it
 should `yield from asyncio.sleep(DELAY)`.<br>
A `Task` is like a green thread in libraries that implement cooperative
 multitasking, such as `gevent`.<br>
The `asyncio.Future` and the `concurrent.futures.Future` classes have mostly the
 same interface, but are implemented differently and are not interchangeable.
 `Task` is a subclass of `Future` designed to wrap a coroutine.<br>
In `asyncio.Future`, the `.result()` method takes no arguments, so you can't
 specify a timeout. If you call `.result()` and the future is not done, it
 raises `asyncio.InvalidStateError`. However, the usual way to get the result of
 an `asyncio.Future` is to `yield from` it.

```python
loop = asyncio.get_event_loop()
result = loop.run_until_complete(supervisor())
loop.close()

# for running asyncio code from a non-asyncio context
#  (like on the Python console or in small tests)
def run_sync(coro_or_future):
    return asyncio.get_event_loop().run_until_complete(coro_or_future)

try:
    with (yield from semaphore): # asyncio.Semaphore
        image = yield from get_flag(base_url, cc)
    except Exception as exc:
        raise FetchError(cc) from exc # PEP 3134 Exception Chaining and Embedded Tracebacks

return (yield from http_get(url))
# The parentheses are required because the Python parser gets confused.
```

`res = yield from foo()` works if `foo` is a coroutine function (therefore it
 returns a coroutine object when called) or if `foo` is a plain function that
 returns a `Future` or `Task` instance (due to `asyncio.async()`). <br>
As of Python 3.4, `asyncio` only supports TCP and UDP directly. For HTTP or any
 other protocol, we need third-party packages; `aiohttp` is the one everyone
 seems to be using for `asyncio` HTTP clients and servers at this time.<br>
Despite its name, `wait` is not a blocking function. `asyncio.wait(...)`
 coroutine accepts an iterable of futures or coroutines; `wait` wraps each
 coroutine in a `Task`. Because it is a coroutine function, calling `wait(...)`
 returns a coroutine/generator object.<br>
Behind the scenes Node.js implements a thread pool in C with the `libeio`
 library, to provide its callback-based file APIs--because as of 2014 there are
 no stable and portable asynchronous file handling APIs for most OSes.
`asyncio` does not provide an asynchronous filesystem API at this time--as Node
 does. If that becomes a bottleneck in your application, you can use the
 `loop.run_in_executor` function to run `save_flag` in a thread pool.<br>
The futures returned by `asyncio.as_completed` are not necessarily the same
 futures we pass into the `as_completed` call. Internally, the `asyncio`
 machinery replaces the future objects we provide with others that will, in the
 end, produce the same results.
 ([Guido's answer][which_other_futures_my_come_out_of_asyncio.as_completed])

```python
loop = asyncio.get_event_loop()
server_coro = asyncio.start_server(handle_queries, address, port, loop=loop)
server = loop.run_until_complete(server_coro)
host = server.sockets[0].getsockname()
try:
    loop.run_forever()
except KeyboardInterrupt:
    pass
server.close()
loop.run_until_complete(server.wait_closed())
loop.close()
```

high-level [Streams API][streams_api] that provides a ready-to-use server so you
 only need to implemenet a handler function, which can be a plain callback or a
 coroutine / lower-level
 [Transports and Protocols API][transports_and_protocols_api], inspired by the
 transport and protocols abstractions in the Twisted framework.

[Ryan Dahl: Introduction to Node.js][introduction_to_nodejs],
 [Fan-in and Fan-out: The crucial components of concurrency][fan_in_and_fan_out]
 ([video][fan_in_and_fan_out_video]),
 [A Web Crawler With asyncio Coroutines][web_crawler_with_asyncio_coroutines],
 [Python's asyncio is for composition, not raw performance][python_asyncio_is_for_composition_not_raw_performance],
 [PyCon US 2013 keynote][pycon_us_2013_keynote],
 [Guido van Rossum on Tulip][guido_van_rossum_on_tulip],
 [Tulip: Async I/O for Python 3][tulip_async_io_for_python_3],
 [A deep dive into PEP-3156 and the new asyncio module][deep_dive_into_pep3156_and_new_asyncio_module]
 ([video][deep_dive_into_pep3156_and_new_asyncio_module_video]),
 [Using futures for async GUI programming in Python 3.3][using_futures_for_async_gui_programming_in_python_3_3]
 ([code][using_futures_for_async_gui_programming_in_python_3_3_code]),
 [The new Python asyncio module aka "tulip"][new_python_asyncio_module_aka_tulip]
 (list of relevant links), [aio-libs][aio_libs], [Vaurien][vaurien] (chaos TCP
 proxy) from [Mozilla Services][mozilla_services],
 [Callbacks are imperative, promises are functional: Node's biggest missed opportunity][node_biggest_missed_oppertunity],
 Tornado's [AsyncIOMainLoop][asynciomainloop], [Quamash][quamash],
 [Autobahn|Python][autobahn_python] and [WebSockets][websockets_library] for
 WebSockets, [Deconstructing Deferred][deconstructing_deferred],
 [WWTD (What Would Twisted Do?)][what_would_twisted_do]

[which_other_futures_my_come_out_of_asyncio.as_completed]: https://groups.google.com/forum/#!msg/python-tulip/PdAEtwpaJHs/7fqb-Qj2zJoJ
[streams_api]: https://docs.python.org/3/library/asyncio-stream.html
[transports_and_protocols_api]: https://docs.python.org/3/library/asyncio-protocol.html
[introduction_to_nodejs]: https://www.youtube.com/watch?v=M-sc73Y-zQA
[fan_in_and_fan_out]: https://speakerdeck.com/pycon2014/fan-in-and-fan-out-the-crucial-components-of-concurrency-by-brett-slatkin
[fan_in_and_fan_out_video]: https://www.youtube.com/watch?v=CWmq-jtkemY
[web_crawler_with_asyncio_coroutines]: http://aosabook.org/en/500L/a-web-crawler-with-asyncio-coroutines.html
[python_asyncio_is_for_composition_not_raw_performance]: http://www.onebigfluke.com/2015/02/asyncio-is-for-composition.html
[pycon_us_2013_keynote]: http://pyvideo.org/pycon-us-2013/keynote-1.html
[guido_van_rossum_on_tulip]: https://www.youtube.com/watch?v=aurOB4qYuFM
[tulip_async_io_for_python_3]: https://www.youtube.com/watch?v=1coLC-MUCJc
[deep_dive_into_pep3156_and_new_asyncio_module]: https://www.slideshare.net/saghul/asyncio
[deep_dive_into_pep3156_and_new_asyncio_module_video]: https://www.youtube.com/watch?v=MS1L2RGKYyY
[using_futures_for_async_gui_programming_in_python_3_3]: http://pyvideo.org/pycon-us-2013/using-futures-for-async-gui-programming-in-python.html
[using_futures_for_async_gui_programming_in_python_3_3_code]: https://github.com/fluentpython/asyncio-tkinter
[new_python_asyncio_module_aka_tulip]: http://haypo-notes.readthedocs.io/asyncio.html
[aio_libs]: https://github.com/aio-libs
[vaurien]: http://vaurien.readthedocs.io/
[mozilla_services]: https://mozilla-services.github.io/
[node_biggest_missed_oppertunity]: https://blog.jcoglan.com/2013/03/30/callbacks-are-imperative-promises-are-functional-nodes-biggest-missed-opportunity/
[asynciomainloop]: http://tornado.readthedocs.io/en/latest/asyncio.html
[quamash]: https://pypi.python.org/pypi/Quamash/
[autobahn_python]: https://github.com/crossbario/autobahn-python
[websockets_library]: https://websockets.readthedocs.io/en/stable/
[deconstructing_deferred]: https://groups.google.com/forum/#!msg/python-tulip/ut4vTG-08k8/PWZzUXX9HYIJ
[what_would_twisted_do]: https://groups.google.com/forum/#!msg/python-tulip/pPMwtsCvUcw/eIoX_n8FSPwJ

## VI. Metaprogramming

### Chapter 19: Dynamic Attributes and Properties

The special method that actually constructs an instance is `__new__`: it's a
 class method (but gets special treatment, so the `@classmethod` decorator is
 not used), and it must return an instance. That instance will in turn be passed
 as the first argument `self` of `__init__`. The real constructor is
 `__new__`--which we rarely need to code because the implementation inherited
 from `object` suffices. The `__new__` method can also return an instance of a
 different class, and when that happens, the interpreter does not call
 `__init__`.<br>
`@property`, `@weight.setter` and `@weight.deleter`,
 `property(fget=None, fset=None, fdel=None, doc=None)`. Although often used as a
 decorator, the `property` built-in is actually a class.<br>
Properties override instance attributes, and instance attributes override class
 atrributes.
The property factory would be placed in a utility module to be used over and
 over again. Eventually that simple factory could be refactored into a more
 extensible descriptor class, with specialized subclasses performing different
 validations.<br>
`dir([object])` lists most attributes of the object. The `__dict__` attribute
 itself is not listed by `dir`, but the `__dict__` keys are listed. Several
 special attributes of classes, such as `__mro__`, `__bases__`, and `__name__`
 are not listed by `dir` either. `vars([object])` returns the `__dict__` of
 `object`; `vars` can't deal with instances of classes that define `__slots__`
 and don't have a `__dict__` (contrast with `dir`, which handles such
 instances). Without an argument, `vars()` does the same as `locals()`.<br>
Attribute access using either dot notation or the built-in functions `getattr`,
 `hasattr`, and `setattr` trigger the appropriate special methods
 (`__delattr__`, `__dir__`, `__getattr__`, `__getattribute__` and
 `__setattr__`). Reading and writing attributes directly in the instance
 `__dict__` does not trigger these special methods--and that's the usual way to
 bypass them if needed. Assume that the special methods will be retrieved on the
 class itself, even when the target of the action is an instance. For this
 reason, special methods are not shadowed by instance attributes with the same
 name. In practice, because they are unconditionally called and affect
 practically every attribute access, the `__getattribute__` and `__setattr__`
 special methods are harder to use correctly than `__getattr__`--which only
 handle nonexisting attribute names. Using properties or descriptors is less
 error prone than defining these special methods.

```python
with urlopen(URL) as remote, open(JSON, 'wb') as local: # since Python 2.7 and 3.1
    local.write(remote.read())

class Record: # the "bunch" idiom
    def __init__(self, **kwargs):
        self.__dict__.update(kwargs)

self.__class__.fetch # if a event record had a key named 'fetch'

def test_missing_db_exception():
    with pytest.raises(schedule.MissingDatabaseError):
        schedule.DbRecord.fetch('venue.1585')
```

[Class-level read-only properties in Python][class_level_read_only_properties_in_python],
 [Uniform Access Principle][uniform_access_principle],
 [Java's *new* Considered Harmful][java_new_considered_harmful]

[class_level_read_only_properties_in_python]: https://stackoverflow.com/questions/1735434/class-level-read-only-properties-in-python
[uniform_access_principle]: http://wiki.c2.com/?UniformAccessPrinciple
[java_new_considered_harmful]: http://www.drdobbs.com/javas-new-considered-harmful/184405016

### Chapter 20: Attribute Descriptors

Descriptors are a way of reusing the same access logic in multiple attributes.
 For example, field types in ORMs such as the Django ORM and SQL Alchemy are
 descriptors, managing the flow of data from the fields in a database record to
 Python object attributes and vice versa. A descriptor is a class that
 implements a protocol consisting of the `__get__`, `__set__`, and `__delete__`
 methods. The `property` class implements the full descriptor protocol. Besides
 properties, other Python features that leverage descriptors are methods and the
 `classmethod` and `staticmethod` decorators. Understanding descriptors is key
 to Python mastery.<br>
If a descriptor does not implement `__set__`, then it's a nonoverriding
 descriptor. Setting an instance attribute with the same name will shadow the
 descriptor, rendering it ineffective for handling that attribute in that
 specific instance. Methods are implemented as nonoverriding descriptors.
 Overriding descriptors are also called data descriptors or enforced
 descriptors. Nonoverriding descriptors are also known as nondata descriptors or
 shadowable descriptors.<br>
Regardless of whether a descriptor is overriding or not, it can be overwritten
 by assignment to the class. This is a monkey-patching technique. Although the
 reading of a class attribute can be controlled by a descriptor with `__get__`
 attached to the managed class, the writing of a class attribute cannot be
 handled by a descriptor with `__set__` attached to the same class.<br>
As usual with descriptors, the `__get__` of a function returns a reference to
 itself when the access happens through the managed class. But when the access
 goes through an instance, the `__get__` of the function returns a bound method
 object. The bound method object has a `__self__` attribute holding a reference
 to the instance on which the method was called. The `__func__` attribute of the
 bound method is a reference to the original function attached to the managed
 class. The bound method object also has a `__call__` method, which handles the
 actual invocation. This method calls the original function referenced in
 `__func__`, passing the `__self__` attribute of the method as the first
 argument. That's how the implicit binding of the conventional `self` argument
 works.<br>
Descriptor Usage Tips:
* Use property to Keep It Simple
* Read-only descriptors require `__set__`
* Validation descriptors can work with `__set__` only
* Caching can be done efficiently with `__get__` only. The namesake instance
  attribute will shadow the descriptor, so subsequent access to that attribute
  will fetch it directly from the instance `__dict__` and not trigger the
  descriptor `__get__` anymore.
* Nonspecial methods can be shadowed by instance attributes. However, this issue
  does not interfere with special methods. The interpreter only looks for
  special methods in the class itself, in other words, `repr(x)` is executed as
  `x.__class__.__repr__(x)`.

```python
class Quantity:
    def __init__(self, storage_name):
        self.storage_name = storage_name
    def __set__(self, instance, value):
        if value > 0:
            instance.__dict__[self.storage_name] = value
        else:
            raise ValueError('value must be > 0')
```

[Descriptor HowTo Guide][descriptor_howto_guide],
 [Python's Object Model][python_object_model]
 ([slide][python_object_model_slide]),
 [The Rise of Worse is Better][rise_of_worse_is_better],
 [Python Warts][python_warts],
 [Adding Support for User-defined Classes][adding_support_for_user_defined_classes]

[descriptor_howto_guide]: https://docs.python.org/3/howto/descriptor.html
[python_object_model]: https://www.youtube.com/watch?v=VOzvpHoYQoo
[python_object_model_slide]: http://www.aleax.it/Python/nylug05_om.pdf
[rise_of_worse_is_better]: http://dreamsongs.com/RiseOfWorseIsBetter.html
[python_warts]: http://web.archive.org/web/20031002184114/www.amk.ca/python/writing/warts.html
[adding_support_for_user_defined_classes]: http://python-history.blogspot.com/2009/02/adding-support-for-user-defined-classes.html

### Chapter 21: Class Metaprogramming

Classes are first-class objects in Python, so a function can be used to create a
 new class at any time, without using the `class` keyword. Metaclasses (class
 factory class) are powerful, but hard to get right. Class decorators (since
 Python 2.6) solve many of the same problems more simply.<br>
We usually think of `type` as a function, because we use it like one, e.g.,
 `type(my_object)` to get the class of the object--same as
 `my_object.__class__`. However, `type` is a class. It behaves like a class that
 creates a new class when invoked with three arguments:
 `type(name, bases, dict)`.<br>
A class decorator is very similar to a function decorator: it's a function that
 gets a class object and returns the same class or a modified one. A significant
 drawback of class decorators is that they act only on the class where they are
 directly applied. This means subclasses of the decorated class may or may not
 inherit the changes made by the decorator, depending on what those changes
 are.<br>
In particular, the `import` statement is not merely a declaration but it
 actually runs all te top-level code of the imported module when it's imported
 for the first time in the process--further imports of the same module will use
 a cache, and only name binding occurs then. That top-level code may do
 anything, including actions typical of "runtime", such as connecting to a
 database. The interpreter compiles the function body (if it's the first time
 that module is imported), and binds the function object to its global name, but
 it does not execute the body of the function, obviously. But the interpreter
 invokes the decorator function at import time. For classes, at import time, the
 interpreter executes the body of every class, even the body of classes nested
 in other classes. Execution of a class body means that the attributes and
 methods of the class are defined, and then the class object itself is
 built.<br>
By default, Python classes are instances of `type`. In other words, `type` is
 the metaclass for most built-in and user-defined classes. `object` is an
 instance of `type`, and `type` is a subclass of `object`. `type` is an instance
 of itself. Only metaclasses are subclasses of `type`, so they act as class
 factories. In particular, a metaclass can customize its instances by
 implementing `__init__`.<br>
(`class ClassFive(metaclass=MetaAleph)`) The Python interpreter evaluates the
 body of `ClassFive` but then, instead of calling `type` to build the actual
 class body, it calls `MetaAleph`. `MetaAleph` has the
 `__init__(cls, name, bases, dic)` method to initialize its instances.<br>
A metaclass can customize a hierarchy of classes--in contrast with a class
 decorator, which affects a single class and may have no impact on its
 descendants.<br>
However, by default, that mapping is a `dict`, which means the order of the
 attributes as they appear in the class body is lost by the time our metaclass
 or class decorator can look at them. The solution to this problem is the
 `__prepare__` special method, introduced in Python 3. This special method is
 relevant only in metaclasses, and it must be a class method. it must return a
 mapping, which will be received as the last argument by `__new__` and then
 `__init__` when the metaclass builds a new class.<br>
metaclass usage - attribute validation, applying decorators to many methods at
 once, object serialization or data conversion, object-relational mapping,
 object-based persistency, dynamic translation of class structures from other
 languages<br>
`cls.__bases__`, `cls.__qualname` (since 3.3), `cls.__subclasses__()` (returns
 the list of subclasses that currently exist in memory), `cls.mro()` (a
 metaclass can override this method)

[Acrimony in c.l.p.][acrimony_in_clp],
 [collections.nameduple source code][collections_nameduple_src] and `._source`
 attribute, [`types` module][types_module],
 [Class Decorators: Radically Simple][class_decorators_radically_simple],
 [Meta-classes Made Easy: Eliminating self with Metaclasses][metaclasses_made_easy],
 [Unifying types and classes in Python 2.2][unifying_types_and_classes_in_python_2_2],
 *Putting Metaclasses to Work: a New Dimension in Object-Oriented Programming*
 (Addison-Wesley, 1998),
 [PEP 487 -- Simpler customisation of class creation][pep_487] (introduces
 `__init_subclass__`), [MacroPy][macropy],
 *[Simply Scheme: Introducing Computer Science][simply_scheme]* (MIT, 1999),
 *Machine Beauty: Elegance And The Heart Of Technology* (Basic Books, 1998)

[acrimony_in_clp]: https://mail.python.org/pipermail/python-list/2002-December/134521.html
[collections_nameduple_src]: https://hg.python.org/cpython/file/3.4/Lib/collections/__init__.py
[types_module]: https://docs.python.org/3/library/types.html
[class_decorators_radically_simple]: https://www.youtube.com/watch?v=cAGliEJV9_o
[metaclasses_made_easy]: http://www.voidspace.org.uk/python/articles/metaclasses.shtml
[unifying_types_and_classes_in_python_2_2]: https://www.python.org/download/releases/2.2.3/descrintro/
[pep_487]: https://www.python.org/dev/peps/pep-0487/
[macropy]: https://github.com/lihaoyi/macropy
[simply_scheme]: https://people.eecs.berkeley.edu/~bh/ss-toc2.html

## Afterword

[A Python Æsthetic: Beauty and Why I Python][python_aesthetic_beauty_and_why_i_python],
 [Transforming Code into Beautiful, Idiomatic Python][transforming_code_into_beautiful_idiomatic_python],
 [Evolution of Style Guides][evolution_of_style_guides], [flake8][flake8],
 [Google Python Style Guide][google_python_style_guide],
 [Pocoo Style Guide][pocoo_style_guide],
 *[The Hitchhiker's Guide to Python][hitchhiker_guide_to_python]* (O'Reilly,
 2016),
 [Code Like a Pythonista: Idiomatic Python][code_like_a_pythonista_idiomatic_python],
 [What is Pythonic?][what_is_pythonic] ([thread][what_is_pythonic_thread]),
 [PEP 3099 -- Things that will Not Change in Python 3000][pep_3099],
 [Python Essays][python_essays], Zen of Python (`import this`)

[python_aesthetic_beauty_and_why_i_python]: https://www.youtube.com/watch?v=x-kB2o8sd5c
[transforming_code_into_beautiful_idiomatic_python]: https://www.youtube.com/watch?v=OSGv2VnC0go
[evolution_of_style_guides]: https://mail.python.org/pipermail/python-ideas/2015-March/032557.html
[flake8]: https://pypi.python.org/pypi/flake8
[google_python_style_guide]: https://google.github.io/styleguide/pyguide.html
[pocoo_style_guide]: http://www.pocoo.org/internal/styleguide/
[hitchhiker_guide_to_python]: http://docs.python-guide.org/en/latest/
[code_like_a_pythonista_idiomatic_python]: http://python.net/~goodger/projects/pycon/2007/idiomatic/handout.html
[what_is_pythonic]: https://blog.startifact.com/posts/older/what-is-pythonic.html
[what_is_pythonic_thread]: https://mail.python.org/pipermail/tutor/2003-October/025930.html
[pep_3099]: https://www.python.org/dev/peps/pep-3099/
[python_essays]: https://www.python.org/doc/essays/

## Appendix: Support Scripts

