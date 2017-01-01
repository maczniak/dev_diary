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
* flat sequences - str (immutable), bytes (immutable), bytearray, memoryview (without copying, [tutorial][memoryview_tutorial]), array.array (number only)

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
[origins_of_pythons_functional_features]: http://python-history.blogspot.kr/2009/04/origins-of-pythons-functional-features.html
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
[tail_recursion_elimination]: http://neopythonic.blogspot.kr/2009/04/tail-recursion-elimination.html

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
`functools.wraps` (helper that copies the relevant attributes from a decorated function), `.lru_cache` (memoization), `.singledispatch` (generic function, depending on the type of the first argument, `@<base_function>.register(<type>)`, Python 3.4 [PEP 443][pep_443], old singledispatch package)<br>
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

[The C Family of Languages: Interview with Dennis Ritchie, Bjarne Stroustrup, and James Gosling][c_family_of_languages_interview]

[c_family_of_languages_interview]: http://www.gotw.ca/publications/c_family_interview.htm

## V. Control Flow

### Chapter 14: Iterables, Iterators, and Generators

### Chapter 15: Context Managers and else Blocks

### Chapter 16: Coroutines

### Chapter 17: Concurrency with Futures

### Chapter 18: Concurrency with asyncio

## VI. Metaprogramming

### Chapter 19: Dynamic Attributes and Properties

### Chapter 20: Attribute Descriptors

### Chapter 21: Class Metaprogramming

## Appendix: Support Scripts

