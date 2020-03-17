# [Python for Finance, 2nd Edition: Mastering Data-Driven Finance][homepage] by Yves Hilpisch, O'Reilly (2018)

[The Python Quants][tpq], [The AI Machine][aimachine], [DX Analytics][dx_analytics], [meetup][meetup], [bootcamp with CQF Institute][bootcamp], [QuantInsti EPAT][quantinsti]

CPython 3.7

[homepage]: http://shop.oreilly.com/product/0636920117728.do
[tpq]: https://tpq.io
[aimachine]: https://aimachine.io
[dx_analytics]: https://dx-analytics.com
[meetup]: https://www.meetup.com/Python-for-Quant-Finance-London/
[bootcamp]: https://fpq.io
[quantinsti]: https://www.quantinsti.com

## I. Python and Finance

### 1. Why Python for Finance

For example, modern or mean-variance portfolio theory (MPT), the capital asset
 pricing model (CAPM), and value-at-risk (VaR) all have their foundations in
 daily stock price data.<br>
Something that might also be a little bit surprising is that the lack of
 consistent theories oftehn is addressed by technological approaches, in that
 high-speed algorithms exploit market microstructure elements (e.g., order flow,
 bid-ask spreads) rather than relying on some kind of financial reasoning.<br>
[`numexpr`][numexpr] *compiles* the expression to improve upon the performance
 of the general `NumPy` functionality by, for example, avoiding in-memory copies
 of `ndarray` objects along the way. `numexpr` also has built-in capabilities to
 parallelize the execution of the respective operation.<br>
Data-Driven and AI-First Finance<br>
Some of the most important financial theories, such as MPT and CAPM, date as far
 back as to the 1950s and 1960s. However, they still represent a cornerstone in
 the education of students in such fields as economics, finance, financial
 engineering, and business administration. This might be surprising since the
 empirical support for most of these theories is meager at best, and the
 evidence is often in complete contrast to what the theories suggest and imply.
 On the other hand, their popularity is understandable since they are close to
 humans' expectations of how financial markets might behave and since they are
 elegant mathematical theories resting on a number of appealing, if in general
 too simplistic, assumptions.<br>
The history of (quantitative) finance in large parts contradicts the scientific
 method. In many cases, theories and models have been developed "from scratch"
 on the basis of simplifying mathematical assumptions with the goal of
 discovering elegant answers to central problems in finance. Among others,
 popular assumptions in finance are normally distributed returns for financial
 instruments and linear relationships between quantities of interest. Since
 these phenomena are hardly ever found in financial markets, it should not come
 as a surprise that empirical evidence for the elegant theories is often
 lacking. Many financial theories and models have been formulated, proven, and
 published first and have only later been tested empirically.
[Eikon Data APIs][eikon_data_apis] (structured financial data such as historical
 price data, unstructured data such as news articles)

[eikon_data_apis]: https://developers.refinitiv.com/eikon-apis/
[numexpr]: https://numexpr.readthedocs.io/en/latest/

### 2. Python Infrastructure

`pipenv`<br>
Intel Math Kernel Library (`mkl`)<br>
JupyterHub allows the management of multiple users for a Jupyter Notebook
 server.

## II. Mastering the Basics

### 3. Data Types and Structures

The `Cython` package brings static typing and compiling features to Python that
 are comparable to those in C. In fact, `Cython` is not only a *package*, it is
 a full-fledged hybrid *programming language* combining Python and C.

### 4. Numerical Computing with NumPy

*Universal functions* are "universal" in the sense that they in general operate on `ndarray` objects as well as on basic Python data types. However, when applying universal functions to, say, a Python `float` object, one needs to be aware of the reduced performance compared to the same functionality found in the `math` module.<br>
*`order`* (*optional*) - `C` for C-like (i.e., row-wise) or `F` for Fortrna-like (i.e., column-wise)<br>
*NumPy dtype objects* - `?` (Boolean), `i8`, `u8`, `f8`, `c32`, `m` (`timedelta`, 64-bit), `M` (`datetime`, 64-bit), `O`, `U24` (24 Unicode characters), `V12` (Raw data (void))<br>
If `resize()` increases the numer of elements, it repeats elements.<br>
`h[(h < 4) | (h >= 12)]`, `np.where(h <= 7, h * 2, h / 2)`<br>
`x['x']` -> `x = x.view(np.recarray)` -> `x.x`<br>
`dt = np.dtype([('Name', 'S10'), ('Age', 'i4'), ('Height', 'f'), ('Children/Pets', 'i4', 2)])`,
 `dt = np.dtype({'names': ['Name', 'Age', 'Height', 'Children/Pets'], 'formats':'O int float int,int'.split()})`,
 `s['Height'].mean()`<br>
`.reshape(-1, 1)` (the value is inferred from the length of the array and
 remaining dimensions)<br>
`simple_function(numpy_array)` or `np.vectorize(f)`

### 5. Data Analysis with pandas

Contrary to `NumPy` `ndarray` objects, enlarging the `DataFrame` object in both
 dimensions is possible.<br>
Using an `ndarray` object is generally a good choice since `pandas` will retain
 the basic structure and will "only" add metainformation (e.g., index
 values).<br>
`date_range(start, end, periods, freq, tz, normalize, name)` `DatetimeIndex`
* `B` - Business day
* `C` - Custom business day (experimental)
* `D` - Calendar day
* `W` - Weekly
* `M`, `BM` - Month end
* `MS`, `BMS` - Month start
* `Q`, `BQ`, `QS`, `BQS` - Quarter end
* `A`, `BA`, `AS`, `BAS` - Year end
* `H` - Hourly
* `T` - Minutely
* `S` - Secondly
* `L` - Milliseconds
* `U` - Microseconds

`.info()`, `.describe()`<br>
`groups = df.groupby(['Quarter', 'Odd_Even'])`,
 `groups[['No1', 'No4']].aggregate([sum, np.mean])`<br>
`df[df['x'] > 0]` or `df.query('x > 0')`<br>
A join can also happen based on an empty `DataFrame` object. In this case, the
 columns are created *sequentially*.

### 6. Object-Oriented Programming

> The purpose of software engineering is to control complexity, not to create
> it. ---Pamela Zave

## III. Financial Data Science

### 7. Data Visualization

A package that makes it convenient to create suck D3.js plots with Python is
 `plotly`. A smaller additional library, called `Cufflinks`, tightly integrates
 `plotly` with `pandas` `DataFrame` objects and allows for the creation of
 popular financial plots (such as candlestick charts, `QuantFigure` feature).

```python
import matplotlib as mpl
mpl.__version__ # 3.0.0
import matplotlib.pyplot as plt
plt.style.use('seaborn')
mpl.rcParams['font.family'] = 'serif'
%matplotlib inline
```

By using the `plt.subplots()` function, one gets direct access to the underlying
 plotting objects (the figure, subplots, etc.). `plt.subplot()` takes as
 arguments three integers for `numrows`, `numcols`, and `fignum` (either
 separated by commas or not).<br>
`plt.setp(ax, xticklabels=...)`, `plt.setp(line, linestype=...)`

### 8. Financial Time Series

### 9. Input/Output Operations

### 10. Performance Python

### 11. Mathematical Tools

### 12. Stochastics

### 13. Statistics

## IV. Algorithmic Trading

### 14. The FXCM Trading Platform

### 15. Trading Strategies

### 16. Automated Trading

## V. Derivatives Analytics

### 17. Valuation Framework

### 18. Simulation of Financial Models

### 19. Derivatives Valuation

### 20. Portfolio Valuation

### 21. Market-Based Valuation

