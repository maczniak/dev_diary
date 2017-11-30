# [Mastering matplotlib][homepage] by Duncan M. McGreggor, Packt Publishing (2015)

[source code][source_code], [color images][color_images]

[homepage]: https://www.packtpub.com/big-data-and-business-intelligence/mastering-matplotlib
[source_code]: https://github.com/masteringmatplotlib/notebooks
[color_images]: https://www.packtpub.com/sites/default/files/downloads/7542OS_ColoredImages.pdf

## Chapter 1: Getting Up to Speed

With the addition of the `nbagg` IPython Notebook backend to matplotlib in
 version 1.4, users can now work with plots in a browser very much like they've
 been able to do in the GTK and Qt apps on the desktop.

## Chapter 2: The matplotlib Architecture

Figures can be displayed and interacted with via ommon user interface events
 such as the keyboard and mouse inputs. This layer of interaction with common
 user interface is called the backend layer. A Figure needs to be composed of
 multiple objects that should be individually modifiable, but it should be
 implemented in such a way that it has a positive and predictable impact on the
 other aspects of the Figure. This logical layer is responsible for the
 abstraction of each visual component that one sees in a Figure. Due to its
 highly visual nature, this layer was identified as the more general concept of
 creating visual art and is thus referred to as the artist layer. Lastly, the
 Figure needs to support programmatic interaction and provide the users with the
 ability to manipulate Figues with a syntax that is as clean and intuitive as
 possible. This is called the scripting layer.

1. The user creates either the data that he/she wants to plot or the functions
   that generate this data
1. This data is either created or loaded in the scripting layer
1. The data is transformed into various objects in the artist layer; it is
   adjusted as scripted
1. These objects are then rendered by the backend, which ultimately provides the
   configured toolkit with the raw data necessary to place an image on the
   toolkit's canvas

* User interface backends (interactive) - GTK 2.x and GTK 3.x, wxWidgets, Tk,
  Qt4 and Qt5, Mac OS X Cocoa
* Hardcopy backends (noninteractive) - PS, PDF, SVG, PNG, Cairo, AGG, GDK, NBAGG

The `Artist` subclasses can be classified into one of the following two groups:
 Primitives and Containers. Of all the containers, the `Axes` class is one of
 the most important. It is the primary omver and shaker in the artist layer. The
 reason for this is simple---the `Axes` instances are where most of the
 matplotlib objects go (both primitives and other containers). In addition to
 the creation of primitives, the methods of this class can prepare the supplied
 data that is needed for the creation of primitives such as lines and shapes,
 add them to the appropriate containers, and draw them when called by some other
 objects. Furthermore, the `Axes` objects set the coordinate system for the
 figure and track callbacks that can be connected to the `xlim_changes` and
 `ylim_changes` events. The callbacks will be called with the `Axes` instances
 as an argument.<br>
Another component of the artist layer that we will touch on briefly is
 collections. These are the classes that provide for the efficient drawing of
 large numbers of similar objects. If you find yourself creating tens or
 hundreds of thousands of circles, polygons, lines, and so on, in most cases
 you will get much better performance from matplotlib if you put these in
 collections.<br>
However, for the scientists' daily use, data visualization, or exploratory
 interactions, `pyplot`--the scripting layer--is a better option. This is what
 we use in most of the IPython Notebooks in this book. The `pyplot` interface is
 much less verbose. Under the covers, `pyplot` uses module-level objects to
 trakc the state of the data so that the user does not have to create things
 like figures, axes, canvases, figure canvas managers, or preferred backends.
 The `pyplot` interface defines a series of functions that depend on the
 components returned by the `setup` function. These include the following
 functions:
* `plot()`: This function calls the `plot` method in the current figure's `Axes`
  object and the figure canvas's `draw*` method (as identified in the preceding
  setup)
* `title()`: This function sets the title of the current figure's `Axes`
  instance
* `savefig()`: This function saves the current figure
* `draw()`: This function redraws the current figure
* `gcf()`: This function returns the current figure
* `gca()`: This function returns the `Axes` instance of the current figure
* `get_current_fig_manager()`: This returns the current figure manager
* `figure()`: This is a Figure factory function
* `switch_backend()`: This is a function that lets one easily change the
  selected backend

If one wishes to use the scripting layer, `pyplot` is the community-recommended
 approach. However, you may come across references to another scripting layer
 interface when digging through the source code or poking around in matplotlib's
 documentation, `pylab`. The `pylab` interface is the procedural interface of
 matplotlib, and it was modeled after the commands that MATLAB provides.
 However, not that `pylab` is deprecated and its use is discouraged by the
 community.

the NetworkX library and its matplotlib integration<br>
`plt.rcParams['backend']`

## Chapter 3: matplotlib APIs and Integrations

If you're going to generate huge numbers of plots, process vast quantities of
 data, or have particular needs for highly customized plots, you'll want to skip
 the scripting layer and work directly with the artist or backend layers.<br>
Take a look at the creation of the figure manager in the constructor method of
 the example class---we directly interfaced with the backend layer. Likewise,
 when we obtain the figure reference from the canvas, this is the domain of the
 backend layer.<br>
If you would like to view the files in the notebook, you can import the image
 display class from IPython in the following way: `Image("expmt_1.png")`
 (`from IPython.display import Image`)

## Chapter 4: Event Handling and Interactive Plots

Since we will be using the IPython Notebook backend---which is also known as
 `nbagg`---for interactive plots, we will not enter the usual
 `%matplotlib inline` command.<br>
The functionality of a websocket-enabled HTTP server is provided by Tornado, a
 Python web framework, and an asynchronous networking library. Your browser
 provides the infrastructure necessary to support the websocket communications
 with the web server. Through ZeroMQ, IPython provides the means to send the
 data it receives over websockets to the rest of the components in the IPython
 architecture.

* `MouseEvent` - `button_press_event`, `button_release_event`,
  `motion_notify_event`, `scroll_event`
* `DrawEvent` - `draw_event`
* `KeyEvent` - `key_press_event`, `key_release_event`
* `PickEvent` - `pick_event`
* `ResizeEvent` - `resize_event`
* `LocationEvent` - `figure_enter_event`, `figure_leave_event`,
  `axes_enter_event`, `axes_leave_event`

The canvas object has an `mpl_connect` method, which must be called if you want
 to provide custom user interaction features along with your plots. This method
 just takes the following two arguments: A string value for the event and a
 callback function (or method)<br>
As of matplotlib version 1.4 and IPython version 2.3, keyboard events for plots
 are not supported in the `nbagg` backend. Therefore, we will use IPython from
 the terminal for this section.<br>
You need to write dispatch method or function for every key (or key combination)
 press and release event.<br>
We can execute callbacks when the mouse enters and leaves figures or axes in a
 way that is similar to the connection of the keyboard and mouse events. It will
 allow you to provide a visual feedback to the user regarding the subplot that
 is our focus or even expose a larger view of the focused plot.<br>
Object picking is one of the greatest, although unsung, features of matplotlib
 as it allows one to essentially create a custom data browser that is capable of
 revealing the details in the deeply nested or rich data across large scales.
 Thanks to this capability, in conjunction with matplotlib's custom styles, one
 can easily create beautiful, compelling data visualization applications that
 are tailored for the needs of the user. Every `Artist` instance has an
 attribute called `picker`. The setting of this attribute is what enables object
 picking in matplotlib.
* If the result if `True`, picking is enabled for the artist object and
  `pick_event` will be fired every time a mouse event occurs over the artist
  object in the figure.
* If the result is a number (for instance, `float` or `int`), the value is
  interpreted as a *tolerance*. If the event's data (such as the *x* and *y*
  values) is within the value of this *tolerance*, `pick_event` will be fired.
* If the result is a callable, then the provided function or method returns a
  boolean value, which determines whether `pick_event` is fired.
* If the result is `None`, picking is disabled.

The navigation toolbar widget is available for all backends (including the
 `nbagg` backend for IPython when it is not in the `inline` mode). When a
 toolbar action is engaged, the `toolbar` instance sets the toolbar's current
 mode.<br>
As a matter of fact, the `matplotlib.backend_bases.NavigationToolbar2` toolbar
 class is an excellent place to look for examples of compound events. When these
 buttons are pressed and the callbacks are fired, old events are disconnected
 and the new ones are connected. In this way, a chain of event may be set up
 with a particular sequence of events firing only a particular set of callbacks
 in a particular order.<br>
Since we do want to redraw and there is no pan event to connect to, there are
 two options:
* Piggyback on `draw_event`, which fires each time the canvas is moved
* Use `button_release_event` (and call `event.canvas.draw()`), which will fire
  when the panning is complete

## Chapter 5: High-level Plotting and Data Analysis

[Milestones in the History of Data Visualization: A Case Study in Statistical Historiography][milestones_in_the_history_of_data_visualization]
 by Michael Friendly (2005,
 [slide][milestones_in_the_history_of_data_visualization_slide])<br>
`matplotlib.sankey` module to visually depict the thermal efficiency of steam
 engines<br>
Leland Wilkinson published the book, *The Grammar of Graphics, Springer
 Publishing* (2005), which, as had been the case with Dr. Cleveland, was the
 culmination from a combined background of academic research and teaching with
 professional experience of developing statistical software platforms. The first
 software implementation that was inspired by this book was SPSS's nViZn
 (pronounced as *envision*). This was followed by R's ggplot2 and in the Python
 world, Bokeh, among others. The book provides a conceptual framework for the
 cognitive analysis of our statistical tools and how we can make them better,
 allowing us to ultimately create visualizations that are more clear,
 meaningful, and reveal more of the underlying problem space.<br>
In many ways, Bokeh views itself as a natural successor to matplotlib, offering
 their view of improvements in the overall architecture, scalability of problem
 datasets, APIs, and usability. However, in contrast to matplotlib, Bokeh
 focuses its attention on the web browser. Bokeh provides a matplotlib
 compatibility layer. It doesn'y cover the complete matplotlib API usage a given
 project may entail, but enough.<br>
A few years ago, the ŷhat company open-sourced a project of theirs: a clone of
 R's ggplot2 for Python.<br>
`matplotlib.dates.YearLocator`<br>
Not to be left behind, matplotlib has embraced the sensibilities of the ggplot
 world and has supported the `ggplot` style since its 1.4 release.
 `plt.style.available`, `plt.style.use('ggplot')`<br>
The Pandas project describes generic Python as a great tool for data managing
 and preparation, but not much strong in the areas of data analysis and
 modeling. This is the area that Pandas was envisioned to focus upon, filling a
 much-needed gap in the suite of available libraries, and allowing one to carry
 out the entire data analysis workflows in Python without having to switch to
 tools like R or SPSS.<br>
SciPy provides just the thing: spline interpolation
 (`scipy.interpolate.UnivariateSpline`). This will give us a smooth curve for
 our discrete values.<br>
Andrews' curves from the Pandas library can be useful when attempting to uncover
 a hidden structure in datasets of higher dimensions.

[milestones_in_the_history_of_data_visualization]: http://www.datavis.ca/papers/gfkl.pdf
[milestones_in_the_history_of_data_visualization_slide]: http://www.math.yorku.ca/SCS/Gallery/milestone/Visualization_Milestones.pdf

## Chapter 6: Customization and Configuration

## Chapter 7: Deploying matplotlib in Cloud Environments

## Chapter 8: matplotlib and Big Data

## Chapter 9: Clustering for matplotlib

