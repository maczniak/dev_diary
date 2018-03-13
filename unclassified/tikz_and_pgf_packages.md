# [The Ti*k*Z and PGF Packages][homepage]

Manual for version 3.0.1 (November 3, 2017)

[homepage]: http://pgf.sourceforge.net

### 1. Introduction

## I. Tutorials and Guidelines

### 2. Tutorial: A Picture for Karl's Students

```
\documentclass{article}
\usepackage{tikz}
\begin{document}
\begin{tikzpicture}
  [scale=3,
  important line/.style={very thick}]

  \colorlet{anglecolor}{green!50!black}

  \draw[help lines,step=0.5cm] (-1.4,-1.4) grid (1.4,1.4);

  \path [name path=upward line] (1,0) -- (1,1);
  \path [name path=sloped line] (0,0) -- (30:1.5cm);
  \draw [name intersections={of=upward line and sloped line, by=z}]
    [very thick,orange] (1,0) -- (x);

  \begin{scope}[thin] % Any changes to the clipping area are local to the scope.
    \draw (1,0) -- (1,1);
  \end{scope}

  \foreach \x in {-1,-0.5,...,1}
    \draw (\x cm,-1pt) -- (\x cm, 1pt);

  \foreach \x/\xtext in {-1, -0.5/-\frac{1}{2}, 1}
    \draw (\x cm,1pt) -- (\x cm,-1pt) node[anchor=north] {$\xtext$};

  \draw[very thick,red]
    (30:1cm) -- node[left=1pt,fill=white] {$\sin \alpha$} (30:1cm |- x axis);

  \draw (0,0) .. controls (6,1) and (9,1) ..
    node[near start,sloped,above] {near start}
    node {midway}
    node[very near end,sloped,below] {very near end} (12,0);
\end{tikzpicture}
\end{document}
```

### 3. Tutorial: A Petri-Net for Hagen

The `arrows` library for the special arrow tip used in the grapic, the
 `decoration.pathmorphing` library for the "snaking line" in the middle, the
 `backgrounds` library for the two rectangular area that are behind the two main
 parts of the picture, the `fit` library to easily compute the sizes of these
 rectangles, and the `positioning` library for placing nodes relative to other
 nodes.<br>
`petri` library

### 4. Tutorial: Euclid's Amber Version of the *Elements*

These libraries are `calc`, `intersections`, `through`, and `backgrounds`.

```
\def\A{\textcolor{input}{$A$}}
\draw (A) let
            \p1 = ($ (B) - (A) $)
            % Indeed, the digits following these macros are just normal TeX
            %  parameters. We could also use a longer name, but then we have to
            %  use curly braces.
          in
            circle ({veclen(\x1,\y1)});
\node [draw,circle through=(B),label=left:$D$] at (A) {};
\path [name intersections={of=D and E}]; % intersection-1/2
\path [name intersections={of=D and E, by={[label=above:$C$]C, [label=below:$C'$]C'}}];
\node [fill=red,inner sep=1pt,label=-45:$D$] at (D) at
  ($ (A) ! .5 ! (B) ! {sin(60)*2} ! 90:(B) $) {};
\path let \p1 = ($ (B) - (C) $) in
  coordinate [label=left:$G$] (G) at ($ (B) ! veclen(\x1,\y1) ! (F) $);
```

### 5. Tutorial: Diagrams as Simple Graphs

The jittering lines were created using the `random steps` decoration.<br>
The style definition (`\tikzset{terminal/.style=...}`) in the preamble is
 applied in all pictures.<br>
A perhaps better way of positioning the nodes is to use a *matrix*.<br>
The `graph` library can both be used to connect nodes that have already been
 created, but it can also be used to create nodes "on the fly" and these
 processes can also be mixed.

```
\begin{tikzpicture}[,>=stealth',thick,black!50,text=black,
                    every new ->/.style={shorten >=1pt},
                    graphs/every graph/.style={edges=rounded corners}]
  \graph [use existing nodes] {
    p1 -> ui1 -- p2 -> dot -- p3 -> digit -- p4 -- p5 -- p6 -> e -- p7 -- p8 -> ui2 -- p9 -> p10;
    p4 -> [skip loop=-5mm]  p3;
```

The trick is to use a slash inside the node name: In order to "render" the node,
 the text following the slash is used instead of the node name, which the text
 before the slash. Alternatively, the `as` option can be used, which also allows
 you to specify how a node should be rendered.<br>
You can specify that a graph is `simple`. This means that there can be at most
 one edge between any two nodes. If you specify an edge twice, the options of
 the second specification "win."

### 6. Tutorial: A Lecture Map for Johannes

Johannes is going to use the `mindmap` library and since he wishes to show a
 calendar, he will also need the `calendar` library. In order to put something
 on a background layer, it seems like a good idea to also include the
 `backgrounds` library.

```
\tikz [font=\footnotesize,
       grow=right, level 1/.style={sibling distance=6em},
                   level 2/.style={sibling distance=1em}, level distance=5cm]
  \node {Computional Complexity} % root
    child { node {Computational Problems}
```

The `grow` key is used to configure the direction in which a tree grows. You can
 change growth direction "in the middle of a tree" simply by changing this key
 for a single child or a whole level. By including the `trees` library you also
 get access to additional growth strategies such as a "circular" growth.<br>
Since Johannes does not want to add `\hskip0pt` inside each node, he uses the
 `execute at begin node` option to make Ti*k*Z insert this text with every
 node.<br>
He adds a `circular drop shadow`, defined in the `shadows` library, to the
 concepts, just to make things look a bit more fancy.<br>
He could rely on Ti*k*Z's automatic naming of the nodes in a tree, where the
 children of a node named `root` are named `root-1`, `root-2`, `root-3`, and so
 on.

```
\tiny
\begin{tikzpicture}
  \calendar [day list downward,
             month text=\%mt\ \%y0,
             month yshift=3.5em,
             name=cal,
             dates=2009-04-01 to 2009-05-01]
    if (weekend)
      [black!25]
    if (day of month=1) {
      \node at (0pt,1.5em) [anchor=base west] {\small\tikzmonthtext};
    };
```

### 7. Guidelines on Graphics

To create consistency between graphics and text, stick to the following
 guidelines: Do not scale graphics. Use the same font, the same line width, and
 a consistent color coding both in graphics and the body text. / The "line
 width" for normal text is the width of the stem of letters like T. For TeX,
 this is usually 0.4 pt. Creating consistency when using different graphic
 programs is almost impossible. For this reason, you should consider sticking to
 a single graphics program.<br>
Unfortunately, plots are notoriously hard to get right. Partly, the default
 settings of programs like GNUPLOT or Excel are to blame for this since these
 programs make it very convenient to create bad plots.<br>
Here are a few recommendations that may help you avoid producing chart junk:
* Do not use 3D pie charts. They are *evil*.
* Consider using a table instead of a pie chart.
* Do not apply colors randomly; use them to direct the reader's focus and to
  group things.
* Do not use background patterns, like a crosshatch or diagonal lines, instead
  of colors. They distract. Background patterns in information graphics are
  *evil*.

Good typography (like good organization) is something you do *not* notice. The
 job of typography is to make reading the text, that is, "absorbing" its
 information content, as effortless as possible.

## II. Installation and Configuration

### 8. Installation

### 9. Licenses and Copyright

