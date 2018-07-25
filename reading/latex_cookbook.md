# [LaTeX Cookbook][homepage] by Stefan Kottwitz, Packt Publishing (2015)

[LaTeX.org][latex_org], [TeXample.net][texample_net],
 [TeXdoc Online][texdoc_online], [TeXblog][texblog] (personal blog)

[homepage]: http://latex-cookbook.net/
[latex_org]: https://latex.org/forum/
[texample_net]: http://texample.net/
[texdoc_online]: http://texdoc.net/
[texblog]: http://texblog.net/

## Chapter 1: The Variety of Document Types

[TeXworks][texworks] (editor), [Overleaf][overleaf] and
 [ShareLaTeX][sharelatex]<br>
We will use a class of the KOMA-Script ([manual][koma_script_manual]) bundle.
 This is a set of classes that were originally designed with the aim of
 replacing the standard classes and providing more features. KOMA-Script has
 been extensively developed during recent years. You can identify KOMA-Script
 classes and packages by the prefix `scr`. This prefix stands for **script**,
 which was the initial name of this bundle.<br>
In contrast with `\section*`, the `\addsec` command generates an entry in the
 table of contents.<br>
A very good source for checking the quality of a template is the guide to
 obsolete commands and packages in **LaTeX2e**, which is also called
 [**l2tabu**][l2tabu].<br>
The `ClassicThesis` ([manual][classicthesis_manual]) package by Prof. André
 Miede is a thesis template of very good quality. The design follows the classic
 guide to writing, *The Elements of Typographic Style* by Robert Bringhurst.
 (don't use bold fonts, text body is not very wide, page number follows the
 title)<br>
You can use the `\includeonly` command to compile just the selected chapters for
 speeding up writing.<br>
`\xspace` inserts a space when there's no punctunation mark following it.<br>
[`inputenv`][inputenv], [`caption`][caption],
 [LaTeX Templates][latex_templates]<br>
The headings in sans serif font is an intentional default setting in KOMA-Script
 classes, which makes the headings lighter than the standard LaTeX big, bold,
 and serif headings.<br>
We chose the high-quality T1 supporting font set Latin Moderns by loading the
 `lmodern` package. We used the `microtype` package to get finder typography
 (better text justification).<br>
The `\par` command is equivalent to an empty line in the input. Sometimes,
 people use it to keep the code compact. We need to end the paragraph before the
 font size changes because that size defines the space between lines.<br>
A very well designed book class is `memoir` (`memman` and `memdesign` manuals).
 Another great start with a special beauty is the `tufte-latex` class.

```latex
\documentclass{beamer}
\usetheme{Warsaw} % AnnArbor, Antibes, Bergen, Berkeley, Berlin, Boadilla, ...
\AtBeginSection{
  \begin{frame}{Outline}
    \tableofcontents[currentsection]
  \end{frame}}
%\setbeamertemplate{navigation symbols}{}
%\usefonttheme{serif} % professionalfonts, structure/bold/italicserif/smallcapsserif
\usecolortheme{dolphin} % (outer) whale, (inner) lily, (overall) albatross, ...
\begin{document}
\title{Talk on the Subject}
\subtitle{What this is about}
\author{Author Name}
\institute{University of X}
\date{June 24, 2015}
\begin{frame}
  \titlepage
\end{frame}
\begin{frame}{Outline}
  \tableofcontents[pausesections]
\end{frame}

\section{Introduction}
\subsection{A subsection}
\begin{frame}{Very Informative Title}
  \begin{itemize}
    \item First thing to say. \alert{main point}
    \item There is more.\pause
    \item Another short point.
  \end{itemize}
  \begin{alertblock}{A highlighted block} % exampleblock, block
    Some important information put into a block.
  \end{alertblock}
  % overlay-specification-aware commands (\item, \includegraphics, \renewcommand)
  \uncover<3,5->{Surprise!}
  \begin{columns}[t]
    \begin{column}{0.45\textwidth}
      Sample text in\\
      two lines
    \end{column}
    \begin{column}[T]{0.45\textwidth}
      \includegraphics[width=3cm]{filename}
    \end{column}
  \end{columns}
\end{frame}
```

`moderncv`, `leaflet`

[texworks]: http://www.tug.org/texworks/
[overleaf]: https://www.overleaf.com/
[sharelatex]: https://www.sharelatex.com/
[koma_script_manual]: http://texdoc.net/texmf-dist/doc/latex/koma-script/scrguien.pdf
[l2tabu]: http://texdoc.net/texmf-dist/doc/latex/l2tabu-english/l2tabuen.pdf
[classicthesis_manual]: http://texdoc.net/texmf-dist/doc/latex/classicthesis/ClassicThesis.pdf
[inputenv]: http://texdoc.net/texmf-dist/doc/latex/base/inputenc.pdf
[caption]: http://texdoc.net/texmf-dist/doc/latex/caption/caption-eng.pdf
[latex_templates]: http://www.latextemplates.com/

## Chapter 2: Tuning the Text

The default encoding is called OT1, and ocvers 128 glyphs. T1 provides 256
 glyphs, so many accented characters are included. If you would like to work
 with other languages, take a look at the `fontenc` manual.<br>
`polyglossia` (alternative to babel for XeLaTeX and LuaLaTeX)<br>
We will use the `tcolorbox` package. It is based on `pgf`.<br>
The `showframe` package belongs to the `eso-pic` bundle, which will help us
 absolute positioning of text.<br>
`lua-visual-debug` package<br>
For two-sided prints with very thin paper, matching base lines would look much
 better. Especially in two-column documents it may be desirable to have
 baselines of adjacent lines at exactly the same height. Normal text lines will
 be placed at a baseline grid. Displayed formulas, figures, tables, and captions
 are allowed to have a different baseline, but the following text should return
 to the grid. Add the `grid` package to the preamble with font size options, as
 follows: `\usepackage[fontsize=10pt,baseline=12pt]{grid}`. Then, put the
 equation into a `gridenv` environment. So-called glue, also known as rubber
 space, which is elastic space, is removed or replaced by a fixed space. The
 heights of many frequently-used items were made integral multiples of the
 baseline height so that they would fit exactly to the grid.<br>
The `picture` package is a helper package. Some commands, such as `\put`, expect
 arguments with simple numbers, which are interpreted as factors of
 `\unitlength`.<br>
The `atbegshi` package is a modern reimplementation of the `everyshi` package.
 The `eso-pic` pacakge actually builds on `atbegshi`. The `textpos` package
 based on the `everyshi` package. It provides a convenient user interface. The
 package `TikZ` can be used to place text in relation to the `current page` node
 in `overlay` mode. The `PSTricks` bundle contains the package `pst-abspos` for
 putting an object at an arbitrary position on the page.<br>
In older texts, such as in books of fairy tales, we sometimes see the first
 paragraph in a text starting with a huge letter, while the following text flows
 around it. This is called a drop cap or an initial. We will use the `lettrine`
 package. The package `coloredletrrine` provides an even fancier way. It
 provides bicolor initials based on the EB Garamond font. At the time of
 writing, the package was still in development.<br>
The `shapepar` package can typeset paragraphs in a specific shape. Furthermore,
 we can let text flow around a shape.<br>
In a two-column layout, it's looks nice to put the quotation into a window in
 the middle of the page between the two columns, with the regular text flowing
 around it. The `pullquote` package provides a solution. It also explains image
 inclusion support with the `shape=image` list. For this, the free ImageMagick
 program would need to be installed on the system.

## Chapter 3: Adjusting Fonts

[The LaTeX Font Catalogue][latex_font_catalogue]<br>
`tlmgr install collection-fontsrecommended|collection-fontsextra`,
 [`getnonfreefonts`][getnonfreefonts]<br>
1) `\usepackage{lmodern}` 2) `\usepackage{kpfonts}` 3) `\usepackage{libertine}`,
 `\usepackage[libertine,cmintegrals,cmbraces,vvarbb]{newtxmath}`,
 `\usepackage[scaled=0.95]{inconsolata}` 4) `\usepackage[sc,osf]{mathpazo}`,
 `\usepackage[T1,small,euler-digits]{eulervm}`,
 `\usepackage[scaled=0.86]{berasans}`, `\usepackage[scaled=0.84]{beramono}`<br>
`fontname` - Avant Garde `pag`, Bookman `pbk`, Charter `bch`, Computer Concrete
 `ccr`, Computer Modern Roman `cmr`, Computer Modern Sans Serif `cmss`, Computer
 Modern Typewriter `cmtt`, Courier `pcr`, Garamond `mdugm`, Helvetica `phv`,
 Inconsolata `fi4`, Latin Modern `lmr`, Latin Modern Sans Serif `lmss`, Latin
 Modern Typewriter `lmtt`, New Century Schoolbook `pnc`, Palatino `ppl`, Times
 `ptm`, Utopia `put`, Zapf Chancery `pzc`<br>
`\newcommand{\zapf}{\fontfamily{pzc}\selectfont}`<br>
The font properties at the end of the paragraph determine how TeX formats a
 paragraph, especially its line spacing. That's the reason for having the
 paragraph break within the group or environment, not directly afterwards.<br>
`fntguide` (the Guide to LaTeX 2e font selection)<br>
`kpsewhich filename`<br>
We will see how to choose one or more specific symbols from a package and access
 them without loading the whole package. By simply copying and pasting, we
 copied the behavior of the symbol package.<br>
But `boldmath` and `unboldmath` are for making all symbols of a formula bold.
 Such kind of emphasizing is rather rare today, as it destroys the uniform
 grayness of the text from a typographer's point of view. A more common
 requirement is to get bold versions of certain symbols. For example, bold
 symbols are often used for vectors and number systems.
 `\bmdefine{\balpha}{\alpha}` The `bm` package does the following for us:
 1) determining available bold math fonts and using them if available, 2)
 falling back to poor man's bold if no bold version can be found, which means
 overprinting with slight offsets, 3) keeping the correct spacing of the symbol,
 4) respecting the meaning of symbols, such as delimiters.<br>
It can be a requirement by the university or institute. It may even be a design
 decision, for example, presentation slides often use sans serif. It's the
 default behavior of the LaTeX `beamer` class. `sfmath` package. There's an
 issue with math accents. For example, with the default Computer Modern font,
 math accents such as `\tilde`, `\dot`, and `\hat` may be slightly misplaced,
 since the Computer Modern sans serif font doesn't provide the positioning
 information. The `sansmathaccent` package comes to the rescue. (alternative
 approach) `sansmath` package or sans serif fonts with direct math support such
 as Arev Sans and Kepler fonts.<br>
`dsfont` package to get double stroke letters<br>
`symbols` (Comprehensive LaTeX Symbol List)<br>
To enable the searching and copying of ligatures, `input{glyphtounicode}` and
 `\pdfgentounicode=1`. The `mmap` package is an extension of `cmap`, which also
 works with math symbols.<br>
There may be a reason to disable ligatures, for example, in verbatim text such
 as the source code. `microtype` package and `\DisableLigatures`<br>
For the `contour` package, a vector font is required, which is recommended in
 any case. This should be a so called Type 1 font. With pdfLaTeX, TrueType fonts
 are supported too. If your engine supports it, you can even let the contour
 generate a real outline of the text, instead of using copies, which should
 result in a better quality. With [XeLaTeX][xelatex] and [LuaLaTeX][lualatex],
 you can use even system fonts such as TrueType fonts and feature rich OpenType
 fonts. That's actually LaTeX basing on the new engines called XeTeX and LuaTeX.
 Those names are more popular.

[latex_font_catalogue]: http://www.tug.dk/FontCatalogue/
[getnonfreefonts]: http://www.tug.org/fonts/getnonfreefonts/
[xelatex]: http://www.tug.org/xetex/
[lualatex]: http://www.luatex.org/

## Chapter 4: Working with Images

Today, pdfLaTeX is most widely used. It allows the inclusion of PDF images.
 Furthermore, the bitmap formats, JPG/JPEG, and PNG can be used. Classic LaTeX,
 which generates Device Independent file format (DVI) files, only supports
 Encapsulated PostScript (EPS). This is a PostScript format with some
 restrictions; for example, it cannot span several pages.<br>
The `interpolate` option activates the interpolation for bitmaps, which is
 supported in PDF.<br>
You can now use the command `\FloatBarrier` to block floating beyond this point.
 The command `\clearpage` ends the page and forces the output of all floats that
 are not yet placed.<br>
[Using Imported Graphics in LaTeX and pdfLaTeX][epslatex]<br>
(color frame) `\usepackage[export]{adjustbox}`,
 `\includegraphics[width=10cm,cframe=red!50!black 5mm]{filename}`

```latex
\newsavebox{\picbox}
\newcommand{\cutpic}[3]{
  \savebox{\picbox}{\includegraphics[width=#2]{#3}}
  \tikz\node [draw, rounded corners=#1, line width=4pt,
    color=white, minimum width=\wd\picbox,
    minimum height=\ht\picbox, path picture={
      \node at (path picture bounding box.center) {
        \usebox{\picbox}};
    }] {};}

\tikzset{annotations/.style = {
  every path/.append style = { ...

\begin{tikzonimage}[width=.8\textwidth]{filename}
  [annotations]
  ...
```

`onimage` draws over an image. We activated the `tsx/show help lines` option,
 which draws a grid over the image. For the final document, we comment out or
 delete this option to remove the grid.<br>
`\raisebox{-0.5\height}` (moves the base line)<br>
The `adjustbox` package provides a lot of user commands for aligning boxes with
 text or images.<br>
(`stackengine`) `\stackinset{c}{}{c}{}{$\star$}{O}`,
 `\stackinset{l}{1em}{t}{1em}{Inside annotation}{\includegraphics...`

[epslatex]: http://mirror.navercorp.com/CTAN/info/epslatex/english/epslatex.pdf

## Chapter 5: Beautiful Designs

The `background` package is based on TikZ and the `everypage` package. It can
 require several compiling runs before the positioning is finally correct. This
 is because TikZ writes the position marks into the `.aux` file, which gets read
 in and processed in the next LaTeX run. Instead of images, you cloud display
 dynamic values such as the page number or the head mark with chapter title,
 instead of using a package such as `fancyhdr`, `scrpage2`, or
 `scrlayer-scrpage`. It also shows how to select only certain pages to have
 this background. There are more packages that can perform similar tasks; for
 example, `watermark`, `xwatermark`, and the packages `everypage` and `eso-pic`,
 which don't require TikZ.<br>
There are fonts which provide typographical ornaments as glyphs; `fourier-orns`,
 `adforn`, and `webomints` are very good examples.<br>
`menukeys` package - `\menu`, `\keys`, `\directory`

## Chapter 6: Designing Tables

The `tabular*` environment achieves a desired table width by adjusting the
 spacing between columns. The `tabularx` environment also spans to a given width
 by automatically calculating column widths, trying to distribute available
 space evenly. The `tabulary` environment also balances column widths, but tries
 to give more space to columns with more content.<br>
Even with the `array` environment, it's quite the same; it would just be in math
 mode.<br>
We will use the `booktabs` package, which has been written with good design in
 mind. Sepcifically, it enhances lines in tables. The `\cmidrule` command is
 used for a line spanning one, two, or more columns.<br>
The `bbding` package provides a check mark symbol `\Checkmark`.<br>
Consequently, don't use double lines.<br>
Generally, whitespace gives invisible support to the structure; that's why we
 increased the default `\arraystretch` value from 1 to 1.6,
 `\renewcommand{\arraystretch}{1.6}`.<br>
(table notes instead of footnotes) Footnotes in `minipage` environments already
 work this way, so you could wrap your table in a `minipage` environment as a
 first possibility. We will use the `threeparttable` package, which can generate
 footnotes below tables with the same width as the table body.<br>
The `siunitx` package is, first of all, intended for typesetting values with
 units consistently. Incidentally, supporting its primary purpose, it provides a
 `tabular` column type `S` for aligning at decimal points.<br>
`\usepacakge[table]{xcolor}`, `\rowcolors{2}{gray!15}{white}`,
 `\newcommand{\head}[1]{\textcolor{white}{\textbf{#1}}}`. The `xcolor` package
 implicitly loads the `colortbl` package, which is the standard package for
 coloring tables. (`\columncolor`, `\rowcolor`, `\cellcolor`)<br>
We will use the `slashbox` package. It is part of MikTeX, but not of TeX
 Live.<br>
(vertical spacing) `\smallskip`, `\medskip`, `\bigskip`<br>
`\newcounter{rank}`, `\setcounter{rank}{0}`,
 `\newcommand{\steprank}{\stepcounter{rank}\makebox[2em]{\therank}\hspace{\tabcolsep}}`<br>
We will load the `datatool` package, let it import data from a comma separated
 data file, sort it, and print it.

## Chapter 7: Contents, Indexes, and Bibliographies

To benefit from the automatic adjustments of `tocstyle`, you need several
 compiler runs. Expect at least three runs. In the first run, LaTeX writes all
 entries from commands, such as `\chapter` and `\section`, to the external table
 of contents file. The name of this file ends with `.toc`. For the list of
 figures and tables, the filenames end with `.lof` and `.lot`, respectively. In
 the following run, LaTeX can read the entries of the external file for printing
 the ToC. Widths are calculated and written to the `.aux` file. In the next run,
 the ToC is produced using the known entries from the `.toc` file and the known
 widths from the `.aux` file. Of course, changes in sectioning and page numbers
 would require a further run.<br>
If you load the `tocstyle` package without options, it produces a graduated ToC,
 just like the standard classes. However, it tries hard to avoid page breaks
 between ToC entries and their parents, and it automatically adjusts the
 widths.

The `biblatex` package entirely uses TeX macros for formatting. BibTeX styles,
 by contrast, are programmed in a postfix stack language.<br>
It is highly recommended that you use the `biber` backend with the `biblatex`
 package for the following reasons: UTF-8 input, custom sorting, flexible names,
 multiple bibliographies and no memory limitations.<br>
`natbib` is a popular package for author-year citations with BibTeX.<br>
The `\addbibresource` command replaces the older BibTeX command `\bibliography`.
 The latter takes just the file name and is not as flexible.<br>
[`biblatex` styles][biblatex_styles]

There's a package with a similar name called `glossary`. This is an older
 version by the same author, which is now obsolete; so, `glossaries` should be
 used instead.

insert the `\index` commands right before key words.<br>
The `latexmk` tool runs often enough to resolve cross references.

[biblatex_styles]: https://ctan.org/topic/biblatex

## Chapter 8: Getting the Most out of the PDF

If a document class already loads the `hyperref` package, like the `beamer`
 package does, or another package, you can still use the `\hypersetup`
 command.<br>
Colored links are actually printed, in contrast to the default borders, so you
 may still decide to use the `hidelinks` package for printing.

```latex
\hypersetup{pdfauthor   = The Author, % or pdfinfo option
            pdftitle    = The Book,
            pdfsubject  = Draft version,
            pdfkeywords = {book, draft},
            pdfproducer = TeX version,
            pdfcreator  = LaTeX editor}

\usepackage{hyperxmp}
\hypersetup{
  pdfcopyright = {Copyright (C) 2012 by author name.
    All rights reserved.},
  pdflicenseurl = {http://latex-community.org/license/}}
```

We should group by braces to be on the safe side with option parsing. The space
 with a preceding backslash is a forced space, otherwise the space would get
 lost.<br>
Adobe Systems, Inc. supports eXtensible Metadata Platform (XMP) for embedding
 metadata. The `xmpincl` package is more flexible than the `hyperxmp` package,
 but it doesn't have such a handy interface. You need to create a separate XML
 file.<br>
[JavaScript for Acrobat][javascript_for_acrobat]<br>
[Ebooks and paper sizes: Output routines made easier][ebooks_and_paper_sizes],
 [screenread package][screenread]<br>
`pdfcrop` (depends on Ghostscript and Perl)<br>
If the class doesn't matter, because you are just generating a graphic, you can
 use the `standalone` class. The older `preview` package does a similar job. It
 can extract environments of a LaTeX document as separate graphic files.<br>
`pdfpages` package combines PDF files.<br>
`animate` package,
 `\multiframe{number of frames}{variable = initial value+increment}{drawing code with \variable}`,
 `\animategraphics[options]{frame rate}{file basename}{first}{last}`<br>
The `\shadedraw` command creates the actual drawing while filling it with
 shading. Here, we used a shiny shading called `color wheel`.

[javascript_for_acrobat]: https://www.adobe.com/devnet/acrobat/javascript.html
[ebooks_and_paper_sizes]: https://www.tug.org/TUGboat/tb32-3/tb102veytsman-ebooks.pdf
[screenread]: http://personal.psu.edu/jcc8/screenread/

## Chapter 9: Creating Graphics

`smartdiagram` package<br>
`\smartdiagramanimated` instead of `\smartdiagram`

```latex
\usepackage{tikz}
\usetikzlibrary{matrix,calc,shapes}
\tikzset{
  treenode/.style = {shape=rectangle, rounded corners,
                     draw, anchor=center,
                     text width=5em, align=center,
                     top color=white, bottom color=blue!20,
                     inner sep=1ex},
  decision/.style = {treenode, diamond, inner sep=0pt}
}
\newcommand{\yes}{edge node [above] {yes}}
\begin{tikzpicture}[-latex] % arrow style
  \matrix (chart)
    [
      matrix of nodes,
      column sep      = 3em,
      row sep         = 5ex,
      column 1/.style = {nodes={decision}},
      column 2/.style = {nodes={env}}
    ]
    {
      |[root]| Formula &          \\
       single-line?    & equation \\
    };
  \draw
    (chart-1-1) edge (chart-2-1)
    \foreach \x/\y in {2/3, 3/4, 4/5, 5/6} {
      (chart-\x-1) \no (chart-\y-1) };
  \draw
    (chart-6-1) -- +(-2,0) |- (chart-1-1)
      node[near start,sloped,above] {no, reconsider};
```

If an edge needs to be labeled, add a node using the `edge from parent` option
 after the node.<br>
`pgfplots`, `pgf-pie`, `pie-chart` package<br>
Use a `scope` environment to apply a style to a part of the drawing.<br>
`(90:1.2)` (in polar coordinates)<br>
A classical approach is to use `opacity`. Another pleasing way for getting
 transparent graphics is by using the blend mode feature of the PDF standard.

## Chapter 10: Advanced Mathematics

There should not be empty lines in the code before or after displayed math, as
 it would add paragraph breaks. The also existing TeX syntax `$$ ... $$~ for
 displayed math should not be used as the vertical spacing would not be consistent with LaTeX.<br>
[detexify][detexify]<br>
You can achieve subscripts or superscrupts for your new operator:
 `\DeclareMathOperator*{\diff}{\diff}`<br>
On the Internet and in old documents, you still can see the `eqnarray`
 environment for such a purpose (`align`, `gather`). Don't use it as the spacing
 around the relation sign would be wrong.<br>
The package `mathtools` shows equation numbers and tags only if there's a
 reference by the `\eqref` or `\refeq` commands. When you just load the
 `mathtools` package, it implicitly loads the `amsmath` package as well. It
 fixes known `amsmath` errors. The `amsmath` package is very static, which is
 one reason why the `mathtools` package has been created. `\adjustlimits`, 
 \smashoperator`, `\cramped`<br>
The `breqn` package is designed for automatic line-breaking in equations. The
 package authors recommended to load `breqn` after all other math related
 packages, such as `amsmath`, `amssymb`, `mathpazo`, or `mathptmx`. So, for
 example, `breqn` detects and uses options provided to amsmath, such as `leqn`
 and `fleqn`. A good rule of thumb is: load sophisticated or late developed
 packages late.<br>
As the `beamer` class uses sans-serif math font by default, we switch to the
 serif math font as in standard documents: `\usefonttheme[onlymath]{serif}`<br>
load the `fit` library, which we will use for auto-fitting to nodes.

```latex
\tikzset{
  highlighted/.style = { draw, thick, rectangle,
                         rounded corners, inner sep = 0pt,
                         fill = red!15, fill opacity = 0.5
                       }
}

\draw[->, thick, red, dashed] (N) -- (NT)
 node [pos=0.68, above] {Transpose};
```

The `tikzmark` package has basically the same approach and can be used in a
 similar way. the `empheq` package is another option for emphasizing equations
 or parts of them.<br>
When you switch the style by using the `\theoremstyle` command, it becomes valid
 for the following `\newtheorem` definitions. The `proof` environment is a
 special predefined environment, which starts with `Proof` and automatically
 gets an endmark, the *quod erat demonstrandum* symbol. The `amsthm` package
 provides a command, `\newtheoremstyle`, which takes nine arguments, so you can
 customize fonts and spacing of the environment, also specifically for head and
 body. `thmtools` package (depends on `shadethm` and `thmbox` packages)
* The default style is `plain`. The label is printed in bold, the optional name
  in parenthesis in normal upright font, and the body text in italics.
* The `definition` style generates a bold label and normal upright body text.
* The `note` style produces an italic label and normal upright body text.

In algebra, especially in category theory, we use so-called commutative
 diagrams.

```latex
% pgfplots package
\begin{axis} [axis lines=cneter]
  \add plot [domain=-3:3, thick, smooth] { x^3 - 5*x };
\end{axis}
```

An overall view into a function can be improved with reduced axes, maybe even
 shifted away a bit. There's a [style][shift_library] which you can use. The
 same library can be used in three dimensions.<br>
 [PGFPlots.net][pgfplots_net]<br>
The `tkz-euclide` package (no English manual yet) uses cm as a length unit.<br>
several options to actually calculate something:
* The `calc` package offers basic math with LaTeX, with lengths and counters
* The `fp` package provides fixed point arithmetic with high precision
* The `pgfmath` package belongs to the PGF/TikZ package and provides many
  functions and a good parser
* LuaLaTeX is a version of LaTeX which allows programming calculation in the
  programming language Lua

If you would like to do high-precision calculations, I would switch to LuaLaTeX
 or to the `fp` package. The reason is that the precision of the `pgfmath`
 package is limited by TeX's internal capabilities.<br>
extensive reference manuals for mathematics with LaTeX: `amsmath` and
 `mathmode`

[detexify]: http://detexify.kirelabs.org/classify.html
[shift_library]: http://pgfplots.net/media/tikzlibrarypgfplots.shift.code.tex
[pgfplots_net]: http://pgfplots.net/

## Chapter 11: Science and Technology

`algorithmicx` package with `algpascal` and `algc` layouts<br>
`inconsolata` package for an excellent typewriter font<br>
`tkz-graph` for graph theory (no English manual yet)<br>
`\usetikzlibrary{positioning}` for placing nodes relative to other nodes

## Chapter 12: Getting Support on the Internet

[LaTeX][latex], [The TeX FAQ][tex_faq], [MacTeX][mactex],
 [The Visual LaTeX FAQ][visual_latex_faq]<br>
In the case of an included file, you can insert the command `\endinput`.<br>
Replace images with a rectangle using the `\rule{...}` command or give the
 option `demo` to the `graphicx` package as follows:
 `\usepackage[demo]{graphicx}`<br>
Replace a bibliography file with the `filecontents*` environment of the
 `filecontents` package.<br>
The `mwe` (minimal working examples) package is very handy for creating reduced
 examples. It automatically loads the `blindtext` and `graphicx` packages and
 provides several dummy images; this makes a minimal example elegant.

[latex]: https://www.latex-project.org/
[tex_faq]: https://texfaq.org/
[mactex]: http://tug.org/mactex/
[visual_latex_faq]: http://texdoc.net/texmf-dist/doc/latex/visualfaq/visualFAQ.pdf

