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
 classes and packages by the prefix `scr`. This prefix standas for **script**,
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

## Chapter 3: Adjusting Fonts

## Chapter 4: Working with Images

## Chapter 5: Beautiful Designs

## Chapter 6: Designing Tables

## Chapter 7: Contents, Indexes, and Bibliographies

## Chapter 8: Getting the Most out of the PDF

## Chapter 9: Creating Graphics

## Chapter 10: Advanced Mathematics

## Chapter 11: Science and Technology

## Chapter 12: Getting Support on the Internet

