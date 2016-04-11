# [Graphing Data with R][book] by John Jay Hilfiger, O'Reilly (2016)

[book]: http://shop.oreilly.com/product/0636920038382.do

It explains exploratory graph and presentation graph with (not ggplot2 package)
 base R graphics.

## Part I. Getting Started with R

### 1. R Basics

^, **, %% (remainder), %/% (round down after division)<br>
help(sum), ?sum<br>
ls() object list in the current workspace<br>
R Commander - install.packages("Rcmdr", dependencies=TRUE), library(Rcmdr), library(help=Rcmdr)<br>
[CRAN Task Views](https://cran.r-project.org/web/views/), [all packages](https://cran.r-project.org/web/packages/available_packages_by_name.html), available.packages(), installed.packages()<br>
 # comment<br>
str() print type and values<br>
data() sample datasets<br>
attach(dataset) can omit dataset name until detach(dataset)<br>
table() frequency table<br>
q()<br>
edit(data.frame(Year=numeric(7),...)) data editor, fix() in-place edit<br>
XLConnect package (Excel input/output)<br>
save/load()<br>
?Startup - .Rprofile, .Rprofile.site<br>
cat() print to console

?airquality dataset, CrossTable() of gmodels package

### 2. An Overview of R Graphics

?device from jpeg() to dev.off()<br>
exploratory graph vs presentation graph

* base R graphics package
* grid (low-level graphics) package
* lattice package - multivariate data (trellis graphics, ...),
  ex) BP dataset of epicalc package
* ggplot2 package - consistent syntax, easy customization

mtcars dataset

## Part II. Single-Variable Graphs

### 3. Strip Charts

overwrapped on the line<br>
stripchart(Volume, method = “jitter”), stripchart(mpg~cyl)<br>
par() graphic parameter, axis(),
 text/mtext() in the plotting area or in the margins<br>
cf) stripchart() of plotrix package, dotplot.mtb()

?trees dataset, example(strip chart), vignette([x])<br>
pch (plot character) = code number of "." ?points, demo(colors)

### 4. Dot Charts

on each line<br>
dotchart(Murder, labels = row.names(USArrests))

data2 = USArrests[order(USArrests$Murder),] sort<br>
colors() reference [1][color_ref_1], [2][color_ref_2]<br>
USArrests dataset

[color_ref_1]: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
[color_ref_2]: http://research.stowers-institute.org/efg/R/Color/Chart/

### 5. Box Plots

box-and-whiskers plot (up to the highest and lowest points within 1.5 x
 interquartile range)<br>
boxplot(MathAchieve$MathAch)<br>
varwidth=TRUE (in proportion to the size of the group), horizontal = TRUE, names = c(...)<br>
cf) Engelmann-Hecker-Plot (EH-Plot) ehplot() of plotrix package

par(mfrow = c(#row,#column))<br>
par(bg = "white", fg = "white", col.axis = "gray47", mar = c(7,8,5,4), cex = .65, las = 1)<br>
abline() reference line<br>
legend()<br>
MathAchieve dataset of nlme package

### 6. Stem-and-Leaf Plots

textual histogram<br>
stem(sbp$sbp)<br>
back-to-back stem.leaf() of aplpack package

sbp dataset of multcomp package, Baumann dataset of car package

### 7. Histograms

hist(sbp)<br>
breaks = c(110,135,160,185), 30 (as a hint, ?pretty) or seq(110,190,15)<br>
label = T (show frequency number), freq = F (prob. density)<br>
stacked histograms histStack() of plotrix package<br>
multiple histograms Hist() of RCmdrMisc package<br>
trellis graphics of lattice package (when the number of groups is large)<br>
histogram(~ Salaries$salary | Salaries$rank * Salaries$sex, type = "count")<br>
back-to-back histbackback() of Hmisc package

Salaries and Burt datasets of car package, case0302$Dioxin dataset of Sleuth2 package

### 8. Kernel Density Plots

eq = density(sbp$sbp, bw = 5)<br>
plot(eq), lines(eq) superimpose<br>
bandwidth estimation - ASH, KernSmooth, np package<br>
cdf
* ecdf(sam)
* Ecdf() of Hmisc package, grid()
* stat_ecdf() of ggplot2 package

par() print current parameters<br>
rnorm(100000)<br>
ploygon() color area under curve

### 9. Bar Plots (Bar Charts)

barplot(table(rank))

### 10. Pie Charts

### 11. Rug Plots

## Part III. Two-Variable Graphs

### 12. Scatter Plots and Line Charts

### 13. High-Density Plots

### 14. The Bland-Altman Plot

### 15. QQ Plots

## Part IV. Multivariable Graphs

### 16. Scatter plot Matrices and Corrgrams

### 17. Three-Dimensional Plots

### 18. Coplots (Conditioning Plots)

### 19. Clustering: Dendrograms and Heat Maps

### 20. Mosaic Plots

## Part V. What Now?

### 21. Resources for Extending Your Knowledge of Things Graphical and R Fluency

### A. References

### B. R Colors

### C. The R Commander Graphical User Interface

### D. Packages Used/Referenced

### E. Importing Data from Outside of R

### F. Solutions to Chapter Exercises

### G. Troubleshooting: Why Doesn't My Code Work?

### H. R Functions Introduced in This Book

