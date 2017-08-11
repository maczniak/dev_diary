# [Fundamentals of Systems Biology: From Synthetic Circuits to Whole-cell Models][homepage] by Markus W. Covert, CRC Press (2015)

[WholeCell.org][wholecell_org]

[homepage]: https://www.crcpress.com/Fundamentals-of-Systems-Biology-From-Synthetic-Circuits-to-Whole-cell-Models/Covert/p/book/9781420084108
[wholecell_org]: http://www.wholecell.org

## Section I Building Intuition

### Chapter 1. Variations on a Theme of Control

Protein factors bind DNA and modulate the transcription of target genes. Many
 transcription factors regulate their own expression. This phenomenon is called
 autoregulation.<br>
Binding of FNR (fumarate and nitrate reduction) to the promoter physically
 blocks access to its own gene, preventing *fnr* from being transcribed. Whether
 FNR can bind to DNA depends on the presence or absence of molecular oxygen.
 When there is no oxygen in the environment, FNR is active, and it binds DNA.
 When oxygen is present, it binds the FNR protein, which undergoes a structural
 change that prevents it from binding DNA. In autoregulation, they strongly
 favor negative regulation.<br>
Several interesting data-mining strategies and databases have also been
 developed to help you learn what is already known about any given gene;
 [EcoCyc][ecocyc] is just such a datbase.

In systems biology, we commonly measure the abundances of RNAs and proteins.
 First, let us consider how we could measure RNA abundance. If you only wanted
 to measure the abundances of a few different RNAs, you could probably use a
 technique such as northern blotting or fluorescence in situ hybridization,
 both of which "count" RNA molecules by binding them to other molecules
 carrying a detectable label. Measuring a fairly large set of RNAs could be
 achieved with quantitative polymerase chain reaction (qPCR), a highly sensitive
 but gene-specific technique. Finally, if you wanted to count many thousands of
 RNAs--or all of the RNAs encoded by an organism--you could take advantage of
 gene microarray analysis or sequencing the RNA molecules themselves.<br>
Protein abundance is also a critical component of cellular behavior. To measure
 the abundance of a small number of proteins, you could use western blotting, a
 technique involving antibody detection that is conceptually similar to
 northern blotting. Mass spectrometry would provide measurements of a larger
 set of proteins and is especially useful for cases in which you do not have an
 antibody against your proteins. Two-dimensional protein gel electrophoresis can
 provide simultaneous relative measurements of a large variety of proteins and
 may even capture an organism's entire protein repertoire.<br>
We also have tools to investigate the special case of DNA-protein interactions.
 The most popular way to detect these interactions is first to cross-link the
 proteins to the DNA with formaldehyde, then gather either a particular set of
 DNA-protein interactions with an antibody (chromatin immunoprecipitation, or
 ChIP) or the entire repertoire of DNA-protein interactions from an organism.
 If you expect to find only a small number of binding sites, you would use qPCR
 of the DNA bound to the protein, but if you wanted to query many binding sites
 or the entire genome, you could hybridize the pool of DNA that was bound to
 your protein to a microarray (ChIP-chip) or sequence it (ChIP-seq).

[ecocyc]: https://ecocyc.org

### Chapter 2. Variation: Boolean Representations

### Chapter 3. Variation: Analytical Solutions of Ordinary Differential Equations

### Chapter 4. Variation: Graphical Analysis

### Chapter 5. Variation: Numerical Integration

### Chapter 6. Variation: Stochastic Simulation

## Section II From Circuits to Networks

### Chapter 7. Transcriptional Regulation

### Chapter 8. Signal Transduction

### Chapter 9. Metabolism

### Chapter 10. Integrated Models

