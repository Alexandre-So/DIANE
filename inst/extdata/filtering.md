# Removing low count genes

Removing genes with very low abundance is a common practice in RNA-Seq analysis pipelines for several reasons :

-   They have little biological significance, and could be caused either by noise or mapping errors.

-   The statistical modelling we are planning to perform next is not well suited for low counts, as they make the mean-variance relationship harder to estimate.

There is no absolute and commonly accepted threshold value, but it is recommended to allow only genes with more than 10 counts per sample in average. DIANE thus proposes a threshold at 10\*sampleNumber, but feel free to experiment with other values depending on your dataset.

DIANE offers two ways to filter these gene.

-   The first and historical method is to look at the sum of all the count of each row and to compare it to a threshold.

-   The second is to take the median of all biological replicates of each conditions, and to keep genes that have a value above the threshold in at least one condition.
