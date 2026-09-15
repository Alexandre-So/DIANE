# Custom gene lists

------------------------------------------------------------------------

Here you can bring your own list of genes into DIANE, instead of relying only on the gene lists produced by a differential expression analysis. A gene family, the genes of a QTL interval, or a list taken from a publication can all be imported, and are then usable for clustering, network inference, and Venn comparisons, exactly like a differentially expressed gene list.

## Providing the genes

Gene identifiers can be pasted in the text area, uploaded as a file, or both, in which case the two are merged. They can be separated by line breaks, spaces, commas or semicolons, and duplicates are removed.

If you upload a spreadsheet export with several columns, the extra columns and the header line are read as gene identifiers too. They are not silently discarded : they show up in the count of genes that were not found, so that you can check what was actually understood.

## Which genes are kept

Only the genes present in your normalized expression matrix are kept, which is why this step requires the normalisation to be done first. A gene that was removed by the low count filter cannot be clustered nor used for network inference, so keeping it would only produce errors later on.

The three counters above show how many genes your dataset holds, how many you submitted, and how many were found in both. When some genes are missing, a sample of them is displayed so that you can spot an identifier format problem.

## Naming

A gene list name has to be unique across all your gene lists, imported ones and differential expression ones alike, because that name is what DIANE uses to store the clustering and network results computed from it. It cannot contain `" + "`, which is reserved to name the combination of several gene lists.

A list holds between 2 and 5000 genes.
