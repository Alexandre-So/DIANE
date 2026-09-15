
# Reactive values in DIANE shared accross modules
---

Variable reactive globale **r** :
```
r
| raw_counts (dataframe : (genes * samples))
| normalized_counts (dataframe: (genes * samples))
| normalized_counts_pre_filter (dataframe : (genes * samples))
| tcc (TCC class object)
| conditions (vector of the samples condition names)
| design (dataframe : (conditionNames * factors))
| regulators
| gene_info dataframe 
| splicing_aware (boolean)
| gene_lists
|   | list name (vector of genes)
| gene_lists_infos
|   | list name
|   |    | origin ("DEA" or "custom")
|   |    | Conditions, lfc, fdr (only when origin is "DEA")
| top_tags
|   | list name (dataframe : (genes * (logFC, logCPM, FDR))
| fit (dispersion and glmFit for dea)
| clusterings
|   | joined list names
|   |    | model (coseqResult)
|   |    | membership (named vector)
|   |    | conditions (vector)
| networks
|   | joined list names
|   |    | nodes
|   |    | edges
|   |    | membership clustering louvain(named vector)
|   |    | conditions
|   |    | graph
``` 

Juste pour savoir ce qui est accessible à tout moment dans l'appli.

Peut être aussi pour être stockée dans une session et reloadé direct.


Organism specific :

+ GO analysis, change id to entrez

+ gene info

+ TF list for network inference