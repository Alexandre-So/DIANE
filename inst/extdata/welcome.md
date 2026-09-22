## Purpose

DIANE is a shiny application for the analysis of high throughput gene expression data (**RNA-Seq**). Its function is to extract important regulatory pathways involved in the response to environmental changes, or any perturbation inducing genomic modifications.

We designed this tool to process, explore, and perform advanced statistical analysis on **multifactorial expression data** using state of the art methods. It includes:

- Raw count data pre-processing and sample-wise normalization

- Customizable differential expression analysis

- Gene ontology enrichment analysis for model organisms

- Expression based clustering in the framework of Poisson Mixture Models, and characterization of those clusters with generalized linear models and GO enrichment analysis

- Machine learning based Gene regulatory network inference

All of the features in DIANE are accessible via a single page shiny application that can be locally launched, or used online. Several versions of the application are available:

- The official published version : https://diane.ipsim.inrae.fr/DIANE 
- An updated version, continued from the aforementioned version : https://shinyapps.southgreen.fr/app/dianelatest ; https://diane.ipsim.inrae.fr/DIANE_latest

The steps should be performed in the order of the different tabs. For instance, before running clustering or network inference, differential expression should be performed first. The figure on the left summarizes DIANE's main workflow.

For more advanced users, all server-side functions in DIANE are exported so they can be called from R scripts.

Fore more information, please find full documentation and examples in the github page <https://github.com/Alexandre-So/DIANE>.

Once the application is launched, if the resolution poorly fits your screen, you can adjust it with the keyboard shortcuts `ctrl +` or `ctrl -` (use `cmd` on Mac).

**Please report any bug or suggestion via github or at alexandre.soriano\@cirad.fr and oceane.cassan\@lirmm.fr**.

**To cite DIANE in publications use:**

Cassan, O., Lèbre, S. & Martin, A. Inferring and analyzing gene regulatory networks from multi-factorial expression data: a complete and interactive suite. BMC Genomics 22, 387 (2021). <https://doi.org/10.1186/s12864-021-07659-2>

<details>
<summary><strong>BibTeX entry for LaTeX users (click to expand)</strong></summary>

    @Article{cassan2021Inferring,
        title = {Inferring and analyzing gene regulatory networks from multi-factorial expression data: a complete and interactive suite},
        author = {Océane Cassan and Sophie Lèbre and Antoine Martin},
        journal = {BMC Genomics},
        year = {2021},
        volume = {22},
        number = {387},
        url = {https://bmcgenomics.biomedcentral.com/articles/10.1186/s12864-021-07659-2}}

</details>

