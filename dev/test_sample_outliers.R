# Non-regression suite for sample_correlation() and detect_sample_outliers().
# Standalone, no testthat :  Rscript dev/test_sample_outliers.R
# Exit status 0 if everything passes.

suppressMessages(pkgload::load_all(".", quiet = TRUE))

nb_ok <- 0L; nb_ko <- 0L; failures <- character(0)
chk <- function(label, cond) {
  cond <- isTRUE(tryCatch(cond, error = function(e) FALSE))
  cat(if (cond) "  PASS  " else "  FAIL  ", label, "\n")
  if (cond) nb_ok <<- nb_ok + 1L else { nb_ko <<- nb_ko + 1L; failures <<- c(failures, label) }
}
hdr <- function(s) cat("\n---", s, "---\n")

# erreur levee, avec un message qui nomme ce qui ne va pas plutot que de laisser
# remonter celui de stats::cor (dependant de la locale)
errs_clearly <- function(expr) {
  out <- tryCatch({ force(expr); NULL }, error = conditionMessage)
  !is.null(out) && grepl("sample|gene|column|row|count|condition|numeric|must be",
                         out, ignore.case = TRUE)
}
warns <- function(expr) {
  got <- FALSE
  withCallingHandlers(tryCatch(force(expr), error = function(e) NULL),
                      warning = function(w) { got <<- TRUE; invokeRestart("muffleWarning") })
  got
}
flagged_of <- function(r) sort(r$sample[r$flagged %in% TRUE])

# ---------------------------------------------------------------- fixtures --
# Comptes dont log2(counts + 1) a EXACTEMENT la correlation demandee : les
# attentes ci-dessous portent sur des nombres choisis, pas sur du bruit.
exact_cor_counts <- function(R, n_genes = 3000, seed = 1) {
  set.seed(seed)
  Z <- scale(matrix(stats::rnorm(n_genes * ncol(R)), n_genes, ncol(R)),
             center = TRUE, scale = FALSE)
  X <- 10 + 2 * (qr.Q(qr(Z)) %*% chol(R)) * sqrt(n_genes - 1)
  colnames(X) <- colnames(R)
  pmax(2^X - 1, 0)          # des comptes, jamais negatifs
}
block_cor <- function(sizes, w, b) {
  g <- rep(seq_along(sizes), sizes); n <- length(g)
  R <- matrix(b, n, n)
  for (i in 1:n) for (j in 1:n) if (g[i] == g[j]) R[i, j] <- w
  diag(R) <- 1
  nm <- paste0("C", g, "_", stats::ave(g, g, FUN = seq_along))
  dimnames(R) <- list(nm, nm); R
}
degrade <- function(counts, sample, share = 0.55, seed = 9) {
  set.seed(seed); lg <- log2(counts + 1)
  lg[, sample] <- (1 - share) * lg[, sample] + share * stats::rnorm(nrow(lg), 10, 2)
  pmax(2^lg - 1, 0)
}

healthy      <- exact_cor_counts(block_cor(c(4, 4, 4), 0.97, 0.80), 3000, 3)
one_degraded <- degrade(healthy, "C1_4")
duplicates   <- degrade(exact_cor_counts(block_cor(c(2,2,2,2,2,4), 0.96, 0.75), 3000, 7),
                        "C6_4", share = 0.60, seed = 11)
# A/B/C : cor(A,B) = 0.99, cor(A,C) = cor(B,C) = 0.50, une seule condition
R_abc <- matrix(c(1,.99,.50, .99,1,.50, .50,.50,1), 3, 3,
                dimnames = list(c("G_A","G_B","G_C"), c("G_A","G_B","G_C")))
abc <- exact_cor_counts(R_abc, 2000, 4)
# C4 biologiquement eloignee de tout le reste, mais interieurement saine
g6 <- rep(1:4, each = 3); nm6 <- paste0("C", g6, "_", stats::ave(g6, g6, FUN = seq_along))
R_far <- matrix(0.88, 12, 12)
for (i in 1:12) for (j in 1:12) if (g6[i] == g6[j]) R_far[i, j] <- 0.97
R_far[10:12, 1:9] <- R_far[1:9, 10:12] <- 0.45; diag(R_far) <- 1
dimnames(R_far) <- list(nm6, nm6)
far_condition <- exact_cor_counts(R_far, 4000, 41)

stopifnot(min(eigen(R_abc, TRUE, TRUE)$values) > 0,
          min(eigen(R_far, TRUE, TRUE)$values) > 0)   # vraies matrices de correlation

# ------------------------------------------------------- A. entrees, formes --
hdr("A. Entrees et dimensions")
chk("A1 une seule colonne retenue : erreur nommant le probleme",
    errs_clearly(sample_correlation(healthy[, "C1_1", drop = FALSE])))
chk("A2 une seule ligne retenue : erreur nommant le probleme",
    errs_clearly(sample_correlation(healthy[1, , drop = FALSE])))
chk("A3 deux lignes : erreur (correlation sur 2 points vaut toujours +-1)",
    errs_clearly(sample_correlation(healthy[1:2, ])))
chk("A4 conds partiellement absente : avertissement",
    warns(sample_correlation(healthy, conds = c("C1", "PAS_LA"))))
chk("A5 conds totalement absente : erreur",
    errs_clearly(suppressWarnings(sample_correlation(healthy, conds = "PAS_LA"))))
chk("A6 data.frame accepte",
    identical(dim(sample_correlation(as.data.frame(healthy))), c(12L, 12L)))
chk("A7 colonne non numerique : erreur nommant le probleme", {
  df <- as.data.frame(healthy); df$C1_1 <- as.character(df$C1_1)
  errs_clearly(sample_correlation(df)) })
chk("A8 methode invalide : erreur",
    inherits(try(sample_correlation(healthy, correlation_method = "peason"),
                 silent = TRUE), "try-error"))
chk("A9 valeurs negatives : erreur, pas des NaN silencieux", {
  neg <- healthy; neg[1, 1] <- -5; errs_clearly(sample_correlation(neg)) })
for (kk in list(-1, 0, NA_real_, Inf, c(3, 5), "5"))
  chk(paste("A10 k =", paste(format(kk), collapse = ",") , ": erreur"),
      errs_clearly(detect_sample_outliers(healthy, k = kk)))
chk("A11 la matrice rendue est une vraie matrice de correlation", {
  cm <- sample_correlation(healthy)
  isSymmetric(cm) && all(abs(diag(cm) - 1) < 1e-12) && all(abs(cm) <= 1 + 1e-12) })

# --------------------------------------- B. colonnes degenerees, evaluabilite --
hdr("B. Colonnes degenerees et evaluabilite")
dead <- healthy; dead[, "C2_1"] <- 0
const <- healthy; const[, "C3_2"] <- 42
one_na <- healthy; one_na[5, "C1_3"] <- NA

chk("B1 librairie entierement a zero : l'echantillon EST signale",
    "C2_1" %in% flagged_of(suppressWarnings(detect_sample_outliers(dead))))
chk("B2 elle ne rend pas overall non evaluable pour les autres", {
  r <- suppressWarnings(detect_sample_outliers(dead))
  sum(is.na(r$overall)) <= 1 })
chk("B3 colonne constante non nulle : l'echantillon EST signale",
    "C3_2" %in% flagged_of(suppressWarnings(detect_sample_outliers(const))))
chk("B4 une seule cellule NA n'aveugle pas tout le jeu", {
  r <- suppressWarnings(detect_sample_outliers(one_na)); sum(is.na(r$overall)) <= 1 })
chk("B5 flagged est logique et sans NA", {
  r <- suppressWarnings(detect_sample_outliers(dead))
  is.logical(r$flagged) && !any(is.na(r$flagged)) })
chk("B6 non evaluable et non signale sont distinguables",
    "reason" %in% names(suppressWarnings(detect_sample_outliers(dead))) &&
      any(grepl("usable|evaluable", suppressWarnings(detect_sample_outliers(dead))$reason,
                ignore.case = TRUE)))
chk("B7 un critere indisponible n'annule pas l'alerte d'un autre", {
  # groupes de 2 uniquement : deviation non evaluable partout
  d <- degrade(exact_cor_counts(block_cor(c(2,2,2,2), 0.96, 0.70), 3000, 5), "C1_2")
  "C1_2" %in% flagged_of(detect_sample_outliers(d)) })

# ------------------------------------------------------------- C. deviation --
hdr("C. Critere deviation")
chk("C1 A/B/C : G_C est signale malgre une MAD nulle",
    identical(flagged_of(detect_sample_outliers(abc)), "G_C"))
chk("C2 groupe a 2 replicats : deviation vaut NA, pas 0", {
  r <- detect_sample_outliers(exact_cor_counts(block_cor(c(2,2,2), 0.95, 0.70), 2000, 2))
  all(is.na(r$deviation)) })
chk("C3 la reference exclut reellement l'echantillon evalue", {
  # A/B/C : reference de G_C = cor(A,B) = 0.99, within = 0.50, donc -0.49
  r <- detect_sample_outliers(abc)
  abs(r$deviation[r$sample == "G_C"] - (0.50 - 0.99)) < 1e-6 })
chk("C4 plan majoritairement en duplicats : seul le degrade est signale",
    identical(flagged_of(detect_sample_outliers(duplicates)), "C6_4"))

# ------------------------------------------------- D. seuils, MAD, classement --
hdr("D. Seuils, MAD et classement")
chk("D1 profil c(0.95,0.95,0.95,0.20) : signale malgre mad = 0",
    length(flagged_of(detect_sample_outliers(abc))) == 1)
chk("D2 la premiere ligne est un echantillon signale", {
  r <- detect_sample_outliers(one_degraded)
  !any(r$flagged) || isTRUE(r$flagged[1]) })
chk("D3 aucune colonne numerique non finie en sortie", {
  r <- suppressWarnings(detect_sample_outliers(dead))
  num <- r[, c("within","between","gap","overall","deviation")]
  all(vapply(num, function(v) all(is.na(v) | is.finite(v)), logical(1))) })
chk("D4 l'ordre est deterministe", {
  identical(detect_sample_outliers(one_degraded)$sample,
            detect_sample_outliers(one_degraded)$sample) })

# --------------------------------------------------------- E. gap, libelles --
hdr("E. Critere gap et libelles")
chk("E1 'closer to another condition' jamais affiche avec gap > 0", {
  r <- detect_sample_outliers(one_degraded)
  bad <- grepl("closer to another condition", r$reason) & r$gap > 0
  !any(bad, na.rm = TRUE) })
chk("E2 condition legitimement eloignee : aucune alerte",
    length(flagged_of(detect_sample_outliers(far_condition))) == 0)
chk("E3 un degrade dans un groupe de 4 : lui seul est signale",
    identical(flagged_of(detect_sample_outliers(one_degraded)), "C1_4"))
chk("E4 jeu sain : aucune alerte",
    length(flagged_of(detect_sample_outliers(healthy))) == 0)

# ------------------------------------------- F. combinaison et jeu de reference --
hdr("F. Combinaison des criteres et jeu de reference")
chk("F1 within/between/overall restent dans l'etendue des correlations observees", {
  r <- detect_sample_outliers(healthy); cm <- sample_correlation(healthy)
  r <- r[match(colnames(cm), r$sample), ]
  cond <- r$condition
  all(vapply(seq_len(nrow(r)), function(i) {
    same <- setdiff(which(cond == cond[i]), i); other <- which(cond != cond[i])
    inside <- function(v, x) is.na(x) || (x >= min(v) - 1e-12 && x <= max(v) + 1e-12)
    inside(cm[i, same], r$within[i]) && inside(cm[i, other], r$between[i]) &&
      inside(cm[i, -i], r$overall[i]) }, logical(1))) })
chk("F2 colonnes de sortie stables", {
  identical(names(detect_sample_outliers(healthy)),
            c("sample","condition","within","between","gap","overall",
              "deviation","flagged","reason")) })
chk("F3 abiotic_stresses (raw) : aucune alerte", {
  data("abiotic_stresses", envir = environment())
  length(flagged_of(detect_sample_outliers(abiotic_stresses$raw_counts))) == 0 })
chk("F4 abiotic_stresses (normalized) : aucune alerte", {
  data("abiotic_stresses", envir = environment())
  length(flagged_of(detect_sample_outliers(abiotic_stresses$normalized_counts))) == 0 })
chk("F5 fausses alertes < 2 % sur 100 jeux sains simules", {
  fp <- replicate(100, {
    ng <- 4000; g <- rep(1:6, each = 3)
    base <- matrix(stats::rnorm(ng * 6, 10, 2), ng, 6)
    X <- base[, g] + matrix(stats::rnorm(ng * 18, 0, 1.1), ng, 18)
    colnames(X) <- paste0("C", g, "_", stats::ave(g, g, FUN = seq_along))
    length(flagged_of(detect_sample_outliers(pmax(2^X - 1, 0)))) > 0 })
  mean(fp) < 0.02 })

# ------------------------------------------------------- G. contrat d'entree --
hdr("G. Contrat d'entree et transformation")
chk("G1 log2(x+1) reste sans effet sur spearman (a documenter)", {
  raw <- round(healthy)
  max(abs(stats::cor(raw, method = "spearman") -
          stats::cor(log2(raw + 1), method = "spearman"))) < 1e-12 })
chk("G2 donnees deja en log : avertissement", {
  warns(sample_correlation(log2(healthy + 1))) })
chk("G3 non-regression de draw_correlation_heatmap : valeurs inchangees", {
  p <- draw_correlation_heatmap(healthy)
  identical(round(sample_correlation(healthy), 2),
            {  m <- reshape2::acast(p$data, Var1 ~ Var2, value.var = "value")
               m[rownames(sample_correlation(healthy)),
                 colnames(sample_correlation(healthy))] }) })
chk("G4 la heatmap interactive rend bien un plotly",
    inherits(draw_correlation_heatmap_interactive(healthy), "plotly"))
chk("G5 un seul gene survivant au filtre de la page d'accueil : erreur claire", {
  errs_clearly(detect_sample_outliers(healthy[1, , drop = FALSE])) })

chk("G6 heatmap interactive : echelle finie malgre une librairie morte", {
  p <- draw_correlation_heatmap_interactive(dead)
  z <- p$x$attrs[[1]]; is.finite(z$zmin) && is.finite(z$zmax) })
chk("G7 heatmap statique : limites finies malgre une librairie morte", {
  p <- suppressWarnings(draw_correlation_heatmap(dead))
  lim <- p$scales$scales[[1]]$limits; all(is.finite(lim)) })

cat(sprintf("\n===== %d PASS, %d FAIL =====\n", nb_ok, nb_ko))
if (nb_ko) cat(paste0("  - ", failures, collapse = "\n"), "\n")
quit(status = if (nb_ko) 1 else 0)
