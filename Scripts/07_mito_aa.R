# Mitochondrial amino-acid differences among lineages and clades -----------------------
# Comparative candidate analysis, mitochondrial part. No gene annotation is available for
# the clade mitogenome contigs (de novo assemblies), so protein-coding genes are found as
# open reading frames (invertebrate mitochondrial code, NCBI table 5, >= 150 codons, both
# strands). COX1 and CYTB are identified by conserved motifs (HPEVY, PEWY); other ORFs are
# named by length and strand. ORFs that lie inside another ORF on the opposite strand are
# flagged: one of each such pair is probably not a real gene.
#
# 1. Within F (16.1 kb contig): for every ORF, the amino acid (and nucleotide) states that are
#    fixed or nearly fixed (>= 80% of called individuals, >= 5 called) in each F lineage, and
#    the sites where lineages differ - nonsynonymous vs synonymous counts per ORF.
# 2. Across clades: COX1, CYTB and one further ORF present on all four contigs (motif LQDA):
#    clade consensus proteins, aligned to F, pairwise differences, and the substitutions
#    unique to IV (IV differs, F, S and X share a state) - IV is the low-CTmax clade.
#
# Inputs (cluster export): data/reference/mito/mtDNA_clades.fa,
#   results/03_clades/mito/mitogenome_consensus_{F,S,X,IV}.fa (one per individual, contig
#   coordinates); Output_data/sample_table.csv (lineage).
# Outputs (Output_data/mito_aa/): F_lineage_orfs.csv, F_lineage_aa_sites.csv,
#   clade_aa_pairs.csv, IV_specific_aa.csv
# Requires: here, tidyverse

library(here)
library(tidyverse)

export_dir <- sort(list.dirs(here("Raw_data", "cluster_exports"), recursive = FALSE), decreasing = TRUE)[1]
out_dir <- here("Output", "Output_data", "mito_aa")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
excluded <- c("MR_peak_2_04", "MR_peak_2_05")
min_codons <- 150
min_called <- 5
min_major <- 0.8

read_fasta <- function(f) {
  x <- readLines(f)
  h <- grep("^>", x)
  seqs <- map_chr(seq_along(h), \(i) {
    to <- if (i < length(h)) h[i + 1] - 1 else length(x)
    if (to > h[i]) toupper(paste(x[(h[i] + 1):to], collapse = "")) else ""
  })
  set_names(seqs, sub("^>(\\S+).*", "\\1", x[h]))
}

# invertebrate mitochondrial code (table 5)
bases <- c("T", "C", "A", "G")
codons <- paste0(rep(bases, each = 16), rep(rep(bases, each = 4), 4), rep(bases, 16))
aa5 <- strsplit("FFLLSSSSYY**CCWWLLLLPPPPHHQQRRRRIIMMTTTTNNKKSSSSVVVVAAAADDEEGGGG", "")[[1]]
code5 <- set_names(aa5, codons)
revcomp <- function(s) chartr("ACGTN", "TGCAN", paste(rev(strsplit(s, "")[[1]]), collapse = ""))
cods <- function(s) substring(s, seq(1, nchar(s) - 2, 3), seq(3, nchar(s), 3))
translate <- function(s) {
  if (nchar(s) < 3) return("")
  a <- code5[cods(s)]
  a[is.na(a)] <- "X"          # codons with N or gaps
  paste(a, collapse = "")
}

find_orfs <- function(s) {
  L <- nchar(s)
  map_dfr(c("+", "-"), \(strand) {
    sq <- if (strand == "+") s else revcomp(s)
    map_dfr(0:2, \(f) {
      aa <- translate(substr(sq, f + 1, L - ((L - f) %% 3)))
      segs <- strsplit(aa, "*", fixed = TRUE)[[1]]
      starts <- cumsum(c(0, nchar(segs[-length(segs)]) + 1))
      tibble(seg = segs, aa_start = starts) %>%
        filter(nchar(seg) >= min_codons) %>%
        mutate(strand = strand,
               # start at the first M within the first 60 codons, else keep the whole segment
               off = map_int(seg, \(x) { m <- regexpr("M", x)[1]; if (m > 0 && m <= 60) m - 1L else 0L }),
               a = f + 3 * (aa_start + off), b = f + 3 * (aa_start + nchar(seg)),   # 0-based, on sq
               protein = substring(seg, off + 1),
               raw = seg)
    })
  }) %>%
    mutate(gene = case_when(str_detect(raw, "HPEVY") ~ "COX1",
                            str_detect(raw, "PEWY") ~ "CYTB",
                            TRUE ~ paste0("ORF", nchar(protein), strand)),
           start = if_else(strand == "+", a + 1, L - b + 1),   # 1-based contig coordinates
           end = if_else(strand == "+", b, L - a))
}

orf_seq <- function(s, o) {
  sq <- if (o$strand == "+") s else revcomp(s)
  substr(sq, o$a + 1, o$b)
}

major <- function(v) {
  v <- v[!v %in% c("X", "N", "-")]
  if (length(v) < min_called) return(list(state = NA_character_, freq = NA_real_, n = length(v)))
  t <- sort(table(v), decreasing = TRUE)
  list(state = names(t)[1], freq = as.numeric(t[1]) / length(v), n = length(v))
}

# ---- inputs ----
ref <- read_fasta(file.path(export_dir, "data", "reference", "mito", "mtDNA_clades.fa"))
cons <- map(set_names(c("F", "S", "X", "IV")), \(cl)
  read_fasta(file.path(export_dir, "results", "03_clades", "mito", paste0("mitogenome_consensus_", cl, ".fa"))))
lineages <- read_csv(here("Output", "Output_data", "sample_table.csv"), show_col_types = FALSE) %>%
  select(sample_id, lineage) %>% filter(!is.na(lineage))
# ORFs per clade contig; flag ORFs nested inside an ORF on the opposite strand
orfs <- map(set_names(c("F", "S", "X", "IV")), \(cl) {
  o <- find_orfs(ref[[paste0("mtDNA_", cl)]])
  o$nested_antisense <- map_lgl(seq_len(nrow(o)), \(i)
    any(o$start <= o$start[i] & o$end >= o$end[i] & o$strand != o$strand[i]))
  o
})

# ---- 1. F lineages ----
f_ids <- intersect(names(cons$F), lineages$sample_id[str_detect(lineages$lineage, "^F")])
f_ids <- setdiff(f_ids, excluded)
f_lin <- set_names(lineages$lineage[match(f_ids, lineages$sample_id)], f_ids)
lins <- sort(unique(f_lin))
fixed_states <- function(mat) {
  # mat: individuals x positions (character); per lineage majority state where >= min_major
  map(set_names(lins), \(l) {
    m <- mat[f_lin[rownames(mat)] == l, , drop = FALSE]
    apply(m, 2, \(v) { r <- major(v); if (!is.na(r$state) && r$freq >= min_major) r$state else NA_character_ })
  }) %>% do.call(cbind, .)
}
site_rows <- list(); orf_rows <- list()
for (i in seq_len(nrow(orfs$F))) {
  o <- orfs$F[i, ]
  nt <- map_chr(f_ids, \(id) orf_seq(cons$F[[id]], o))
  aa <- map_chr(nt, translate)
  aa_mat <- do.call(rbind, strsplit(aa, "")); rownames(aa_mat) <- f_ids
  nt_mat <- do.call(rbind, strsplit(nt, "")); rownames(nt_mat) <- f_ids
  fa <- fixed_states(aa_mat); fn <- fixed_states(nt_mat)
  diff_aa <- which(apply(fa, 1, \(r) n_distinct(na.omit(r)) > 1))
  diff_nt <- which(apply(fn, 1, \(r) n_distinct(na.omit(r)) > 1))
  nonsyn <- sum(map_lgl(diff_nt, \(j) n_distinct(na.omit(fa[(j - 1) %/% 3 + 1, ])) > 1))
  orf_rows[[i]] <- tibble(gene = o$gene, strand = o$strand, start = o$start, end = o$end,
                          codons = nchar(o$protein), nested_antisense = o$nested_antisense,
                          fixed_nt_differences = length(diff_nt), nonsynonymous = nonsyn,
                          synonymous = length(diff_nt) - nonsyn)
  if (length(diff_aa)) {
    site_rows[[i]] <- map_dfr(diff_aa, \(j) {
      st <- map(set_names(lins), \(l) major(aa_mat[f_lin == l, j]))
      tibble(gene = o$gene, aa_pos = j, lineage = lins,
             state = map_chr(st, "state"), freq = map_dbl(st, "freq"), n = map_int(st, "n"))
    })
  }
}
f_orfs <- bind_rows(orf_rows) %>% arrange(start)
f_sites <- bind_rows(site_rows)
write_csv(f_orfs, file.path(out_dir, "F_lineage_orfs.csv"))
write_csv(f_sites, file.path(out_dir, "F_lineage_aa_sites.csv"))

# ---- 2. across clades ----
clade_protein <- function(cl, o) {
  ids <- setdiff(names(cons[[cl]]), excluded)
  aa <- do.call(rbind, strsplit(map_chr(ids, \(id) translate(orf_seq(cons[[cl]][[id]], o))), ""))
  paste(apply(aa, 2, \(v) { r <- major(v); if (is.na(r$state)) "X" else r$state }), collapse = "")
}
# global alignment (Needleman-Wunsch; match 1, mismatch -1, gap -2): returns y projected onto x
nw_project <- function(x, y) {
  x <- strsplit(x, "")[[1]]; y <- strsplit(y, "")[[1]]
  n <- length(x); m <- length(y); g <- -2
  H <- matrix(0, n + 1, m + 1); H[, 1] <- g * 0:n; H[1, ] <- g * 0:m
  P <- matrix(0L, n + 1, m + 1)          # 1 diag, 2 up (gap in y), 3 left (gap in x)
  for (i in 1:n) for (j in 1:m) {
    s <- c(H[i, j] + if (x[i] == y[j]) 1 else -1, H[i, j + 1] + g, H[i + 1, j] + g)
    P[i + 1, j + 1] <- which.max(s); H[i + 1, j + 1] <- max(s)
  }
  out <- rep("-", n); i <- n; j <- m
  while (i > 0 && j > 0) {
    if (P[i + 1, j + 1] == 1L) { out[i] <- y[j]; i <- i - 1; j <- j - 1 }
    else if (P[i + 1, j + 1] == 2L) i <- i - 1 else j <- j - 1
  }
  out
}
pick <- function(cl, gene = NULL, motif = NULL) {
  o <- orfs[[cl]]
  if (!is.null(gene)) o <- filter(o, .data$gene == !!gene)
  if (!is.null(motif)) o <- filter(o, str_detect(raw, motif), strand == "-", nchar(protein) > 180)
  o[1, ]
}
shared <- list(COX1 = list(gene = "COX1"), CYTB = list(gene = "CYTB"), ORF_LQDA = list(motif = "LQDA"))
pairs_out <- list(); iv_out <- list()
for (g in names(shared)) {
  prot <- map(set_names(c("F", "S", "X", "IV")), \(cl) clade_protein(cl, do.call(pick, c(list(cl), shared[[g]]))))
  proj <- map(prot, \(p) nw_project(prot$F, p))
  pm <- do.call(cbind, proj)
  ok <- \(v) !v %in% c("X", "-")
  pairs_out[[g]] <- combn(names(proj), 2, simplify = FALSE) %>% map_dfr(\(p) {
    a <- pm[, p[1]]; b <- pm[, p[2]]; k <- ok(a) & ok(b)
    tibble(gene = g, pair = paste(p, collapse = "-"), aligned_aa = sum(k), differences = sum(a[k] != b[k]))
  })
  k <- apply(pm, 1, \(r) all(ok(r))) & pm[, "F"] == pm[, "S"] & pm[, "F"] == pm[, "X"] & pm[, "IV"] != pm[, "F"]
  fprot <- strsplit(prot$F, "")[[1]]
  iv_out[[g]] <- tibble(gene = g, pos_F = which(k), F_S_X = pm[k, "F"], IV = pm[k, "IV"],
                        context_F = map_chr(which(k), \(i) paste(fprot[max(1, i - 5):min(length(fprot), i + 5)], collapse = "")))
}
write_csv(bind_rows(pairs_out), file.path(out_dir, "clade_aa_pairs.csv"))
write_csv(bind_rows(iv_out), file.path(out_dir, "IV_specific_aa.csv"))

message("F lineages: ", paste(lins, collapse = ", "), "; ", nrow(f_orfs), " ORFs, ",
        n_distinct(paste(f_sites$gene, f_sites$aa_pos)), " amino-acid sites differing between lineages")
print(f_orfs, n = Inf)
print(bind_rows(pairs_out), n = Inf)
message(nrow(bind_rows(iv_out)), " IV-specific substitutions")
