# Comparing seasonal and latitudinal patterns in thermal adaptation

2026-10-29

- [Main Message](#main-message)
- [1. Collections and environment](#1-collections-and-environment)
  - [Sites and collection conditions](#sites-and-collection-conditions)
  - [Paired low- and high-salinity
    sites](#paired-low--and-high-salinity-sites)
  - [Generations between collections](#generations-between-collections)
- [2. Phenotypes: thermal limits and body
  size](#2-phenotypes-thermal-limits-and-body-size)
  - [Critical Thermal Limits](#critical-thermal-limits)
  - [Body Size](#body-size)
  - [Warming tolerance](#warming-tolerance)
  - [Trait correlations](#trait-correlations)
  - [Paired sites: trait differences](#paired-sites-trait-differences)
  - [Assay checks](#assay-checks)
- [3. Low-coverage whole genome data](#3-low-coverage-whole-genome-data)
  - [Sequencing summary](#sequencing-summary)
  - [Site filtering](#site-filtering)
  - [Clade identity](#clade-identity)
  - [Genetic differentiation within
    clades](#genetic-differentiation-within-clades)
  - [Relatedness and sample quality](#relatedness-and-sample-quality)
  - [Local PCA and candidate
    inversions](#local-pca-and-candidate-inversions)
  - [Parasites and symbionts](#parasites-and-symbionts)
  - [Seasonal allele-frequency
    change](#seasonal-allele-frequency-change)
  - [Does space substitute for time? Concordance of temperature
    responses](#does-space-substitute-for-time-concordance-of-temperature-responses)
- [4. Clade-specific physiological
  patterns](#4-clade-specific-physiological-patterns)
  - [CTmax model](#ctmax-model)
  - [Acclimation vs. site differences (within- and between-site
    temperature)](#acclimation-vs-site-differences-within--and-between-site-temperature)
  - [Trait variability within clades](#trait-variability-within-clades)
  - [Parasites, epibionts and thermal
    limits](#parasites-epibionts-and-thermal-limits)
- [5. Partitioning vulnerability](#5-partitioning-vulnerability)
  - [Scenario 1 (Fixed CTmax)](#scenario-1-fixed-ctmax)
  - [Scenario 2 (Clade-level
    variation)](#scenario-2-clade-level-variation)
  - [Scenario 3 (Clade variation and
    acclimation/adaptation)](#scenario-3-clade-variation-and-acclimationadaptation)
  - [Comparing Scenarios](#comparing-scenarios)

## Main Message

By assaying thermal limits several times per year in natural populations
distributed across a large latitudinal gradient, we highlight the
importance of intra-specific variation for predicting vulnerability to
climate change. Indeed, across the entire spatiotemporal framework
examined here, we observed an ~12°C range in thermal limits. Both
spatial and temporal variations in thermal limits are essential for
robust predictions. Also highlighted in this study are the major
differences in thermal limits between the various cryptic lineages of
*Acartia tonsa*. The broad (and sometimes overlapping) distrbutions of
these clades provide a unique opportunity to compare replicate
spatiotemporal patterns in adaptation against a relatively similar
genetic background.

## 1. Collections and environment

Collection sites, conditions at the time of collection, continuous
temperature records, and the timing of collections relative to copepod
generation times.

### Sites and collection conditions

Copepods were collected by surface tow from sites across the Western
Atlantic at several times throughout the year. The sites are shown below
along with surface temperatures and salinity were measured at the time
of collection. Across the entire set of collections, temperature ranged
from 10°C to 36°C.

<img src="../Figures/site-chars-1.png" data-fig-align="center" />

Collections aimed to obtain copepods near the onset of peak
temperatures, after peak temperatures, and then after temperatures
cooled during the Fall. To put the collections into this larger seasonal
context, we examined continuous temperature records for each site.
Continuous data is not available for all sites, however, so here we’ve
pieced together daily temperature values from either local temperature
sensors (sites in Florida and the Chesapeake) and high resolution
satellite temperature data (Connecticut, Maine, and the Canadian sites).
This satellite data comes from the NOAA 1/4° Daily Optimum Interpolation
Sea Surface Temperature (OISST).

These temperature profiles are shown below, with the temperatures
measured during the time of collection included for comparison While
collection temperatures in Maine did not always match the recorded daily
averages, across all sites the temperature records do give a general
sense of the timing of seasonal maxima (and the proximity of collections
to the seasonal maximum temperature). In general, the first sample from
each site fell just after the site reached the warmest period. The
exception to that pattern is in Florida, where collections occurred
after an extended period of high temperatures.

<img src="../Figures/continuous-temps-1.png" data-fig-align="center" />

Exact locations for the sites are provided here:

|           Site           |    Region     |   Lat    |   Long    |
|:------------------------:|:-------------:|:--------:|:---------:|
|        Key Largo         |    Florida    | 25.28391 | -80.33014 |
|      Manatee River       |    Florida    | 27.50561 | -82.57277 |
|        Ft. Hamer         |    Florida    | 27.52488 | -82.43101 |
|        Tyler Cove        |   Maryland    | 38.35083 | -76.22902 |
|      Ganey’s Wharf       |   Maryland    | 38.80555 | -75.90906 |
|       Esker Point        |  Connecticut  | 41.32081 | -72.00166 |
|       Sawyer Park        |     Maine     | 43.90698 | -69.87179 |
| St. Thomas de Kent Wharf | New Brunswick | 46.44761 | -64.63692 |
|      Ritchie Wharf       | New Brunswick | 47.00481 | -65.56291 |

The latitudinal gradient covers a wide range of seasonality. Shown below
is the temperature range. While based on collection temperatures, and
therefore an underestimate of the total seasonal range of temperatures,
these patterns are representative of the expected latitudinal gradient
in seasonality.

<img src="../Figures/lat-temp-range-plot-1.png"
data-fig-align="center" />

There was a general increase in the collection temperature range at
warmer sites. This provides an opportunity to disentangle some of the
traditionally proposed hypotheses about thermal adaptation
(macrophysiological gradients often include stable and warm environments
contrasted with cooler, more variable environment).

<img src="../Figures/max-temp-vs-range-1.png" data-fig-align="center" />

### Paired low- and high-salinity sites

Nested within each of the three regions (South, Central, and Northern
regions) are pairs of low and high salinity sites:

| Region  | Low.Salinity  |      High.Salinity       |
|:-------:|:-------------:|:------------------------:|
|  South  |   Ft. Hamer   |      Manatee River       |
| Central | Ganey’s Wharf |        Tyler Cove        |
|  North  | Ritchie Wharf | St. Thomas de Kent Wharf |

 

There are fairly well-established divergences between high salinity and
low salinity populations of *Acartia tonsa*. These sets of
geographically proximate sites potentially provide independent
comparisons of the effects of seasonality across salinity gradients.
Shown here are the collection conditions for these pairs of sites.
Temperature was typically similar across the pairs within each
collection, while salinity differences were fairly stable across
collections.

<img src="../Figures/season-sal-comps-1.png" data-fig-align="center" />

### Generations between collections

<img src="../Figures/num-gens-plot-1.png" data-fig-align="center" />

## 2. Phenotypes: thermal limits and body size

### Critical Thermal Limits

A total of 427 individuals were examined. Critical thermal limits and
body size measurements were made before individuals were preserved in
ethanol. We excluded data for individuals that were either later
classified as males or juveniles, that we were unable to assign to a
clade, or that were identified as individuals from a closely related
species (*Acartia hudsonica*). With these individuals excluded, the full
data set contains 395 adult females with CTmax and body size
measurements, and clade identifications.

Critical thermal maxima (CTmax) was measured using a custom setup. The
method uses a standard dynamic ramping assay to determine the maximum
temperature individuals could sustain normal functioning. This differs
from lethal temperatures, and indeed, all individuals recovered
following the assay.

Individuals were rested for one hour after collection before the assay.
During the assay, copepods were held in artificial seawater, composed of
bottled spring water and Instant Ocean salt mix adjusted to match
collection salinities. During the assay, several ‘control’ individuals
were maintained in this solution at ambient temperatures without the
temperature ramp to ensure that there was no background changes in
behavior or mortality. When sorting individuals from the plankton tow
contents, they were held in a 50:50 mix of 60 um filtered water from the
collection site and artificial seawater as an additional acclimation
step.

Sample sizes varied slightly across experiments, but generally ~20
individuals were measured per season at each site. The major exceptions
were the early samples from the Florida sites and the late sample from
Sawyer Park (Maine). Only two sets of samples (peak and late) were
collected from Fort Hamer and Manatee River. The late season collection
from Sawyer Park occurred after *Acartia tonsa* abundance decreased, and
samples were dominated by *Acartia hudsonica*. Thermal limits were
measured for all *A. tonsa* individuals recovered. No samples were
collected from Key Largo for this project, as *Acartia tonsa* wasn’t
present in the water during the peak season, likely due to a recent
extreme heat event.

| site                     | early | peak | late |
|:-------------------------|------:|-----:|-----:|
| Manatee River            |     0 |   17 |   20 |
| Ft. Hamer                |     0 |   19 |   19 |
| Tyler Cove               |    20 |   19 |   20 |
| Ganey’s Wharf            |    20 |   20 |   20 |
| Esker Point              |    17 |   19 |   20 |
| Sawyer Park              |    14 |   17 |    8 |
| St. Thomas de Kent Wharf |    15 |   19 |   19 |
| Ritchie Wharf            |    16 |   18 |   19 |

Shown below are the measured CTmax values. Individual measurements are
shown in small points for each collection. The large points indicate the
mean values for each collection.

### Body Size

Following the CTmax assay, individuals were photographed for body size
measurements. Prosome lengths were measured from these photographs using
a scale micrometer and the software ImageJ. These measurements are shown
below. As before, large points indicate the mean body size. While less
cohesive than CTmax, a general trend of increasing body size with
latitude and time of year can be seen.

<img src="../Figures/size-and-ctmax-season-1.png"
data-fig-align="center" />

### Warming tolerance

Warming tolerance (the difference between thermal limits and
environmental temperatures) is a commonly used metric of climate
vulnerability. We calculated this as the difference between measured
CTmax values and the collection temperature. Smaller warming tolerance
values indicate that populations were nearer to their upper thermal
limits, suggesting greater vulnerability.

<img src="../Figures/seasonal-warming-tol-1.png"
data-fig-align="center" />

### Trait correlations

Regardless of the underlying mechanism (genetic differentiation or
phenotypic plasticity), we expect that collections from warmer waters
should yield copepods with higher thermal limits and smaller body sizes.
Our observations largely fit this expectation, with strong increases in
CTmax at higher temperatures, and a general decrease in prosome lengths
as temperature increased. There was a larger correlation between CTmax
and temperature than size and temperature. There are myriad reasons this
might be observed, but one potential is the potential for acclimation of
thermal limits. Body size is fixed in the adult stage, while CTmax can
continue to vary as water temperature changes. This is indirect evidence
for the importance of rapid acclimation in the observed patterns.

<img src="../Figures/temp-cors-1.png" data-fig-align="center" />

The relationship between prosome length and CTmax is also of interest.
Given that larger body sizes are associated with cold
adaptation/acclimation, it is unsurprising that there is a general trend
of decreasing thermal limits with increasing size. Individual regression
lines for each site are also included - the dark grey lines in the
background represent the ‘universal’ regression for that site, with
individual colored regression lines for each collection. At most sites,
evidence for this negative relationship between CTmax and body size is
observed across collections rather than within collections, suggesting
this relationship is primaly a reflection of the response to seasonal
temperature variation.

<img src="../Figures/ctmax-vs-size-1.png" data-fig-align="center" />

### Paired sites: trait differences

The three pairs of cross-salinity comparisons do show evidence for
fine-scale trait divergence, although there was no consistent pattern in
the direction or magnitude of differences. CTmax was similar across
sites in the Southern and Central pairs. In the Northern pair, CTmax
tended to be slightly lower for individuals from the low salinity site.
Size was more variable between the paired sites. In the South,
individuals from the low salinity site were consistently smaller than
individuals from the high salinity site, despite experiencing similar
temperatures. By contrast, in the Central pair individuals from the low
salinity site tended to be slightly larger than those from the high
salinity site, although the magnitude of this difference this varied
seasonally. In the Northern pair, individuals from the low salinity site
were slightly larger than individuals from the high salinity site during
the early and peak season collections, but there was an abrupt reversal
of this pattern during the late collection, when individuals from the
high salinity site were much larger than individuals from the low
salinity site.

Note that two of the three pairs also differ in clade composition:
Manatee River and Ft. Hamer hold different F lineages (F-MR, F-FH), and
St. Thomas de Kent Wharf and Ritchie Wharf differ in their mix of clades
X and IV (Section 3). Differences between paired sites therefore combine
environment, lineage and clade composition; Section 4 separates them.

<img src="../Figures/sal-pair-traits-1.png" data-fig-align="center" />

### Assay checks

<img src="../Figures/ramp-record-plot-1.png" data-fig-align="center" />

<img src="../Figures/rep-comp-1.png" data-fig-align="center" />

<img src="../Figures/rank-sims-1.png" data-fig-align="center" />

## 3. Low-coverage whole genome data

After phenotyping, each individual was preserved in 95% ethanol.
Individual DNA libraries were prepared using Twist Bio 96-plex prep
kits, then sequenced on two lanes of an Illumina NovaSeq X Plus 25B by
Novogene (436 libraries). Read processing, mapping, site filtering,
clade assignment and population genomic analyses were run on the Unity
cluster (Snakemake project `aton_lcwg`). The figures in this section are
built from the exported result tables (`Raw_data/cluster_exports/`;
export in use: aton_lcwg_tables_2026-10-29). *Acartia hudsonica*
libraries are excluded throughout.

### Sequencing summary

Reads per library varied over two orders of magnitude; 23 libraries
failed (\< 500,000 reads, 20 of them on plate 5). The reference genome
is closest to clade X: the other clades map less well and yield less
usable depth per read, on every plate. Libraries with low mapping and
high GC content carry DNA from other organisms (Section 3.3).

<img src="../Figures/seq-overview-1.png" data-fig-align="center" />

There is no clear relationship between the number of reads and
individual size or CTmax (phenotype analysis set only).

<img src="../Figures/read-exp-factors-1.png" data-fig-align="center" />

|           |       Sum Sq |  Df |   F value |   Pr(\>F) |
|:----------|-------------:|----:|----------:|----------:|
| size      | 9.603839e+14 |   1 | 2.6795203 | 0.1024488 |
| ctmax     | 1.671663e+11 |   1 | 0.0004664 | 0.9827809 |
| Residuals | 1.404992e+17 | 392 |        NA |        NA |

### Site filtering

Genotype-likelihood analyses use only the 15 chromosomes, minus
soft-masked repeats, nuclear copies of mitochondrial DNA (NUMTs) and
SNPs flagged as collapsed paralogs by ngsParalog. Because of the
reference bias, far fewer sites pass the missing-data filter (data in at
least 50% of individuals) in clades S and IV than in X.

<img src="../Figures/site-filtering-1.png" data-fig-align="center" />

### Clade identity

Clades were assigned in two independent ways. **Mitochondrial:** each
individual’s reads were mapped to one partial mitogenome per clade, and
the clade was taken from the reads covering the COI barcode region.
**Nuclear:** genome-wide genotype likelihoods (PCAngsd, NGSadmix) on all
kept libraries. The two agree for every individual. Two libraries
contained DNA from individuals of two different clades (both
mitochondrial and nuclear signals mixed) and were excluded.

<img src="../Figures/mito-read-split-1.png" data-fig-align="center" />

All four clades are cleanly separated in the nuclear genome. Clade F
splits further into site-specific sub-lineages (Manatee River, Ft.
Hamer, and the Chesapeake/New England group), visible on PC4 and at K =
5–6.

<img src="../Figures/clade-structure-1.png" data-fig-align="center" />

Within clade F, the sub-lineages are clearly separated (2–6% COI
divergence) but much less divergent than the clades are from each other
(12–18%). We therefore analyse four clades, and keep the F sub-lineage
as an annotation.

<img src="../Figures/coi-divergence-1.png" data-fig-align="center" />

<img src="../Figures/clade-props-1.png" data-fig-align="center" />

### Genetic differentiation within clades

Population structure and diversity are analysed within each clade (X, F,
S, IV), with populations defined as clade × site × sampling event (n ≥
8). Within clade F, site differences include differences between F
sub-lineages.

<img src="../Figures/within-clade-pca-1.png" data-fig-align="center" />

<img src="../Figures/fst-pairs-1.png" data-fig-align="center" />

#### Isolation by distance

Genetic differentiation between sites within each clade, against the
straight-line distance between sites. F<sub>ST</sub> is averaged over
the season pairs available for each pair of sites. F pairs between
sub-lineages (F-MR, F-FH, F-N) are shown separately: they reflect
divergence between lineages, not gene flow between populations.
Straight-line distances underestimate the distance by sea (around
Florida, and around Nova Scotia between Maine and the Gulf of
St. Lawrence), and each clade has only 2–4 sites, so this is
descriptive.

<img src="../Figures/ibd-1.png" data-fig-align="center" />

Shown below are estimates of diversity across the data set. Tajima’s D
can be used to detect random vs. non-random evolutionary processes
(e.g. drift vs. selection). A value near zero indicates neutral
processes and a stable population size. Negative values indicate an
excess of rare alleles, suggesting possible recent selective sweeps or
population expansion after a bottleneck. Positive values indicate
possible population contraction or balancing selection, resulting in
fewer rare alleles than expected.

Most values of Tajima’s D for our data set are small (within 0.5 of 0).
A few samples reached moderate values around -1 and 1. Interestingly,
all cases are from the two sites in the Chesapeake, with a low Tajima’s
D value for S clade individuals at the high salinity site, and high
values for F clade individuals at both high and low salinity sites.

We also calculated nucleotide diversity, which represents the average
number of nucleotide differences between randomly sampled sequences in a
population. Theta values tended to be high, with the exception of F
Clade individuals at the low salinity site in Florida and the two sites
in the Chesapeake.

We note that these diversity metrics should only be compared within
clades since reference bias changes which sites are usable in each
clade.

Within sites, there is notably little change in these metrics across
seasons. Three general within-clade patterns can be seen across sites:

1.  **Increasing Tajima’s D and decreased nucleotide diversity,** as
    seen in Clade F. This may indicate serial founder effects. Diversity
    should fall steadily in the direction of expansion ([Excoffier et
    al. 2009](https://doi.org/10.1146/annurev.ecolsys.39.110707.173414)).
    For a clade spreading north since the last glacial maximum, northern
    sites would be expected to have lower θπ and higher D ([Hewitt
    2000](https://doi.org/10.1038/35016000)). This suggests a low
    latitude origin for this clade.
2.  **Consistently low Tajima’s D and consistently high nucleotide
    diversity**, as seen in Clades S and IV (although note the limited
    sampling for these clades). This pattern suggests large,
    well-connected populations. There may have been population expansion
    before sites diverged, or there may be substantial gene flow between
    populations keeping them similar ([Excoffier et
    al. 2009](https://doi.org/10.1146/annurev.ecolsys.39.110707.173414)).
3.  **Consistently small Tajima’s D and consistently high nucleotide
    diversity**, as seen in Clade X. While this typically would indicate
    large, stable populations, Clade X is found only at the high
    latitude sites, where populations of *Acartia tonsa* are present
    during only part of the year, exhibiting drastic fluctuations in
    population size over this period. This may indicate the importance
    of an egg bank / resting eggs. If the population is generated each
    year by hatching out of eggs laid over many previous years,
    diversity would remain high. The egg bank acts as a store of
    diversity, so a brief active season doesn’t mean a small genetic
    population ([Hairston
    1996](https://doi.org/10.4319/lo.1996.41.5.1087)). Resting eggs are
    well documented in *Acartia* (Marcus 1996, *Hydrobiologia*
    320:141–152). In Narragansett Bay, *A. tonsa* replaces the
    egg-banking *A. hudsonica* each summer ([Sullivan & McManus
    1986](https://doi.org/10.3354/meps028121)), the same seasonal
    turnover your ME site shows. Under these dynamics, individual sites
    may retain distinct populations, as seen in the within-clade PCAs
    above.

<img src="../Figures/diversity-pops-1.png" data-fig-align="center" />

Diversity is compared within clades only: reference bias changes which
sites are usable in each clade. The spectra are folded (no outgroup),
and low-coverage data shift the absolute values of Tajima’s D, so
differences between populations of the same clade are more informative
than whether D is above or below zero. Confidence intervals come from a
block bootstrap over 1 Mb blocks; populations of 8 individuals (F MD
late, S MR late) are the least precise. \### Linkage disequilibrium

LD was estimated with ngsLD within each clade on every fifth SNP (MAF ≥
0.05), for a random 2% of SNP pairs up to 100 kb apart, and averaged in
distance bins. Dashed lines show the r² expected between unlinked SNPs
from sample size alone (≈ 1/n). LD decays to near that level within a
few kb in X, S and IV, as expected for very large long-term population
sizes. The higher plateau in F reflects allele-frequency differences
between the F sub-lineages pooled in the clade, not physical linkage.

<img src="../Figures/ld-decay-1.png" data-fig-align="center" />

### Relatedness and sample quality

Relatedness was estimated with ngsRelate for every pair of individuals
within each population. KING-robust kinship does not depend on allele
frequencies, so it is less affected than *r*<sub>ab</sub> by the small
samples per population: unrelated pairs fall near or below 0,
second-degree relatives near 0.125 and first-degree relatives near 0.25.
In panel B, related pairs move towards high R1 and high KING; pairs
above 0.1 are labelled.

<img src="../Figures/relatedness-1.png" data-fig-align="center" />

In MR peak, MR_peak_1_09 belongs to the F-FH sub-lineage while the rest
are F-MR, so its kinship with every other individual is strongly
negative (the cluster near −1.1). MR_peak_2_04 and MR_peak_2_05 carry
about 15% and 22% F-N ancestry (NGSadmix, K = 6) and have elevated
kinship with each other (KING 0.20, R0 0.11). The heterozygosity check
below shows both are far more heterozygous than any other F individual
(F = −0.18 and −0.22; clade median 0.13), so their libraries most likely
contain DNA from a second individual rather than coming from admixed
relatives. On library plate 3 they sit in wells F11 and G11, directly
next to GW early individuals (F-N; wells F10 and G10). Both were
excluded from the genomic analyses within clades
(`clades.exclude_samples` in the cluster config) and are not shown here;
their phenotype data are kept. With them removed, no pair in any
population reaches second-degree kinship.

<img src="../Figures/inbreeding-1.png" data-fig-align="center" />

Per-individual inbreeding coefficients (PCAngsd, allowing for structure
within each clade) flag libraries that are more heterozygous than
expected (strongly negative F), the signature of DNA from two
individuals in one library. Individuals more than 4 robust SDs below
their clade’s median are labelled. High F mostly reflects low sequencing
depth (panel B: F falls with depth), so only unusually low values are
informative. MR_peak_2_04 and MR_peak_2_05 (F = −0.18 and −0.22) were
excluded on this basis and no longer appear; no other individual stands
out.

### Local PCA and candidate inversions

Each clade’s genome was split into windows of 2,000 SNPs, and the
population structure in each window (top two PCs of the covariance among
individuals) was compared between all windows (lostruct; Li & Ralph
2019). Windows were then placed on MDS axes: most of the genome shares
the same structure and clusters near zero; regions with distinct
structure, such as inversions, stand out as runs of windows far from
zero. Windows more than 4 robust SDs from the median are coloured.

<img src="../Figures/localpca-genome-1.png" data-fig-align="center" />

For each candidate region (at least 5 windows, mostly outliers),
individuals are placed by their average PC1 score across the region’s
windows. An inversion typically splits individuals into three groups
(the two homozygous arrangements and the heterozygotes); a region
tracking site or sub-lineage instead points to differentiation, and a
few individuals far from everyone else to a rare variant arrangement or
introgression.

<img src="../Figures/localpca-regions-1.png" data-fig-align="center" />

#### Follow-up of the candidate regions

Each candidate region (and a control region on chromosome 1 with no
local-PCA signal) was analysed on its own: a PCA of the region
(PCAngsd), each individual’s heterozygosity inside the region relative
to its genome-wide heterozygosity, and LD over distances up to 2 Mb. An
inversion shows as groups on the regional PC1 with heterozygotes raised
in heterozygosity (ratio above the bulk) and homozygotes for the rarer
arrangement lowered; suppressed recombination raises long-range LD when
both arrangements are common.

<img src="../Figures/region-followup-1.png" data-fig-align="center" />

**Chromosome 6 (X)** fits a rare chromosomal variant such as an
inversion. A minority of individuals, almost all from St. Thomas de Kent
Wharf and a few from Ritchie Wharf, sits apart on the regional PC1. Most
of them are about 5% more heterozygous on chromosome 6 than their
genome-wide average, as expected for heterozygous carriers, while the
two most extreme individuals (TK_peak_1_09, TK_peak_2_07) are less
heterozygous, as expected for homozygotes of the rare arrangement. At
this frequency (roughly 5% of chromosomes overall) LD across the whole
sample is raised only slightly (panel B: higher at short distances and
marginally at 0.1–2 Mb than on the control chromosome). **Chromosome 3
(X)** does not: individuals split by site as on the control chromosome,
with no heterozygosity classes; only two or three TK early individuals
stand out on PC2 with slightly raised heterozygosity, which could be a
smaller rare variant within the region.

### Parasites and symbionts

Reads that did not map to the *A. tonsa* reference were screened for
small-subunit rRNA (phyloFlash, SILVA 138.1). Each read pair was
assigned to the shared ancestor of all equally good database hits, at a
rank limited by its identity, and host (*Acartia*) rRNA was removed. An
organism group was called present at ≥ 5 read pairs and ≥ 2 per million
library pairs (`Scripts/04_micro_calls.R`). Parasites (*Paradinium*,
trematodes) and epibionts (apostome and peritrich ciliates) show strong
site × season patterns: apostomes in every Connecticut late-season
individual, trematodes in most Ritchie Wharf peak-season individuals,
and *Zoothamnium* on late-season Maine and St. Thomas de Kent Wharf
copepods.

<img src="../Figures/micro-prevalence-1.png" data-fig-align="center" />

<img src="../Figures/micro-loads-1.png" data-fig-align="center" />

Kinetoplastids are present at low levels in most individuals, mostly at
the northern sites, and are treated as non-pathogenic background;
crustacean (Malacostraca) DNA is likely diet.

<img src="../Figures/micro-all-groups-1.png" data-fig-align="center" />

### Seasonal allele-frequency change

Allele frequencies per population at the unlinked SNP set (one SNP per
10 kb; Section 3.2, LD) were compared between seasons within each site.
Temporal differentiation is Nei & Tajima’s F<sub>c</sub> ([Nei & Tajima
1981](https://doi.org/10.1093/genetics/98.3.625)) over SNPs with ≥ 5
individuals with data in both samples and a mean minor-allele frequency
of 0.05–0.95. Under the temporal method, F<sub>c</sub> ≈
t/(2N<sub>e</sub>) + sampling noise of both samples, so N<sub>e</sub>
follows once the sampling noise is subtracted ([Waples
1989](https://doi.org/10.1093/genetics/121.2.379); [Jorde & Ryman
2007](https://doi.org/10.1534/genetics.107.075481)).

**Calibrating sampling noise.** With \< 1× coverage, frequencies are
sampled twice, once through the individuals and once through the reads,
so an individual contributes less than two chromosomes of information
([Jónás et al. 2016](https://doi.org/10.1534/genetics.116.191197)).
Pool-seq studies handle this with an “effective number of chromosomes”
([Feder et al. 2012](https://doi.org/10.1371/journal.pone.0048588);
[Bergland et al. 2014](https://doi.org/10.1371/journal.pgen.1004775);
[Machado et al. 2021](https://doi.org/10.7554/eLife.67577)). Here it was
measured directly: each population was split at random into two halves
(two replicates), allele frequencies were estimated for each half, and
F<sub>c</sub> between the halves (pure sampling noise) was compared with
the 1/(2n) expectation. The ratio, c, is the effective number of
chromosomes per individual divided by two, and the sampling term for any
sample becomes 1/(2cn). This split-half calibration is an empirical
resampling check rather than a named published method.

<img src="../Figures/split-half-1.png" data-fig-align="center" />

Every population gives c ≈ 0.6–0.7 (about 1.2–1.4 effective chromosomes
per individual), and the two random splits agree to within 0.02. The
calibration is therefore stable and similar across clades, sites and
seasons.

<img src="../Figures/seasonal-change-1.png" data-fig-align="center" />

| Clade | Pair | Generations | SNPs | Fc | Excess (calibrated) | Ne | Ne 95% CI |
|:---|:---|---:|---:|---:|:---|:---|:---|
| F | FH peak→late | 20 | 33199 | 0.151 | 0.0019 ± 0.0014 | 5398 | 2155 – ∞ |
| F | GW early→late | 16 | 27144 | 0.138 | 0.0055 ± 0.0021 | 1450 | 838 – 5377 |
| F | GW early→peak | 8 | 27438 | 0.114 | 0.0036 ± 0.0018 | 1113 | 564 – 38734 |
| F | GW peak→late | 8 | 30102 | 0.136 | -0.0007 ± 0.0023 | ∞ | 1029 – ∞ |
| F | MD early→late | 18 | 20682 | 0.199 | 0.0107 ± 0.0019 | 838 | 620 – 1295 |
| F | MR peak→late | 18 | 24405 | 0.190 | 0.0052 ± 0.0015 | 1735 | 1118 – 3869 |
| S | MD peak→late | 8 | 24640 | 0.171 | 0.0039 ± 0.0016 | 1029 | 564 – 5888 |
| X | ME early→peak | 5 | 33373 | 0.179 | 0.0111 ± 0.0012 | 225 | 185 – 286 |
| X | RW early→late | 8 | 56735 | 0.147 | 0.0016 ± 0.0010 | 2459 | 1092 – ∞ |
| X | RW early→peak | 5 | 52812 | 0.136 | 0.0033 ± 0.0011 | 751 | 449 – 2297 |
| X | RW peak→late | 3 | 51775 | 0.139 | 0.0013 ± 0.0011 | 1176 | 434 – ∞ |
| X | TK early→peak | 4 | 46842 | 0.148 | 0.0023 ± 0.0016 | 880 | 375 – ∞ |

Temporal F<sub>c</sub> and N<sub>e</sub> per seasonal pair, with
sampling noise calibrated by the split-half c. CI from a
delete-one-chromosome jackknife of the excess; ∞ = excess not
distinguishable from zero.

| comparison |  snps |      r |    se |
|:-----------|------:|-------:|------:|
| ME vs TK   | 40454 | -0.013 | 0.006 |
| ME vs RW   | 40454 |  0.000 | 0.005 |
| TK vs RW   | 40454 | -0.002 | 0.006 |

Clade X: correlation of early→peak allele-frequency change between sites
(unlinked SNPs; SE from a delete-one-chromosome jackknife)

Calibration removes almost all of the apparent excess: with two
chromosomes per individual assumed, every pair showed an excess of ≈
0.05 regardless of the number of generations (open circles); with the
calibrated sampling term the excess is 0.000–0.011 (filled points). Most
pairs give N<sub>e</sub> in the high hundreds to a few thousand, with
lower 95% bounds of roughly 400–2,000, and several upper bounds are
infinite (no drift detectable within the season). These estimates
describe drift over a few to ~20 generations within one season, not
long-term N<sub>e</sub>, and they are at or above the lab estimate of
~400. ME early→peak is the exception (N<sub>e</sub> ≈ 225, CI ≈
185–285): ME early is the smallest X sample (10 individuals), so a
modest error in its c would inflate the excess, and the value should be
checked before it is interpreted biologically. In clade X, early→peak
changes at different sites remain uncorrelated genome-wide (r ≈ 0).

**Per-SNP seasonal test.** For each SNP, the minor-allele count was
modelled with a binomial GLM, count ~ site + x, with each population
weighted by its effective number of chromosomes (2cn), as in the
*Drosophila* seasonal studies ([Bergland et
al. 2014](https://doi.org/10.1371/journal.pgen.1004775); [Machado et
al. 2021](https://doi.org/10.7554/eLife.67577)). Two versions of x were
fitted: (1) **time**, the collection day of year scaled from 0 (first
sampling event at a site) to 1 (last), which rewards change in the same
direction through the season at every site; and (2) **temperature**, the
mean MUR SST over the generation before each collection (Ganey’s Wharf
corrected against station ET5.1), which rewards change that tracks
temperature, so that a warming and a cooling step move frequencies in
opposite directions. Temperature matters because the season is not a
monotonic temperature ramp: water before the early and peak collections
was similar (or cooler at peak in the north), and the late collections
followed a strong cooling. The null distribution comes from permuting x
among each site’s sampling events, enumerating every distinct ordering
(time: X 11 and F 23 permutations, as reversing all sites only flips the
sign; temperature: X 23 and F 47). The genome-wide test asks whether
more SNPs reach a given p value with the real values than with the
permuted ones. Results are shown with and without chromosome 6, which
carries the likely inversion in clade X (Section 3.2). Clade S has a
single seasonal site (MD) with two samples, so it has no permutation
null and is not shown.

<img src="../Figures/seasonal-glm-1.png" data-fig-align="center" />

| Clade | SNPs | Model | Chromosomes | Threshold | Observed | Permutation mean | Permutation max | Rank of observed |
|:---|:---|:---|:---|:---|---:|---:|---:|:---|
| X | all | time | all chr. | p\<0.0001 | 17 | 19.5 | 28 | 11 / 12 |
| X | all | time | all chr. | p\<0.001 | 537 | 541.6 | 583 | 7 / 12 |
| X | all | time | all chr. | p\<0.01 | 10682 | 10798.4 | 11100 | 10 / 12 |
| X | all | time | no chr6 | p\<0.0001 | 16 | 18.6 | 24 | 11 / 12 |
| X | all | time | no chr6 | p\<0.001 | 496 | 507.7 | 540 | 9 / 12 |
| X | all | time | no chr6 | p\<0.01 | 10010 | 10164.2 | 10409 | 11 / 12 |
| X | all | temperature | all chr. | p\<0.0001 | 11 | 11.9 | 17 | 15 / 24 |
| X | all | temperature | all chr. | p\<0.001 | 447 | 425.2 | 457 | 6 / 24 |
| X | all | temperature | all chr. | p\<0.01 | 10233 | 9849.9 | 10496 | 5 / 24 |
| X | all | temperature | no chr6 | p\<0.0001 | 11 | 11.4 | 16 | 15 / 24 |
| X | all | temperature | no chr6 | p\<0.001 | 423 | 399.8 | 434 | 5 / 24 |
| X | all | temperature | no chr6 | p\<0.01 | 9615 | 9263.7 | 9844 | 5 / 24 |
| X | unlinked | time | all chr. | p\<0.0001 | 0 | 0.2 | 1 | 12 / 12 |
| X | unlinked | time | all chr. | p\<0.001 | 10 | 8.0 | 12 | 5 / 12 |
| X | unlinked | time | all chr. | p\<0.01 | 142 | 173.0 | 186 | 12 / 12 |
| X | unlinked | time | no chr6 | p\<0.0001 | 0 | 0.2 | 1 | 12 / 12 |
| X | unlinked | time | no chr6 | p\<0.001 | 10 | 7.3 | 11 | 2 / 12 |
| X | unlinked | time | no chr6 | p\<0.01 | 134 | 161.2 | 172 | 12 / 12 |
| X | unlinked | temperature | all chr. | p\<0.0001 | 0 | 0.2 | 1 | 24 / 24 |
| X | unlinked | temperature | all chr. | p\<0.001 | 4 | 7.1 | 11 | 22 / 24 |
| X | unlinked | temperature | all chr. | p\<0.01 | 167 | 161.5 | 194 | 8 / 24 |
| X | unlinked | temperature | no chr6 | p\<0.0001 | 0 | 0.2 | 1 | 24 / 24 |
| X | unlinked | temperature | no chr6 | p\<0.001 | 4 | 6.4 | 10 | 22 / 24 |
| X | unlinked | temperature | no chr6 | p\<0.01 | 158 | 150.7 | 181 | 9 / 24 |
| F | all | time | all chr. | p\<0.0001 | 2 | 3.3 | 9 | 19 / 24 |
| F | all | time | all chr. | p\<0.001 | 127 | 114.2 | 137 | 3 / 24 |
| F | all | time | all chr. | p\<0.01 | 2514 | 2498.2 | 2671 | 13 / 24 |
| F | all | time | no chr6 | p\<0.0001 | 2 | 3.0 | 9 | 19 / 24 |
| F | all | time | no chr6 | p\<0.001 | 116 | 103.2 | 122 | 3 / 24 |
| F | all | time | no chr6 | p\<0.01 | 2323 | 2286.2 | 2467 | 8 / 24 |
| F | all | temperature | all chr. | p\<0.0001 | 5 | 3.8 | 8 | 16 / 48 |
| F | all | temperature | all chr. | p\<0.001 | 120 | 107.5 | 133 | 13 / 48 |
| F | all | temperature | all chr. | p\<0.01 | 2352 | 2331.4 | 2488 | 17 / 48 |
| F | all | temperature | no chr6 | p\<0.0001 | 3 | 3.6 | 7 | 35 / 48 |
| F | all | temperature | no chr6 | p\<0.001 | 110 | 97.1 | 122 | 11 / 48 |
| F | all | temperature | no chr6 | p\<0.01 | 2162 | 2129.3 | 2305 | 16 / 48 |
| F | unlinked | time | all chr. | p\<0.0001 | 0 | 0.1 | 1 | 24 / 24 |
| F | unlinked | time | all chr. | p\<0.001 | 13 | 11.0 | 15 | 6 / 24 |
| F | unlinked | time | all chr. | p\<0.01 | 204 | 204.3 | 226 | 13 / 24 |
| F | unlinked | time | no chr6 | p\<0.0001 | 0 | 0.1 | 1 | 24 / 24 |
| F | unlinked | time | no chr6 | p\<0.001 | 12 | 9.9 | 14 | 6 / 24 |
| F | unlinked | time | no chr6 | p\<0.01 | 193 | 186.2 | 211 | 8 / 24 |
| F | unlinked | temperature | all chr. | p\<0.0001 | 0 | 0.1 | 1 | 48 / 48 |
| F | unlinked | temperature | all chr. | p\<0.001 | 12 | 10.1 | 16 | 20 / 48 |
| F | unlinked | temperature | all chr. | p\<0.01 | 203 | 192.9 | 218 | 13 / 48 |
| F | unlinked | temperature | no chr6 | p\<0.0001 | 0 | 0.1 | 1 | 48 / 48 |
| F | unlinked | temperature | no chr6 | p\<0.001 | 11 | 9.1 | 16 | 17 / 48 |
| F | unlinked | temperature | no chr6 | p\<0.01 | 189 | 176.2 | 206 | 10 / 48 |

Per-SNP seasonal GLM: SNPs below each p threshold with the real seasons
vs all within-site permutations (rank 1 = more than every permutation).

There is no genome-wide excess of seasonal SNPs in either clade, with
either model. With the real seasons, X and F have about as many low-p
SNPs as the permuted orderings, at every threshold, on both the unlinked
and the full SNP sets. Modelling temperature instead of time does not
change this: the closest to a signal is clade X on all SNPs (rank 5–6 of
24 at p \< 0.01 and p \< 0.001, i.e. p ≈ 0.2–0.25), which is not
significant and does not hold on the unlinked set. Removing chromosome 6
changes nothing, so the X inversion is not driving or masking a seasonal
signal. The genomic-control λ is close to 1 (X ≈ 1.1, F ≈ 1.0) for the
observed and permuted values alike, so the calibrated binomial weights
describe the sampling variance well. Together with the uncorrelated
early→peak changes across X sites, this means that any seasonal
selection in these populations acts on too few loci, or is too weak, to
show up genome-wide with 3–4 seasonal sites per clade, whether it is
modelled as a seasonal trend or as a response to temperature. Individual
SNPs with the smallest p values are listed in
`results/13_seasonal/*.top.tsv` and `results/13_seasonal/temp/*.top.tsv`
but are not more numerous than expected by chance, so none can be called
a seasonal candidate on its own. The permutation null is coarse (time:
12 and 24 orderings; temperature: 24 and 48), which limits the smallest
attainable rank-based p value to 1/12–1/48.

### Does space substitute for time? Concordance of temperature responses

If the alleles that are more common at warmer sites are favoured by warm
conditions, they should also rise after warm periods and fall after cool
ones within sites. For every SNP, two temperature slopes were estimated
with the same binomial GLM and calibrated weights as above: a
**spatial** slope (frequency against each site’s long-term MUR SST, with
a site’s seasonal samples pooled) and a **seasonal** slope (the
temperature model above: frequency against site + temperature before
collection). Their agreement across the genome was measured as the
correlation of the two z-scores, and, among the 1% and 5% of SNPs with
the strongest spatial slopes, as the fraction whose seasonal slope has
the same sign (0.5 = no agreement). The null keeps the spatial slopes
and permutes temperatures among each site’s sampling events (as in the
seasonal test); a second, smaller null permutes site temperatures among
sites. This is the genomic counterpart of the within- vs between-site
comparison for CTmax (Section 4) and follows the seasonal–clinal
comparison of [Machado et
al. (2021)](https://doi.org/10.7554/eLife.67577).

Clade X (ME, TK, RW; one lineage, three seasonal sites) is the main
test. In clade F the warm sites (MR, FH) and the cool sites (MD, GW)
belong to different lineages, so its spatial slopes mostly reflect
lineage differences, not temperature; F is shown for completeness but
not interpreted. Clade S has one seasonal site, so its seasonal null has
a single alternative ordering.

<img src="../Figures/concordance-1.png" data-fig-align="center" />

| Clade | SNPs | Chromosomes | SNPs tested | r | Rank | Top 1% same sign | Rank | Top 5% same sign | Rank |
|:---|:---|:---|---:|---:|:---|---:|:---|---:|:---|
| X | all | all chr. | 2056496 | 0.037 | 5 / 24 | 0.549 | 6 / 24 | 0.528 | 6 / 24 |
| X | all | no chr6 | 1918570 | 0.035 | 8 / 24 | 0.548 | 6 / 24 | 0.527 | 8 / 24 |
| X | unlinked | all chr. | 42357 | 0.030 | 8 / 24 | 0.553 | 10 / 24 | 0.530 | 9 / 24 |
| X | unlinked | no chr6 | 39302 | 0.030 | 9 / 24 | 0.555 | 10 / 24 | 0.534 | 9 / 24 |
| F | all | all chr. | 470321 | -0.023 | 36 / 48 | 0.479 | 48 / 48 | 0.483 | 47 / 48 |
| F | all | no chr6 | 437032 | -0.022 | 30 / 48 | 0.484 | 48 / 48 | 0.487 | 45 / 48 |
| F | unlinked | all chr. | 38608 | -0.023 | 36 / 48 | 0.495 | 34 / 48 | 0.481 | 43 / 48 |
| F | unlinked | no chr6 | 35928 | -0.023 | 34 / 48 | 0.493 | 33 / 48 | 0.486 | 39 / 48 |
| S | all | all chr. | 273722 | 0.003 | 1 / 2 | 0.487 | 1 / 2 | 0.489 | 1 / 2 |
| S | all | no chr6 | 254773 | 0.002 | 1 / 2 | 0.492 | 1 / 2 | 0.491 | 1 / 2 |
| S | unlinked | all chr. | 26743 | 0.016 | 1 / 2 | 0.472 | 1 / 2 | 0.494 | 1 / 2 |
| S | unlinked | no chr6 | 25076 | 0.016 | 1 / 2 | 0.472 | 1 / 2 | 0.494 | 1 / 2 |

Concordance of spatial and seasonal temperature slopes. Rank = position
of the observed value among all seasonal-null orderings (1 = higher than
every permutation).

In clade X the spatial and seasonal responses point weakly in the same
direction: across all SNPs the two slopes are slightly positively
correlated (r ≈ 0.04), and among the 1% of SNPs with the strongest
spatial slopes about 55% change in the same direction through the
season. But the real temperatures rank only 5th–6th of 24 orderings (p ≈
0.2), 8th–12th on the unlinked set, and the smaller spatial null (6
orderings) cannot go below p ≈ 0.17; removing chromosome 6 changes
nothing. So there is no detectable genome-wide concordance between
spatial and seasonal temperature responses. Two limits shape this
answer. First, the spatial temperature contrast within X is short
(7.1–9.9 °C long-term mean across ME, TK and RW), and the spatial slope
is mostly ME against the two Gulf of St. Lawrence sites, so it also
carries their geographic separation. Second, the null rests on only 24
orderings. A modest true concordance, of the size seen here, could go
undetected. Clade F shows the opposite tendency (its observed
concordance is the lowest of 48 orderings for the top-1% statistic), but
because its spatial contrast is a contrast between lineages (Florida
lineages vs F-N) it is not interpreted as a temperature response. Clade
S has no usable seasonal null.

In contrast to CTmax, whose between-site slope matches the within-site
(acclimation) slope (Section 4), allele frequencies give no evidence
that the response to temperature across sites predicts the response
through the season. The genomic side of the space-for-time question
therefore remains unresolved rather than negative: the data are
consistent with no shared response and with a weak one.

## 4. Clade-specific physiological patterns

### CTmax model

To more formally test the relationships between CTmax, collection
temperature, and size, we used a linear mixed effects model, structured
as `ctmax ~ clade * temp_cent + size + (1 + temp_cent|site) + (1|tube)`.
This examines the effects of clade, temperature and size on CTmax, along
with clade-specific effects of temperature. Collection temperature was
centered before model fitting. The model also includes random intercepts
for each site and the tube number (the position of each individual
within the water bath during the CTmax assay).

### Acclimation vs. site differences (within- and between-site temperature)

A single temperature slope mixes two processes. Within a site, CTmax
changes across the season as the same population experiences warmer or
cooler water, which is mostly acclimation. Between sites, CTmax differs
with each site’s typical temperature, which reflects acclimation plus
any local adaptation. Collection temperature was therefore split into a
within-site component (collection temperature minus the site’s mean
collection temperature, so it averages zero at every site) and a
between-site component (the site’s mean temperature minus the mean
across sites) ([van de Pol & Wright
2009](https://doi.org/10.1016/j.anbehav.2008.11.006)). If the
between-site slope equals the within-site slope, site differences are
explained by acclimation to different temperatures; a steeper
between-site slope indicates local adaptation on top of plasticity, and
a shallower one countergradient variation. Clade is in the model, so
turnover of lineages along the coast does not inflate the between-site
slope. Body size and the assay terms are kept as in the CTmax model
above.

The site mean is currently the mean of each site’s collection
temperatures. Sites were not sampled in the same seasons (MR and FH only
at peak and late), so this will be replaced by the long-term MUR SST
climatology (`Output_data/site_climatology.csv`) once it is available;
the within-site component stays centred on the collection temperatures.

| term                      |  slope |    se |     lo |    hi |
|:--------------------------|-------:|------:|-------:|------:|
| Within site (acclimation) |  0.142 | 0.009 |  0.125 | 0.160 |
| Between sites             |  0.095 | 0.057 | -0.016 | 0.207 |
| Between − within          | -0.047 | 0.056 | -0.158 | 0.063 |

CTmax slopes (°C per °C) for within-site and between-site temperature,
all clades (site temperature: long-term MUR SST mean). 95% CI = ± 1.96
SE.

| Clade | Sites | Individuals | Within site       | Between sites       |
|:------|------:|------------:|:------------------|:--------------------|
| F     |     6 |         151 | 0.13 (0.11, 0.16) | 0.10 (-0.06, 0.26)  |
| IV    |     4 |          55 | 0.21 (0.14, 0.27) | -0.23 (-0.57, 0.11) |
| S     |     5 |          71 | 0.15 (0.12, 0.19) | 0.16 (-0.01, 0.33)  |
| X     |     4 |         118 | 0.09 (0.05, 0.14) | 0.26 (-0.08, 0.60)  |

Clade-specific CTmax slopes. Between-site slopes rest on few sites per
clade and are imprecise.

<img src="../Figures/within-between-plot-1.png"
data-fig-align="center" />

CTmax rises by 0.14 °C for every 1 °C of within-site temperature change
(95% CI 0.13–0.16), with clade-specific acclimation slopes of 0.09 to
0.21. The between-site slope is 0.10 (CI -0.02 to 0.21; site
temperature: long-term MUR SST mean), and the difference between the two
is -0.05 (CI -0.16 to 0.06). If that interval includes zero, CTmax
differences between sites within a clade are consistent with acclimation
to each site’s temperature, with no detectable local adaptation beyond
it; with only 2–6 sites per clade, the test can rule out only large
differences between the slopes. Panel B shows the site-level pattern
behind the between-site slope: each point is a site’s CTmax at its own
mean collection conditions.

The Stage 1 clade calls (Section 3.1) were merged with the CTmax and
body size data set to examine clade-specific patterns of divergence. By
plotting CTmax and size against the collection temperature we can see
that these clades appear to be physiologically differentiated. Clades F
and S tend to have higher CTmax, even when collected at the same
temperatures as Clades X and IV. Size is more variable, with varying
degrees of temperature sensitivity across the clades. Overall, however,
Clade F tends to be smaller than Clades S and X. Clade IV tended to be
the largest.

<img src="../Figures/clade-trait-regs-1.png" data-fig-align="center" />

<img src="../Figures/clade-ctmax-contrasts-1.png"
data-fig-align="center" />

<img src="../Figures/clade-contrast-plots-1.png"
data-fig-align="center" />

<img src="../Figures/clade-plast-contrast-1.png"
data-fig-align="center" />

<img src="../Figures/clade-trait-regs-pops-1.png"
data-fig-align="center" />

The general result here is that Clade F appears to be more thermally
tolerant than Clades S and X, which in turn are more thermally tolerant
than Clade IV. Clade IV does however, appear to exibit stronger
plasticity than the other clades (although this pattern is only
marginally significant in two of the comparisons; IV-S and IV-X)

These differences in physiology correspond to the distributional
patterns for each clade. Using a Loess smoother (span = 2) highlights
how the proportional abundance of the clades is affected by temperature
in different ways. Clade F occurs in high proportion at high
temperature, Clade IV occurs in high proportions at low temperatures,
while Clade X occurs in high proportions at intermediate temperatures.
Clade S generally occurs in low proportions, with a few exceptions.

<img src="../Figures/clade-hab-patterns-1.png"
data-fig-align="center" />

<img src="../Figures/clade-contours-1.png" data-fig-align="center" />

### Trait variability within clades

Shown below is the trait variation (ranges) for each site. Ranges are
calculated for each collection separately.

<img src="../Figures/trait-range-plot-1.png" data-fig-align="center" />

Changes in trait variance may be indicative of phenotypic selection. If
selection (as opposed to acclimation) are driving seasonal changes, we
may expect to see a reduction in variance in the peak samples relative
to the early season samples. Note that early season collection
temperatures this year were higher than expected, driven by fairly
strong heatwaves across the North Atlantic. As shown in the temperature
profiles for each site, the ‘early’ samples were collected just after
high temperatures were reached, while ‘peak’ samples were collected
after sites had experienced high temperatures for several weeks
(generations). As warming tolerances were fairly high throughout this
period, we will assume that selection was weak before the early samples.
If the early onset of high temperatures filtered out vulnerable
genotypes prior to our sampling, the results will be a conservative
estimate of the effects of selection on trait variance.

### Parasites, epibionts and thermal limits

Infected and uninfected individuals are compared within the same
collection (same site, date and collection temperature), so differences
are not confounded with acclimation temperature. Only collections with
at least two positive and two negative individuals are shown.

<img src="../Figures/infection-traits-1.png" data-fig-align="center" />

| covariate    | trait | n_collections | n_positive | effect |    se |
|:-------------|:------|--------------:|-----------:|-------:|------:|
| parasite_any | ctmax |             6 |         23 |  0.361 | 0.304 |
| parasite_any | size  |             6 |         23 |  0.009 | 0.012 |
| epibiont_any | ctmax |             4 |         13 | -0.233 | 0.312 |
| epibiont_any | size  |             4 |         13 | -0.013 | 0.023 |

Effect of carrying a parasite or epibiont on CTmax and size, within
collections (trait ~ status + clade + (1 \| collection)); only
collections with both positive and negative individuals.

## 5. Partitioning vulnerability

Our study examines spatial and temporal variation in upper thermal
limits for field-acclimated individuals. Given the short generation time
of these copepods, our results likely reflect the combined influence of
phenotypic plasticity (acclimation to the changes in temperature), local
adaptation (variation amongst the sites), and genetic variation between
the clades.

The four clades, while morphologically difficult to differentiate, have
distinct geographic distributions and physiological tolerances. The
inclusion of this clade-level variation decreases the slope of the CTmax
vs. temperature regression. Ignoring this source of variation would lead
to an overestimated capacity for *A. tonsa* to respond to warming
(either through acclimation or via gene flow and evolutionary rescue).

<img src="../Figures/clade-trait-lat-1.png" data-fig-align="center" />

We illustrate the effects of these different levels of variation by
calculating warming tolerance for each collection (site x collection
temperature combination) under three different scenarios.

### Scenario 1 (Fixed CTmax)

Warming tolerance was calculated using the average CTmax from the peak
season collections (across all sites and individuals regardless of
clade). This essentially assumes a fixed CTmax that is constant across
the geographic range examined.

### Scenario 2 (Clade-level variation)

Here, the same approach is taken but for each clade separately. The
average CTmax from the peak season for each clade was used as a fixed
CTmax value when calculating warming tolerance.

<img src="../Figures/unnamed-chunk-79-1.png" data-fig-align="center" />

### Scenario 3 (Clade variation and acclimation/adaptation)

Here CTmax is predicted based on the continuous temperature data and a
model of CTmax as a function of the interaction between collection
temperature and clade. The predicted CTmax values fall within the range
of CTmax values observed in the field.

<img src="../Figures/unnamed-chunk-80-1.png" data-fig-align="center" />

### Comparing Scenarios

There is strong geographic variation in the distributions of these
clades. It does not, for example, make sense to examine warming
tolerance of Clade X individuals in Florida, as this clade was not
collected from lower latitudes. To incorporate this distribution
information, simulated populations were re-assembled reflecting the
proportional clade composition within each region (calculated across the
entire set of collections).

<img src="../Figures/region-clade-props-1.png"
data-fig-align="center" />

Warming tolerance profiles for each clade were added in these
proportions to generation populations of 100 individuals.

The warming tolerance distributions for each region and the three
different scenarios are shown below.

<img src="../Figures/peak-season-wt-1.png" data-fig-align="center" />

Overall, differences were fairly minor, with slight increases in
predicted warming tolerance in low latitude regions and slight decreases
in higher latitude regions.

We also examined potential effects of warming (a +4°C increase in
temperature). These predictions assume clade composition does not change
over time and that warming is constant throughout the year.

Unsurprisingly warming decreases the predicted warming tolerance.
Similar patterns are observed as in the comparison between the initial
three scenarios - predicted warming tolerance under warming is lowest in
Florida and highest in the high latitude regions. Similarly, including
clade-level variation and acclimation increases warming tolerance in the
lower latitude regions and decreases it in the higher latitude regions.

<img src="../Figures/peak-season-wt-plus-warming-1.png"
data-fig-align="center" />

To simplify the comparisons, here we subset the data to just August and
September (the warmest months and the season typically associated with
the highest abundance of A. tonsa).

<img src="../Figures/wt-range-scenarios-1.png"
data-fig-align="center" />

Changes were slight between the scenarios, with the same relative
patterns between the current and warming scenarios. Compared to the
fixed CTmax estimate, incorporating both genetic diversity and
acclimation reduced the range of warming tolerance estimates across the
samples by ~30%.

| Comparison                      | WT Range | % Decrease |
|:--------------------------------|---------:|-----------:|
| Fixed CTmax                     |    13.05 |       0.00 |
| Clade                           |     9.72 |      25.53 |
| Genetic Variation & Acclimation |     8.97 |      31.26 |
