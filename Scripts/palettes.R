# Project colour palettes (latitude gradient, chosen 2026-10-24). Source from every script.
# One 12-step gradient runs from warm (south) to cool (north). The 8 sites and the
# 4 clades are interleaved along it, so each clade sits next to the sites where it
# is most common: F with Florida, S with the Chesapeake, IV with CT/ME, X with the
# Gulf of St. Lawrence.
# Rule: one variable owns colour in a figure. Cross-clade figures colour by clade;
# within-clade figures may colour by site. Never colour by both in one figure,
# because each clade is close to its neighbouring sites. The other variable goes
# to shape, facets or labels. Use the same hex values in the cluster Python scripts.

lat_gradient <- c("MR" = "#8D1A2C",   #  1
                  "F"  = "#B73534",   #  2  clade
                  "FH" = "#D85B3A",   #  3
                  "MD" = "#EB8A3A",   #  4
                  "S"  = "#EBAC4E",   #  5  clade
                  "GW" = "#DBC270",   #  6
                  "CT" = "#A2C4AA",   #  7
                  "IV" = "#6CB1B4",   #  8  clade
                  "ME" = "#2A9D8F",   #  9
                  "TK" = "#4A62AE",   # 10
                  "X"  = "#24518B",   # 11  clade
                  "RW" = "#192E63")   # 12

clade_cols <- c(lat_gradient[c("X", "F", "S", "IV")],
                "A_hudsonica" = "#B9A7B4")   # not on the gradient: a different species
clade_shapes <- c("X" = 17, "F" = 15, "S" = 16, "IV" = 18, "A_hudsonica" = 3)   # pair with colour

# Sites, south -> north (latitude order)
site_order <- c("MR", "FH", "MD", "GW", "CT", "ME", "TK", "RW")
pop_cols <- lat_gradient[site_order]
pop_cols <- c("KL" = "#5E0F1F", pop_cols)    # KL: south of MR, not sequenced

site_names <- c("MR" = "Manatee River", "FH" = "Ft. Hamer", "MD" = "Tyler Cove",
                "GW" = "Ganey's Wharf", "CT" = "Esker Point", "ME" = "Sawyer Park",
                "TK" = "St. Thomas de Kent Wharf", "RW" = "Ritchie Wharf")
site_cols <- setNames(pop_cols[names(site_names)], site_names)   # same colours, keyed by full name

# Regions take the colour of one of their sites
region_order <- c("Florida", "Chesapeake", "Connecticut", "Maine", "Shediac", "Miramichi")
region_cols <- setNames(lat_gradient[c("MR", "MD", "CT", "ME", "TK", "RW")], region_order)

season_order <- c("early", "peak", "late")

# Text on a site- or clade-coloured fill: dark text on the light middle of the gradient
fill_text_col <- function(hex) {
  rgb <- grDevices::col2rgb(hex) / 255
  ifelse(colSums(rgb * c(0.299, 0.587, 0.114)) > 0.6, "#2B2B29", "#FFFFFF")
}
site_text_col <- function(site) fill_text_col(pop_cols[site])
