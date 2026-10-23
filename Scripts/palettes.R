# Project colour palettes (option 2, chosen 2026-10-22). Source from every script:
#   source("R/palettes.R")
# Rule: one variable owns colour in a figure. Cross-clade figures colour by clade;
# within-clade figures may colour by site; the other variable goes to shape,
# facets or labels. Identical hex values are used in the cluster Python scripts.

clade_cols <- c("X"           = "#81BCC5",
                "F"           = "#EE5E59",
                "S"           = "#E89B1F",   # darkened from #FFB647 so small points show on white
                "IV"          = "#5076A5",
                "A_hudsonica" = "#DDBBCA")
clade_shapes <- c("X" = 17, "F" = 15, "S" = 16, "IV" = 18, "A_hudsonica" = 3)   # pair with colour

# Sites: neutral light-to-dark ramp, south -> north (latitude order)
site_order <- c("MR", "FH", "MD", "GW", "CT", "ME", "TK", "RW")
pop_cols <- setNames(c("#D9D9D9", "#C3C3C3", "#A5A5A5", "#888888",
                       "#6C6C6C", "#515151", "#2E2E2E", "#0E0E0E"), site_order)
pop_cols <- c("KL" = "#EDEDED", pop_cols)    # KL: not sequenced

site_names <- c("MR" = "Manatee River", "FH" = "Ft. Hamer", "MD" = "Tyler Cove",
                "GW" = "Ganey's Wharf", "CT" = "Esker Point", "ME" = "Sawyer Park",
                "TK" = "St. Thomas de Kent Wharf", "RW" = "Ritchie Wharf")
site_cols <- setNames(pop_cols[names(site_names)], site_names)   # same ramp, keyed by full name

region_order <- c("Florida", "Chesapeake", "Connecticut", "Maine", "Shediac", "Miramichi")
region_cols <- setNames(c("#D9D9D9", "#A5A5A5", "#7A7A7A", "#515151", "#2E2E2E", "#0E0E0E"),
                        region_order)

season_order <- c("early", "peak", "late")

# Text on a site-coloured fill: dark on light greys, white on dark greys
site_text_col <- function(site) ifelse(match(site, site_order) <= 4, "#2B2B29", "#FFFFFF")
