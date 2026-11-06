# Build the HiPIC-to-BFI-2 mapping table for the deck's interactive slide.
# Run once in the HiPIC session (it needs HIPIC_CROSSWALK and REVERSE_ITEMS),
# then re-run whenever you change a rating in hipic_bfi2_map.csv.
#
# Inputs (in the deck folder):
#   hipic_bfi2_map.csv  - one row per HiPIC item: HIPIC_Q, MATCH, BFI2_Q ("34;19" for several)
#   bfi2_items.csv      - the 60 BFI-2 items: BFI2_Q, BFI2_ITEM, BFI2_DOMAIN, BFI2_REV
# Output:
#   map_hipic_bfi2.rds  - the finished table, read by the deck

library(dplyr)

MAP_RAW <- read.csv("hipic_bfi2_map.csv", stringsAsFactors = FALSE, colClasses = c(BFI2_Q = "character"))
BFI2    <- read.csv("bfi2_items.csv",     stringsAsFactors = FALSE)

# 1. One row per HiPIC-BFI-2 pair (an item with "34;19" becomes two rows)
PAIRS <- MAP_RAW %>%
  tidyr::separate_rows(BFI2_Q, sep = ";") %>%
  mutate(BFI2_Q = suppressWarnings(as.integer(BFI2_Q))) %>%
  left_join(BFI2, by = "BFI2_Q")

# 2. Back to one row per HiPIC item, with its BFI-2 matches stacked in one cell
MAP <- PAIRS %>%
  group_by(HIPIC_Q, MATCH) %>%
  summarise(
    BFI2   = if (all(is.na(BFI2_Q))) "—" else
               paste0(BFI2_Q, " ", BFI2_ITEM, ifelse(BFI2_REV == "Y", " (R)", ""), collapse = "<br>"),
    DOMAIN = if (all(is.na(BFI2_Q))) "—" else paste(unique(BFI2_DOMAIN), collapse = "<br>"),
    .groups = "drop") %>%
  left_join(HIPIC_CROSSWALK %>% select(Q, CHILD_Q_WORDING), by = c("HIPIC_Q" = "Q")) %>%
  mutate(HIPIC_ITEM = paste0(CHILD_Q_WORDING, ifelse(HIPIC_Q %in% REVERSE_ITEMS, " (R)", "")),
         MATCH      = factor(MATCH, levels = c("Equivalent", "Similar", "Related", "None"))) %>%
  arrange(HIPIC_Q) %>%
  select(`HiPIC #`                 = HIPIC_Q,
         `HiPIC Item (Child form)` = HIPIC_ITEM,
         `Closest BFI-2 Item(s)`   = BFI2,
         `BFI-2 Domain`            = DOMAIN,
         Match                     = MATCH)

# 3. Checks: 144 rows, and the tally by rating
nrow(MAP)
table(MAP$Match)

saveRDS(MAP, "map_hipic_bfi2.rds")
