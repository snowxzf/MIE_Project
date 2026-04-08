# =============================================================================
# Build data_mie286.R from export/trial_metrics_summary.csv
# -----------------------------------------------------------------------------
# Parses messy duration strings from filenames, deduplicates to one row per
#   (participant, mode) by keeping the trial with lowest area_off_px2 (best trace).
# Pivots to wide paired columns (___numerical / ___spatial-color), merges optional
# participant_demographics.csv, writes named vectors for analysis scripts.
# Do not edit data_mie286.R by hand — regenerate with:
#   Rscript build_mie286_vectors.R  (wd = code/)
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

# Filenames vary a lot: 1min09.33, 1min,55 (comma), 2min28 (no sep), _35s_, _43.28s_, 01.16.71, etc.
parse_duration_sec <- function(filename) {
  vapply(as.character(filename), FUN.VALUE = numeric(1), USE.NAMES = FALSE, function(s) {
    if (is.na(s) || !nzchar(s)) return(NA_real_)
    s <- sub('^"|"$', "", s) # strip CSV quotes

    # minutes: optional comma/dot after "min"
    mmin <- regmatches(
      s,
      regexec("(\\d+)\\s*min\\s*[,\\s]*(\\d+)(?:[.,](\\d+))?", s, ignore.case = TRUE, perl = TRUE)
    )[[1]]
    if (length(mmin) >= 4 && nzchar(mmin[1]) && !is.na(suppressWarnings(as.numeric(mmin[3])))) {
      base <- as.numeric(mmin[2]) * 60 + as.numeric(mmin[3])
      if (length(mmin) >= 4 && !is.na(mmin[4]) && nzchar(mmin[4])) {
        base <- base + as.numeric(paste0("0.", mmin[4]))
      }
      return(base)
    }

    # whole minutes only: e.g. ricky_spatial_1min_3rd -> 60 s
    m_end <- regmatches(s, regexec("(\\d+)min_", s, perl = TRUE))[[1]]
    if (length(m_end) >= 2 && nzchar(m_end[1])) {
      return(as.numeric(m_end[2]) * 60)
    }

    # plain seconds: _43s_ or _43.28s_ or trailing _40s / 40s (no underscore after s)
    for (pat in c("_([\\d.,]+)s_", "_([\\d.,]+)s$")) {
      ms <- regmatches(s, regexec(pat, s, perl = TRUE))[[1]]
      if (length(ms) >= 2 && nzchar(ms[1])) {
        return(as.numeric(sub(",", ".", ms[2], fixed = TRUE)))
      }
    }

    three <- regmatches(s, regexec("(\\d+)\\.(\\d{1,2})\\.(\\d{1,2})", s, perl = TRUE))[[1]]
    if (length(three) == 4) {
      mins <- as.numeric(three[2])
      secwhole <- as.numeric(three[3])
      centi <- as.numeric(three[4])
      return(mins * 60 + secwhole + centi / 100)
    }

    twos <- gregexpr("(\\d{1,3})\\.(\\d{1,3})", s, perl = TRUE)[[1]]
    if (twos[1] != -1) {
      frag <- regmatches(s, gregexpr("(\\d{1,3})\\.(\\d{1,3})", s, perl = TRUE))[[1]]
      pick <- frag[length(frag)]
      cap <- regmatches(pick, regexec("(\\d{1,3})\\.(\\d{1,3})", pick, perl = TRUE))[[1]]
      if (length(cap) == 3) return(as.numeric(paste(cap[2], cap[3], sep = ".")))
    }
    NA_real_
  })
}

raw <- read.csv("export/trial_metrics_summary.csv", stringsAsFactors = FALSE, check.names = FALSE)
trial <- raw %>%
  mutate(
    duration_sec = parse_duration_sec(.data$filename),
    mode = trimws(as.character(.data$mode)),
    participant = trimws(as.character(.data$participant)),
    area_off_px2 = as.numeric(.data$area_off_px2)
  )
trial_dedup <- trial %>%
  filter(!is.na(.data$area_off_px2), !is.na(.data$duration_sec), nzchar(.data$participant)) %>%
  group_by(.data$participant, .data$mode) %>%
  # If multiple CSV rows per person×mode, keep single "best" trial (lowest area error).
  slice_min(order_by = .data$area_off_px2, n = 1, with_ties = FALSE) %>%
  ungroup()
active <- trial_dedup %>% filter(.data$mode %in% c("numerical", "spatial-color"))
paired <- active %>%
  select(participant, mode, duration_sec, area_off_px2) %>%
  pivot_wider(
    names_from = mode,
    values_from = c(duration_sec, area_off_px2),
    names_sep = "___"
  )
paired_complete <- paired %>%
  filter(
    !is.na(.data[["duration_sec___numerical"]]) & !is.na(.data[["duration_sec___spatial-color"]]),
    !is.na(.data[["area_off_px2___numerical"]]) & !is.na(.data[["area_off_px2___spatial-color"]])
  )

# Optional covariates: must key on participant matching trial CSV names.
demo_path <- "participant_demographics.csv"
if (file.exists(demo_path)) {
  demo <- read.csv(demo_path, stringsAsFactors = FALSE, check.names = FALSE, encoding = "UTF-8")
  names(demo) <- trimws(names(demo))
  demo$participant <- trimws(as.character(demo$participant))
  if (!"gender" %in% names(demo)) demo$gender <- NA_character_
  if (!"avg_gaming_hours_per_day" %in% names(demo)) {
    demo$avg_gaming_hours_per_day <- if ("avg_gaming_times_per_week" %in% names(demo)) {
      suppressWarnings(as.numeric(demo$avg_gaming_times_per_week))
    } else {
      NA_real_
    }
  } else {
    demo$avg_gaming_hours_per_day <- suppressWarnings(as.numeric(demo$avg_gaming_hours_per_day))
  }
  demo <- demo %>%
    mutate(gender = dplyr::na_if(trimws(as.character(.data$gender)), "")) %>%
    distinct(.data$participant, .keep_all = TRUE)
  paired_complete <- paired_complete %>% dplyr::left_join(demo, by = "participant")
}
if (!"gender" %in% names(paired_complete)) paired_complete$gender <- NA_character_
if (!"avg_gaming_hours_per_day" %in% names(paired_complete)) {
  paired_complete$avg_gaming_hours_per_day <- suppressWarnings(NA_real_)
}
paired_complete <- paired_complete %>%
  mutate(
    gender = dplyr::na_if(trimws(as.character(.data$gender)), ""),
    avg_gaming_hours_per_day = suppressWarnings(as.numeric(.data$avg_gaming_hours_per_day))
  )

# Anyone with all 3 trial types saved but still missing from paired_complete?
triple <- trial %>%
  filter(.data$mode %in% c("no-feedback", "numerical", "spatial-color")) %>%
  distinct(.data$participant, .data$mode) %>%
  count(.data$participant, name = "n_modes") %>%
  filter(.data$n_modes >= 2L)
paired_ids <- paired_complete$participant
miss <- setdiff(triple$participant, paired_ids)
if (length(miss)) {
  message("Participants with 3 modes in CSV but NOT in paired set (check duration NA / dedup):")
  detail <- trial %>%
    filter(.data$participant %in% miss, .data$mode %in% c("numerical", "spatial-color")) %>%
    transmute(
      participant = .data$participant,
      mode = .data$mode,
      sec = .data$duration_sec,
      file = .data$filename
    )
  print(utils::head(as.data.frame(detail), 50L))
}

out <- file("data_mie286.R", open = "wt", encoding = "UTF-8")
cat(
  "# Auto-generated by build_mie286_vectors.R — do not edit by hand.\n",
  "# Regenerate: Rscript build_mie286_vectors.R (wd = code/)\n",
  "# Vectors share one element per participant; index i aligns across all names below.\n",
  "# Names: participant, duration_sec_numerical, duration_sec_spatial_color,\n",
  "#   area_off_px2_numerical, area_off_px2_spatial_color, gender, avg_gaming_hours_per_day\n\n",
  file = out,
  sep = ""
)
cat(file = out, "participant <- ", sep = "")
dput(paired_complete$participant, file = out)
cat(file = out, "\nduration_sec_numerical <- ", sep = "")
dput(paired_complete[["duration_sec___numerical"]], file = out)
cat(file = out, "\nduration_sec_spatial_color <- ", sep = "")
dput(paired_complete[["duration_sec___spatial-color"]], file = out)
cat(file = out, "\narea_off_px2_numerical <- ", sep = "")
dput(paired_complete[["area_off_px2___numerical"]], file = out)
cat(file = out, "\narea_off_px2_spatial_color <- ", sep = "")
dput(paired_complete[["area_off_px2___spatial-color"]], file = out)
cat(file = out, "\ngender <- ", sep = "")
dput(paired_complete[["gender"]], file = out)
cat(file = out, "\navg_gaming_hours_per_day <- ", sep = "")
dput(paired_complete[["avg_gaming_hours_per_day"]], file = out)
close(out)

message("Wrote data_mie286.R (n = ", nrow(paired_complete), " paired participants)")

