
# library -----------------------------------------------------------------

library(tidyverse)
library(here)

library(suwo)
library(tuneR)
library(seewave)

library(seewave)
library(birdnetR)
library(birdnetTools)






# retrieving audio from xeno-canto ----------------------------------------

# query sounds from Xeno-Canto
amro <- query_xenocanto(species = "Turdus migratorius")

amro_canada <- query_xenocanto(species = 'sp: "Turdus migratorius" cnt:"canada"')

amro_vancouver <- query_xenocanto(species = 'sp: "Turdus migratorius" lat: "49.26" lon: "-123.11"')

# take a look
amro_vancouver |>
  head()

amro_vancouver[8,]

# download it (saved to "presentation slides/audio")
audio_dir <- here("presentation slides", "audio")
dir.create(audio_dir, showWarnings = FALSE)

set.seed(123)
download_media(metadata = amro_vancouver[8,],
               path = audio_dir)

# listen to it
file <- tibble(path = list.files(audio_dir, pattern = "\\.mp3$", full.names = TRUE)) |>
  mutate(modified = file.mtime(path)) |>
  slice_max(modified, n = 1) |>   # newest download
  pull(path)

play(readMP3(file))


# look at it
readMP3(file) |>
  mono() |>
  extractWave(from = 2, to = 11, xunit = "time") |>
  spectro(wl = 512, ovlp = 75, flim = c(0, 6),
          scale = FALSE,
          fastdisp = TRUE)


# check who is singing
birdnetR::





# prep_audio.R ------------------------------------------------------------
# Pre-compute everything the "Listen to some birds" slides need, so that
# rendering slides.qmd never touches the network or Python.
#
#   1. Search Xeno-Canto (API v3) for a few local species
#   2. Download the MP3s
#   3. Draw a spectrogram PNG for each clip
#   4. Run BirdNET (with and without a Vancouver / late-October species list)
#   5. Save tidy results to data/*.rds
#
# Requirements
#   - A Xeno-Canto API key (https://xeno-canto.org/account) stored in
#     .Renviron as XC_API_KEY=your-key   (usethis::edit_r_environ())
#   - birdnetR (installs its own Python backend on first use)
#
# Run from the project root or from "presentation slides/":
#   source("presentation slides/prep_audio.R")

library(httr2)
library(dplyr)
library(tidyr)
library(purrr)
library(tuneR)
library(seewave)
library(birdnetR)
library(birdnetTools)

# Paths --------------------------------------------------------------------

slides_dir <- if (dir.exists("presentation slides")) "presentation slides" else "."
audio_dir  <- file.path(slides_dir, "audio")
img_dir    <- file.path(slides_dir, "images", "spectrograms")
data_dir   <- file.path(slides_dir, "data")
walk(c(audio_dir, img_dir, data_dir), dir.create, showWarnings = FALSE, recursive = TRUE)

# Settings -----------------------------------------------------------------

# Rainy-day Vancouver birds + one for Halloween
species <- c(
  "Varied Thrush",
  "Pacific Wren",
  "Spotted Towhee",
  "Song Sparrow",
  "Barred Owl"
)

n_per_species <- 1          # recordings to keep per species
vancouver     <- list(lat = 49.28, lon = -123.12)
talk_week     <- 43L        # week of year for late October

xc_key <- Sys.getenv("XC_API_KEY")
if (xc_key == "") {
  stop("XC_API_KEY is not set. Add it to .Renviron and restart R.", call. = FALSE)
}

# 1. Search Xeno-Canto -----------------------------------------------------
# Note: warbleR::query_xc() used API v2, which Xeno-Canto has retired.

xc_search <- function(query, key = xc_key) {
  resp <- request("https://xeno-canto.org/api/3/recordings") |>
    req_url_query(query = query, key = key) |>
    req_retry(max_tries = 3) |>
    req_perform() |>
    resp_body_json(simplifyVector = TRUE)

  if (resp$numRecordings == 0) return(tibble())
  as_tibble(resp$recordings)
}

# Prefer short, top-quality (A) recordings from Canada; relax if none found
find_recordings <- function(en, n = n_per_species) {
  queries <- c(
    sprintf('en:"%s" q:A len:10-40 cnt:canada', en),
    sprintf('en:"%s" q:A len:10-40', en),
    sprintf('en:"%s" q:A', en)
  )
  for (q in queries) {
    hits <- xc_search(q)
    if (nrow(hits) > 0) return(slice_head(hits, n = n))
  }
  warning("No recordings found for ", en, call. = FALSE)
  tibble()
}

xc_meta <- map(species, find_recordings) |>
  list_rbind() |>
  transmute(
    xc_id           = paste0("XC", id),
    scientific_name = paste(gen, sp),
    common_name     = en,
    type,
    recordist       = rec,
    country         = cnt,
    location        = loc,
    date,
    length,
    licence         = paste0("https:", sub("^https?:", "", lic)),
    page_url        = paste0("https:", sub("^https?:", "", url)),
    download_url    = file,
    mp3             = file.path(audio_dir, paste0(xc_id, ".mp3"))
  )

# 2. Download --------------------------------------------------------------

walk2(xc_meta$download_url, xc_meta$mp3, \(url, dest) {
  if (!file.exists(dest)) {
    message("Downloading ", basename(dest))
    request(url) |> req_retry(max_tries = 3) |> req_perform(path = dest)
  }
})

# 3. Spectrograms ----------------------------------------------------------

draw_spectrogram <- function(mp3, title) {
  wav <- readMP3(mp3)
  if (wav@stereo) wav <- mono(wav, which = "both")
  # Drop wind / traffic rumble below 1 kHz so the birds stand out
  wav <- ffilter(wav, from = 1000, output = "Wave")
  png_file <- file.path(img_dir, sub("\\.mp3$", ".png", basename(mp3)))

  png(png_file, width = 1800, height = 600, res = 150)
  spectro(
    wav,
    flim      = c(0, 10),         # most songbird energy sits below 10 kHz
    wl        = 512,
    ovlp      = 75,
    collevels = seq(-40, 0, 1),
    palette   = reverse.gray.colors.2,
    scale     = FALSE,
    main      = title
  )
  dev.off()
  png_file
}

xc_meta$spectrogram <- map2_chr(xc_meta$mp3, xc_meta$xc_id, draw_spectrogram)

# 4. BirdNET ---------------------------------------------------------------

acoustic <- load_birdnet(type = "acoustic")
geo      <- load_birdnet(type = "geo")

# Species expected in Vancouver in late October, per the BirdNET geo model
local_species <- predict(
  geo,
  latitude  = vancouver$lat,
  longitude = vancouver$lon,
  week      = talk_week
) |>
  as.data.frame()

tidy_birdnet <- function(pred) {
  as.data.frame(pred) |>
    as_tibble() |>
    separate_wider_delim(
      species_name, delim = "_",
      names = c("scientific_name", "common_name")
    ) |>
    mutate(xc_id = sub("\\.mp3$", "", basename(input)), .before = 1) |>
    select(-input)
}

birdnet_all <- predict(acoustic, files = xc_meta$mp3, min_confidence = 0.1) |>
  tidy_birdnet()

birdnet_local <- predict(
  acoustic,
  files          = xc_meta$mp3,
  min_confidence = 0.1,
  species_list   = local_species$species_name
) |>
  tidy_birdnet()

# 5. Summarise: did BirdNET get it right? ----------------------------------

summarise_top <- function(detections) {
  detections |>
    # birdnetTools detects the common_name / confidence columns
    birdnet_filter(threshold = 0.5) |>
    group_by(xc_id, common_name) |>
    summarise(
      max_conf   = max(confidence),
      n_segments = n(),
      .groups    = "drop"
    ) |>
    group_by(xc_id) |>
    slice_max(max_conf, n = 1, with_ties = FALSE) |>
    ungroup() |>
    rename(birdnet_says = common_name)
}

scorecard <- xc_meta |>
  select(xc_id, xeno_canto_says = common_name, recordist) |>
  left_join(summarise_top(birdnet_all), by = "xc_id") |>
  mutate(
    birdnet_says = coalesce(birdnet_says, "(nothing above 0.5)"),
    correct      = birdnet_says == xeno_canto_says
  )

# Save ---------------------------------------------------------------------

saveRDS(xc_meta,       file.path(data_dir, "xc_meta.rds"))
saveRDS(local_species, file.path(data_dir, "local_species.rds"))
saveRDS(birdnet_all,   file.path(data_dir, "birdnet_all.rds"))
saveRDS(birdnet_local, file.path(data_dir, "birdnet_local.rds"))
saveRDS(scorecard,     file.path(data_dir, "scorecard.rds"))

print(scorecard)
message("Done. Pick your favourite clips, then render slides.qmd.")
