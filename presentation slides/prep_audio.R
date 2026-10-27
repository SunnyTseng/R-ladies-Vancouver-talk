
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
model <- load_birdnet()

audio_path <- "C:/Users/sunny/Desktop/R ladies Vancouver talk/presentation slides/audio/Turdus_migratorius-XC465001.mp3"

predictions <- predict(model,
                       files = audio_path,
                       min_confidence = 0.3)

# who!?
as.data.frame(predictions) |>
  mutate(common_name = str_split_i(species_name, "_", 2)) |>
  select(start_time, end_time, common_name, confidence)




