
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





# play a game -------------------------------------------------------------

# vancouver recordings
vancouver_birds <- query_xenocanto(species = 'lat: "49.26" lon: "-123.11" q:"A" type: "song"')

vancouver_birds |>
  pull(species) |>
  unique()

# select 2 recordings from each species
set.seed(123)
vancouver_birds_selected <- vancouver_birds |>
  group_by(species) |>
  slice_sample(n = 2) |>
  ungroup()


# download it (saved to "presentation slides/audio_game")
audio_game_dir <- here("presentation slides", "audio_game")
dir.create(audio_game_dir, showWarnings = FALSE)

download_media(metadata = vancouver_birds_selected,
               path = audio_game_dir)


# Start of the game:

# Player A (one person)
i <- 4 # anynumber between 1:nrow(vancouver_birds)

# Player B (group)

# construct the file name
audio_path <- paste0(str_replace_all(vancouver_birds_selected[i,]$species, " ", "_"),
                   "-XC", vancouver_birds_selected[i,]$key, ".", vancouver_birds_selected[i,]$file_extension)

# read in the audio
ext <- tolower(tools::file_ext(audio_path))

if (ext == "mp3") {
  audio <- readMP3(here("presentation slides", "audio_game", audio_path))
} else if (ext == "wav") {
  audio <- readWave(here("presentation slides", "audio_game", audio_path))
}


# play
audio |>
  play()

# look
audio |>
  extractWave(from = 0, to = 10, xunit = "time") |>
  spectro(flim = c(1, 10),
          fastdisp = TRUE)

# answer by BirdNET
model <- load_birdnet()

predictions <- predict(model,
                       files = here("presentation slides", "audio_game", audio_path),
                       min_confidence = 0.3)

predictions_df <- predictions |>
  as.data.frame()

# did you get this one!


