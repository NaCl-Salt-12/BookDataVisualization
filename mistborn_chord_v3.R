library(tidyverse)      # Data manipulation and visualization
library(ggtext)         # Text formatting in ggplot2
library(tidytext)       # Text mining tools
library(here)           # File path management
library(circlize)       # Circular visualization
library(pdftools)       # For reading PDFs

# Your existing PDF processing code
mistborn_text <- pdf_text("./Mistborn/final_empire.pdf") 
text_lines <- unlist(str_split(mistborn_text, "\n")) %>% 
  str_replace_all("mistborn\\s+\\d+","") %>% 
  str_replace_all("\\d+\\s+brandon sanderson", "") %>% 
  str_replace_all("[^[:alnum:][:space:]\\.]", " ") 

prologue_line <- which(str_detect(text_lines, "PROLOGUE$"))
ars_arcanum_line <- which(str_detect(text_lines, "ARS ARCANUM$"))

# Extract the story text
story_lines <- text_lines[prologue_line:(ars_arcanum_line - 1)]
mistborn_text <- str_c(story_lines,collapse = "\n")

mistborn_chapters <- mistborn_text %>% str_split("\\b\\d{1,2}\\b|PROLOGUE|EPILOGUE") %>% .[[1]] %>% .[-1]
chapters <- tibble(
  chapter_id = 1:40,
  text = mistborn_chapters
)

# Your character and metals lists (expanded)
characters <- c("vin","kelsier","elend","breeze","spook","marsh","sazed","hammond","clubs","ham", 
                "kell","dox","valette","survivor","lestibournes","oreseur","tensoon","yeden",
                "straff","jastes","preservation","ruin")
metals_list <- c("iron","steel","tin","pewter","brass","zinc","copper","bronze","atium","gold")
allomancy <- c("steelpush","ironpull","tin-enhanced","pewter-enhanced","soothe","riot")

# === CHARACTER MENTION ANALYSIS ===
# Function to standardize character names
standardize_character_names <- function(text) {
  text %>% 
    str_to_lower() %>% 
    # Handle common variations
    str_replace_all("\\bkell\\b", "kelsier") %>% 
    str_replace_all("\\bhammond\\b", "ham") %>% 
    str_replace_all("\\bdox\\b", "dockson") %>% 
    str_replace_all("\\bvalette\\b", "vin") %>% 
    str_replace_all("\\bsurvivor\\b", "kelsier") %>% 
    str_replace_all("\\blestibournes\\b", "spook")
}

# Process text for character mentions
mistborn_processed <- chapters %>% 
  mutate(text = standardize_character_names(text)) %>% 
  unnest_tokens(paragraph, text, token = "paragraphs") %>% 
  mutate(paragraph_id = row_number())

# Find character mentions in paragraphs
character_mentions <- mistborn_processed %>% 
  crossing(character = characters) %>% 
  filter(str_detect(paragraph, paste0("\\b", character, "\\b"))) %>% 
  select(chapter_id, paragraph_id, character) %>% 
  group_by(chapter_id, paragraph_id) %>% 
  summarize(
    characters_mentioned = list(unique(str_to_title(character))),
    .groups = "drop"
  ) %>% 
  filter(lengths(characters_mentioned) >= 2)  # Only keep paragraphs with 2+ characters

# Get top characters for the chord diagram
top_character_names <- character_mentions %>% 
  unnest(characters_mentioned) %>% 
  count(characters_mentioned, sort = TRUE) %>% 
  slice_max(order_by = n, n = 8) %>%  # Limit to top 8 for readability
  pull(characters_mentioned)

# === CREATE CO-OCCURRENCE MATRIX FOR CHORD DIAGRAM ===
# Function to create character co-occurrence pairs
create_cooccurrence_pairs <- function(character_list) {
  if (length(character_list) < 2) return(NULL)
  
  # Create all possible pairs
  pairs <- combn(character_list, 2, simplify = FALSE)
  
  # Convert to data frame
  map_dfr(pairs, ~ tibble(
    char1 = .x[1],
    char2 = .x[2]
  ))
}

# Generate co-occurrence data
cooccurrence_data <- character_mentions %>% 
  filter(map_lgl(characters_mentioned, ~ any(.x %in% top_character_names))) %>% 
  mutate(
    characters_mentioned = map(characters_mentioned, ~ .x[.x %in% top_character_names])
  ) %>% 
  filter(lengths(characters_mentioned) >= 2) %>% 
  mutate(pairs = map(characters_mentioned, create_cooccurrence_pairs)) %>% 
  unnest(pairs) %>% 
  select(char1, char2)

# Create symmetric co-occurrence matrix
cooccurrence_matrix <- cooccurrence_data %>% 
  count(char1, char2, name = "weight") %>% 
  # Make symmetric by adding reverse pairs
  bind_rows(
    cooccurrence_data %>% 
      count(char2, char1, name = "weight") %>% 
      rename(char1 = char2, char2 = char1)
  ) %>% 
  group_by(char1, char2) %>% 
  summarize(weight = sum(weight), .groups = "drop") %>% 
  # Remove self-loops and ensure we have all characters
  filter(char1 != char2) %>% 
  complete(char1 = top_character_names, char2 = top_character_names, fill = list(weight = 0)) %>% 
  filter(char1 != char2)

# Convert to matrix format for circlize
chord_matrix <- cooccurrence_matrix %>% 
  pivot_wider(names_from = char2, values_from = weight, values_fill = 0) %>% 
  column_to_rownames("char1") %>% 
  as.matrix()

# Ensure matrix is square and symmetric
all_chars <- union(rownames(chord_matrix), colnames(chord_matrix))
chord_matrix <- chord_matrix[all_chars, all_chars]
chord_matrix[is.na(chord_matrix)] <- 0

# Make matrix symmetric (take average of upper and lower triangles)
chord_matrix <- (chord_matrix + t(chord_matrix)) / 2

# === CREATE CHORD DIAGRAM ===
# Set up colors for characters


mistborn_palette <- c(
  Vin = "#4682B4",
  Kelsier = "#D4A017",
  Sazed = "#8B5A2B",
  Ham = "#355E3B",
  Breeze = "#9B59B6",
  Elend = "#4E8C8A",
  Marsh = "#800000",
  Yeden = "#6B8E23",
  Clubs = "#4A4A4A",
  Spook = "#E67E22"
)


# Create the chord diagram
pdf("mistborn_character_chord_diagram.pdf", width = 10, height = 10)
par(mar = c(1, 1, 1, 1))

chordDiagram(
  chord_matrix,
  grid.col = mistborn_palette,
  transparency = 0.3,
  annotationTrack = "grid",
  preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(chord_matrix)))))
)

# Add character names
circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)

# Add title
title("Mistborn Character Co-occurrence Network", cex.main = 1.5)

dev.off()

# Also create a version for display
circos.clear()
chordDiagram(
  chord_matrix,
  grid.col = mistborn_palette,
  transparency = 0.3,
  annotationTrack = "grid",
  preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(chord_matrix)))))
)

circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)

title("Mistborn Character Co-occurrence Network", cex.main = 1.5)

# Print summary statistics
cat("Character co-occurrence summary:\n")
print(cooccurrence_matrix %>% 
        group_by(char1) %>% 
        summarize(total_connections = sum(weight), .groups = "drop") %>% 
        arrange(desc(total_connections)))

# Clear circos parameters
circos.clear()