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

# Function to standardize character names (similar to original Office code)
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

# Process text for character mentions (adapting your structure)
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
  filter(lengths(characters_mentioned) >= 1)

top_character_names <- character_mentions %>% 
  unnest(characters_mentioned) %>% 
  count(characters_mentioned, sort = TRUE) %>% 
  slice_max(order_by = n, n = 10) %>% 
  arrange(-n) %>% 
  pull(characters_mentioned)

character_mentions %>% 
  unnest(characters_mentioned) %>% 
  filter(characters_mentioned %in% top_character_names) %>% 
  count(characters_mentioned, sort = TRUE)

# Step 1: Create co-occurrence matrix
# Find paragraphs where characters appear together
character_cooccurrence <- character_mentions %>% 
  filter(lengths(characters_mentioned) >= 2) %>%  # Only paragraphs with 2+ characters
  unnest(characters_mentioned) %>% 
  filter(characters_mentioned %in% top_character_names) %>% 
  group_by(chapter_id, paragraph_id) %>% 
  summarize(characters = list(characters_mentioned), .groups = "drop") %>% 
  filter(lengths(characters) >= 2)

# Create all pairwise combinations within each paragraph
create_pairs <- function(chars) {
  if(length(chars) < 2) return(NULL)
  combinations <- combn(chars, 2, simplify = FALSE)
  map_dfr(combinations, ~data.frame(from = .x[1], to = .x[2]))
}

# Generate co-occurrence pairs
cooccurrence_pairs <- character_cooccurrence %>% 
  rowwise() %>% 
  do(create_pairs(.$characters)) %>% 
  ungroup() %>% 
  count(from, to, sort = TRUE) %>% 
  rename(weight = n)

# Step 2: Create symmetric matrix for chord diagram
# Make the relationship symmetric (add reverse pairs)
symmetric_pairs <- cooccurrence_pairs %>% 
  bind_rows(
    cooccurrence_pairs %>% 
      rename(from = to, to = from)
  ) %>% 
  group_by(from, to) %>% 
  summarize(weight = sum(weight), .groups = "drop")

# Convert to matrix format
chord_matrix <- symmetric_pairs %>% 
  pivot_wider(names_from = to, values_from = weight, values_fill = 0) %>% 
  column_to_rownames("from") %>% 
  as.matrix()

# Ensure matrix is square (add missing characters as empty rows/columns)
all_chars <- unique(c(rownames(chord_matrix), colnames(chord_matrix)))
for(char in all_chars) {
  if(!char %in% rownames(chord_matrix)) {
    chord_matrix <- rbind(chord_matrix, setNames(rep(0, ncol(chord_matrix)), colnames(chord_matrix)))
    rownames(chord_matrix)[nrow(chord_matrix)] <- char
  }
  if(!char %in% colnames(chord_matrix)) {
    chord_matrix <- cbind(chord_matrix, setNames(rep(0, nrow(chord_matrix)), rownames(chord_matrix)))
    colnames(chord_matrix)[ncol(chord_matrix)] <- char
  }
}

# Reorder to match
chord_matrix <- chord_matrix[all_chars, all_chars]


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

# Create and save the chord diagram
ragg::agg_png(here("mistborn-character-mentions.png"),
              res = 500, width = 8, height = 8, units = "in", bg = "#1a0d26")
par(
  family = "Helvetica", cex = 2, col = "white", # Changed font to Helvetica
  bg = "#1a0d26", 
  mai = rep(0.5, 4)
) 
chordDiagram(chord_matrix, transparency = 0.3, 
             grid.col = mistborn_palette, 
             link.border = "white", link.lwd = 0.2,
             annotationTrack = c("name", "grid"),
             annotationTrackHeight = c(0.03, 0.05), # Replaced mm_h with proportions
             link.largest.ontop = TRUE
)
title(
  main = "Who interacts with whom Mistborn?",
  sub = "Top 10 characters in Mistborn\nSource: Mistborn text analysis. Visualisation: Your Name",
  col.main = "white", cex.main = 1.3
)
invisible(dev.off())

# Display in R Markdown (optional)
knitr::include_graphics(here("mistborn-character-mentions.png"))
