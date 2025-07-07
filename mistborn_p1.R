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


