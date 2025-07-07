library(tidyverse)
library(tidytext)
library(pdftools)
library(stringr)
library(ggstream)
library(ggtext)
library(extrafont)
library(paletteer)
library(png)
library(grid)


info <- pdf_fonts("./Mistborn/final_empire.pdf") 

mistborn_text <- pdf_text("./Mistborn/final_empire.pdf") 
  
text_lines <- unlist(strsplit(mistborn_text, "\n")) %>% 
# paste(collapse = '\n') %>% 
 str_replace_all("mistborn\\s+\\d+","") %>% 
 str_replace_all("\\d+\\s+brandon sanderson", "") %>% 
 str_replace_all("\n", " ") %>%
 str_replace_all("[^[:alnum:][:space:]\\.]", " ")

 
 prologue_line <- which(str_detect(text_lines, "PROLOGUE$"))
 ars_arcanum_line <- which(str_detect(text_lines, "ARS ARCANUM$"))
 
 # Extract the story text
   story_lines <- text_lines[prologue_line:(ars_arcanum_line - 1)]
   
   # Identify chapter markers
   chapter_markers <- which(str_detect(story_lines, "\\b\\d{1,2}\\b") | 
                              str_detect(story_lines, "PROLOGUE$") | 
                              str_detect(story_lines, "EPILOGUE$"))
   
   # Extract chapter names
   chapter_names <- story_lines[chapter_markers]
   # Assign chapters to each line
   line_chapters <- findInterval(seq_along(story_lines), chapter_markers)
   chapter_labels <- chapter_names[line_chapters]
   
   chapter_id <- tibble(
     chapter_name = str_trim(chapter_names),
     chapter_id = seq(1,40),
   )
   
   # Create a tidy data frame
   df <- tibble(
     chapter = str_trim(chapter_labels),
     text = story_lines,
   ) %>%
     group_by(chapter) %>%
     mutate(line_number = row_number()) %>%
     ungroup() %>% 
     inner_join(chapter_id, join_by(chapter==chapter_name))
   
   
   
text_df <- df %>% 
  unnest_tokens(word, text)


## TODO: later utilize regex and allomancy terms to increase data points


part_lines <- df %>%
  select(-line_number) %>% 
  filter(str_detect(text, "^\\s+PART")) %>% 
  mutate(text = str_trim(text)) %>% 
  rename(
    part = text
  ) %>% 
  select(chapter_id,part)

characters <- c("vin","kelsier","elend","breeze","spook","marsh","sazed","hammond","clubs","ham", "kell","dox","valette","survivor","lestibournes")
metals_list <- c("iron","steel","tin","pewter","brass","zinc","copper","bronze","atium","gold")
metal_levels <- c("pewter", "atium","tin","steel","iron","copper","bronze","gold","zinc","brass")
metals_list <- c("iron","steel","tin","pewter","brass","zinc","copper","bronze","atium","gold")
allomancy <- c("steelpush","ironpull","tin-enhanced","pewter-enhanced","soothe","riot")

metals_text <- text_df %>%
  filter(word %in% metals_list) %>% 
  mutate(word = str_to_title(word)) %>% 
  select(-line_number) %>% 
  count(chapter_id, word, name="mentions") %>% 
   left_join(part_lines, join_by(chapter_id==chapter_id)) %>% 
  fill(part)
  
character_men <- text_df %>% 
  mutate(word = str_to_lower(word)) %>% 
  filter(word %in% characters) %>% 
  mutate(word = case_when(
    word %in% c("hammond","ham") ~ "ham",
    word %in% c("vin","valette") ~ "vin",
    word %in% c("kelsier","kell","survivor") ~ "kelsier",
    word %in% c("lestibournes","spook") ~ "spook",
    word %in% c("dockson","dox") ~ "dockson",
    TRUE ~ word,
  )) %>% 
  ungroup() %>% 
  mutate(word = str_to_title(word)) %>% 
  select(-line_number) %>% 
  count(chapter_id, word, name="mentions") %>% 
   left_join(part_lines, join_by(chapter_id==chapter_id)) %>% 
  fill(part)

# == SEDIMENT ANALYSIS ==

mistborn_sed <- text_df %>% 
  select(chapter_id,word) %>% 
  inner_join(get_sentiments("afinn")) %>%
  group_by(chapter_id) %>% 
  summarize(sentiment_score = sum(value))

# V2 SEDIMENT ANALYSIS

mist_sed <- text_df %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = line_number %/% 3) %>%
  summarise(sentiment = sum(value))


# THEME 

theme_mistborn <- function() {
  theme_void(base_family = "Forum", base_size = 10) +
  theme(
    #plot.background = element_rect(color = NA, fill = "grey8"),
    plot.margin = margin(10, 10, 10, 10),
    legend.position = "bottom", 
    legend.direction = "horizontal",
    legend.key.height = unit(3, "mm"),
    legend.spacing.y = unit(4, "cm"),
    legend.text = element_text(size = 9.5),
    text = element_text(color = "white"),
    plot.title = element_text(size = 24, family = "Forum"),
    plot.subtitle = element_markdown(),
    plot.caption = element_markdown(hjust = 1),
    panel.background = element_rect(fill = "#1a0d26", color = NA),
    plot.background = element_rect(fill = "#1a0d26", color = NA)
  )
    
}


theme_mist <- function() {
  theme_void(base_family = "Forum", base_size = 10) +
  theme(
    #plot.background = element_rect(color = NA, fill = "grey8"),
    plot.margin = margin(10, 10, 10, 10),
    legend.position = "bottom", 
    legend.direction = "horizontal",
    legend.key.height = unit(3, "mm"),
    legend.spacing.y = unit(4, "cm"),
    legend.text = element_text(size = 9.5),
    text = element_text(color = "white"),
    plot.title = element_text(size = 24, family = "Forum"),
    plot.subtitle = element_markdown(),
    plot.caption = element_markdown(hjust = 1),
    panel.grid = element_blank(),
    axis.title = element_markdown(),
    #axis.line = element_line(),
    panel.background = element_rect(fill = "#1a0d26", color = NA),
    plot.background = element_rect(fill = "#1a0d26", color = NA)
  )
}

# COLOR PALETTES:

metal_colors <- c(
  "Iron"   = "#708090",  
  "Steel"  = "#4682B4", 
  "Tin"    = "#C0C0C0",
  "Pewter" = "#96A0A0",
  "Brass"  = "#B5A642",
  "Zinc"   = "#7F8C8D",
  "Copper" = "#B87333",
  "Bronze" = "#CD7F32",
  "Atium"  = "purple4",
  "Gold"   = "goldenrod"
)

mistborn_colors <- c(
  "Vin" = "#6BA6CD",        
  "Kelsier" = "#E55555",    
  "Elend" = "#413f8f",     
  "Breeze" = "#E6B800",   
  "Spook" = "#B565A7",   
  "Marsh" = "#39A8B2",  
  "Sazed" = "#E67E22", 
  "Clubs" = "#A5A5A5",
  "Ham" = "#B8860B"  
)

# PLOTS: 


## Metals stream graph:


ggplot()+
  geom_vline(data=part_lines,aes(xintercept = chapter_id),color = "grey50",linewidth = .2, linetype = 2)+
  geom_stream(data = metals_text, aes(x=chapter_id,y=mentions, fill=word),type="mirror")+
  geom_text(data = metals_text %>%  group_by(part) %>% summarize(x=min(chapter_id)+n_distinct(chapter_id)/2),
            aes(x,y= -Inf, label = part),
            vjust = -1, hjust = 0.5, color = "grey60",family = "Forum", size = 2)+
  scale_fill_manual(values = metal_colors)+
  labs(fill="Metal")+
  theme_mistborn()


## Character mentions streamgraph:

ggplot()+
  geom_vline(data=part_lines,aes(xintercept = chapter_id),color = "grey50",linewidth = .2, linetype = 2)+
  geom_stream(data = character_men, aes(x=chapter_id,y=mentions, fill=word),type="mirror")+
  geom_text(data = character_men %>%  group_by(part) %>% summarize(x=min(chapter_id)+n_distinct(chapter_id)/2),
            aes(x,y= -Inf, label = part),
            vjust = -1, hjust = 0.5, color = "grey60",family = "Forum", size = 2)+
  scale_fill_manual(values = mistborn_colors)+
  labs(fill="Characters")+
  theme_mistborn()


# Metal mention barplot

plot_data <- metals_text %>% 
  group_by(word) %>% 
  summarise(total_mentions = sum(mentions)) %>% 
  arrange(desc(total_mentions)) %>% 
  mutate(word = fct_reorder(word, total_mentions))

plot_data %>% 
  ggplot(aes(y = word, x = total_mentions, fill = word)) +
  geom_col() +
  scale_fill_manual(values = metal_colors) +
  guides(fill = "none") +
  labs(x = "Number of Mentions") +
  geom_text(aes(label = total_mentions), 
            color = "white",
            hjust = -0.2,
            size = 3.5) +
  expand_limits(x = max(plot_data$total_mentions) * 1.1) +
  theme_mistborn() +
  theme(
    axis.text.y = element_markdown(),
    axis.title.x = element_markdown()
  )


## == Sentiment Score plot ==

# TODO: workon theme
ggplot(mistborn_sed,aes(x=chapter_id,y=sentiment_score))+
  geom_vline(data=part_lines,aes(xintercept = chapter_id),color = "grey50",linewidth = .2, linetype = 2)+
  geom_text(data = metals_text %>%  group_by(part) %>% summarize(x=min(chapter_id)+n_distinct(chapter_id)/2),
            aes(x,y= -Inf, label = part),
            vjust = -1, hjust = 0.5, color = "grey60",family = "Forum", size = 2)+
  geom_line(color="lightblue")+
  theme_mistborn()
  
 ggplot(mist_sed,aes(x=index,y=sentiment))+
  # geom_vline(data=part_lines,aes(xintercept = chapter_id),color = "grey50",linewidth = .2, linetype = 2)+
  # geom_text(data = metals_text %>%  group_by(part) %>% summarize(x=min(chapter_id)+n_distinct(chapter_id)/2),
            # aes(x,y= -Inf, label = part),
            # vjust = -1, hjust = 0.5, color = "grey60",family = "Forum", size = 2)+
  geom_col(fill="lightblue")+
  theme_mistborn()
  

