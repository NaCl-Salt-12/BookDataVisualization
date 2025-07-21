library(tidyverse)
library(tidytext)
library(pdftools)
library(ggstream)
library(ggtext)
library(png)
library(grid)
library(widyr)
library(ggraph)
library(igraph)
library(showtext)
library(sysfonts)
library(gt)
library(patchwork)
library(kableExtra)
library(knitr)

font_add(family = "Trajan Pro", regular = "TrajanPro-Regular.ttf")

font_add(family = "Times New Roman", regular = "times.ttf")

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


# V3 SEDIMENT ANALYSIS

chapter_find <- (str_detect(story_lines, "\\b\\d{1,2}\\b") | 
                              str_detect(story_lines, "prologue") | 
                              str_detect(story_lines, "epilogue"))
 
a <- 0

sentence_df <- str_flatten(story_lines, collapse = " ") %>% 
  as_tibble() %>% 
  unnest_sentences(sentence, value) %>% 
  mutate(line_number = row_number())

sentence_part <- sentence_df %>% 
  filter(str_detect(sentence, "^\\s+part")) %>% 
  mutate(part = row_number()) %>% 
  select(line_number,part)


mist_sed3 <- str_flatten(story_lines, collapse = " ") %>% 
  as_tibble() %>% 
  unnest_sentences(sentence,value) %>% 
  mutate(line_number = row_number()) %>% 
  unnest_tokens(word,sentence) %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = line_number %/% 80) %>%
  summarise(sentiment = sum(value))



# WORD CORRECLATION

mist_section <- str_flatten(story_lines, collapse = " ") %>% 
  as_tibble() %>% 
  unnest_sentences(sentence,value) %>% 
  mutate(section = row_number() %/% 10) %>% 
  unnest_tokens(word,sentence) %>% 
  filter(!word %in% stop_words$word)

word_cors<- mist_section %>% 
  group_by(word) %>% 
  filter(n() >= 20) %>% 
  pairwise_cor(word,section, sort = TRUE) %>% 
  filter(item1 %in% metals_list | item2 %in% metals_list)
  


# Metals table

metals_name<-  str_to_title(c("iron","steel","tin","pewter","zinc","brass","copper","bronze","atium","gold"))
allomatic_power <- c("Pulls on Nearby Metals","Pushes on Nearby Metals","Enhances Senses","Enhances Physical Abilities","Riots Emotions","Soothes Emotions","Hides Allomaic Pulses","Reveals Allomatic Pulses","See  other People's Futures","See Your Own Past")

all_chart <- tibble(
  METAL = metals_name,
  EFFECT = allomatic_power
)

# THEME 

theme_mistborn <- function() {
  theme_void(base_family = "Trajan Pro", base_size = 20) +
  theme(
    #plot.background = element_rect(color = NA, fill = "grey8"),
    plot.margin = margin(10, 10, 10, 10),
    legend.position = "bottom", 
    legend.direction = "horizontal",
    legend.key.height = unit(3, "mm"),
    legend.spacing.y = unit(4, "cm"),
    # legend.text = element_text(size = 9.5),
    legend.text = element_text(size = 20),
    text = element_text(color = "white"),
    plot.title = element_text(size = 24, family = "Trajan Pro"),
    plot.subtitle = element_markdown(hjust = 0.5),
    plot.caption = element_markdown(hjust = 1),
    panel.background = element_rect(fill = "#1a0d26", color = NA),
    plot.background = element_rect(fill = "#1a0d26", color = NA)
  )
    
}


theme_mist <- function() {
  theme_void(base_family = "Trajan Pro", base_size = 15) +
  theme(
    #plot.background = element_rect(color = NA, fill = "grey8"),
    plot.margin = margin(10, 10, 10, 10),
    legend.position = "bottom", 
    legend.direction = "horizontal",
    legend.key.height = unit(3, "mm"),
    legend.spacing.y = unit(4, "cm"),
    legend.text = element_text(size = 16),
    text = element_text(color = "#E6E6E6"), # Light gray for general text
    axis.text = element_text(angle = 45, color = "#B3B3B3"), # Slightly darker gray for axis text
    axis.title = element_markdown(color = "#E6E6E6"), # Light gray for axis titles
    plot.title = element_text(color = "#FFFFFF",size = 24),
    plot.subtitle = element_markdown(color = "#D9D9D9"),
    plot.caption = element_markdown(color = "#B3B3B3", hjust = 1),
    panel.grid = element_blank(),
    #axis.line = element_line(),
    panel.background = element_rect(fill = "#1a0d26", color = NA),
    plot.background = element_rect(fill = "#1a0d26", color = NA)
  )
}


theme_allomancy <- function() {
  theme_minimal() +
    theme(
      panel.background = element_rect(fill = "#1a0d26", color = NA),
      plot.background = element_rect(fill = "#1a0d26", color = NA),
      
      text = element_text(color = "#E6E6E6"), 
      axis.text = element_text(color = "#B3B3B3"), 
      axis.title = element_text(color = "#E6E6E6"),
      # axis.text = element_blank(),
      # axis.title = element_blank(),
      plot.title = element_text(color = "#FFFFFF", face = "bold"), 
      plot.subtitle = element_text(color = "#D9D9D9"), 
      plot.caption = element_text(color = "#B3B3B3"), 
      
      # Adjusting grid lines for contrast on dark background
      panel.grid.major = element_line(color = "#3C3C3C", size = 0.3), # Dark gray grid
      panel.grid.minor = element_line(color = "#2E2E2E", size = 0.15), # Darker gray minor grid
      
      # Ensuring legend blends with the dark theme
      legend.background = element_rect(fill = "#1a0d26", color = NA),
      legend.text = element_text(color = "#E6E6E6"),
      legend.title = element_text(color = "#E6E6E6"),
      
      axis.ticks = element_blank()
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

# key events

# TODO:
# story_annotations <- tibble(
#   x    = c(13.5, 1.2, 7, 5, 14, 22),
#   xend = c(19,   1.2, 7, 5, 14, 22),
#   y    = c(-5000, -4200, -4500, 5000, 5000, 3000),
#   yend = c(-5000,  -200,   400,  800, 1000,  500),
#   vjust = c(   0,  0.25,   0.3,  0.85,  0.9,  0.8),
#   label = c(
#     "Macduff & Malcolm decide to go to war against Macbeth",
#     "Three Witches<br>appear",
#     "Macbeth kills King Duncan",
#     "Lady Macbeth & Macbeth<br>plan the murder of King Duncan",
#     "Murder of Banquo reported to Macbeth,<br>Ghost of Banquo appears",
#     "Macduff<br>kills<br>Macbeth"
#     )
# )


stream_graph <- ggplot()+
  geom_vline(data=part_lines,aes(xintercept = chapter_id),color = "grey50",linewidth = .2, linetype = 2)+
  geom_stream(data = metals_text, aes(x=chapter_id,y=mentions, fill=word),type="mirror",extra_span = 0.1, bw=0.55)+
  geom_text(data = metals_text %>%  group_by(part) %>% summarize(x=min(chapter_id)+n_distinct(chapter_id)/2),
            aes(x,y= -Inf, label = part),
            vjust = -1, hjust = 0.5, color = "grey60",family = "Trajan Pro", size = 5)+
  scale_fill_manual(values = metal_colors)+
  labs(fill="Metal",
       subtitle = "Distribution of mentions share (number of words)<br> per metal in 
  each chapter. Parts are separated with vertical lines.")+
  # scale_x_continuous(n.breaks = 20)
  theme_mistborn()

stream_graph
## Character mentions streamgraph:
# 
# ggplot()+
#   geom_vline(data=part_lines,aes(xintercept = chapter_id),color = "grey50",linewidth = .2, linetype = 2)+
#   geom_stream(data = character_men, aes(x=chapter_id,y=mentions, fill=word),type="mirror")+
#   geom_text(data = character_men %>%  group_by(part) %>% summarize(x=min(chapter_id)+n_distinct(chapter_id)/2),
#             aes(x,y= -Inf, label = part),
#             vjust = -1, hjust = 0.5, color = "grey60",family = "Forum", size = 2)+
#   scale_fill_manual(values = mistborn_colors)+
#   labs(fill="Characters")+
#   theme_mistborn()
# 

# Metal mention barplot

plot_data <- metals_text %>% 
  group_by(word) %>% 
  summarise(total_mentions = sum(mentions)) %>% 
  arrange(desc(total_mentions)) %>% 
  mutate(word = fct_reorder(word, total_mentions))

bar_plot <- plot_data %>% 
  ggplot(aes(x = word, y = total_mentions, fill = word)) +
  geom_col() +
  scale_fill_manual(values = metal_colors) +
  guides(fill = "none") +
  labs(y = "Number of Mentions") +
  coord_cartesian(clip = 'off')+
  geom_text(aes(label = total_mentions), 
            color = "grey70",
            hjust = 0.5,
            vjust = -0.4,
            # size = 4.5,
            size = 7.5,
            family = "Trajan Pro") +
  # expand_limits(x = max(plot_data$total_mentions) * 1.1) +
  theme_mist() +
  theme(
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 18,angle = 90),
  )

bar_plot
## == Sentiment Score plot ==

# TODO: workon theme
# ggplot(mistborn_sed,aes(x=chapter_id,y=sentiment_score))+
#   geom_vline(data=part_lines,aes(xintercept = chapter_id),color = "grey50",linewidth = .2, linetype = 2)+
#   geom_text(data = metals_text %>%  group_by(part) %>% summarize(x=min(chapter_id)+n_distinct(chapter_id)/2),
#             aes(x,y= -Inf, label = part),
#             vjust = -1, hjust = 0.5, color = "grey60",family = "Forum", size = 2)+
#   geom_line(color="lightblue")+
#   theme_mistborn()
# 
# ggplot(mist_sed3,aes(x=index,y=sentiment))+
#   geom_smooth(se = F, color = "lightblue")+
#   theme_allomancy()+
#   theme(axis.text.x = element_blank(),
#         axis.title.x = element_blank())
#   
  
## == Network Graph == 3
 
 set.seed(2000)

 graph <- word_cors %>% 
   filter(correlation > 0.15) %>%
   graph_from_data_frame()
 
 V(graph)$degree <- degree(graph)
 # Add a node attribute to indicate if the node is in metals_list
 V(graph)$is_metal <- V(graph)$name %in% metals_list
 
 # Create the plot with conditional highlighting and dot colors
network_Graph <- graph %>%
   ggraph(layout = "fr") +
   geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation), 
                  show.legend = FALSE, color = "grey40") +
   geom_node_point(aes(size = degree, color = is_metal)) +
   scale_color_manual(values = c("FALSE" = "#ff79c6", "TRUE" = "lightblue")) +  # Different colors for metal/non-metal
   geom_node_label(aes(label = name),
                  color = "grey95",
                  fill = "#4D267380",
                  repel = TRUE,
                  force = 1,
                  force_pull = 0.1,
                  # min.segment.length = 1,
                  family = "Trajan Pro",
                  label.size = 0,
                  size = 3.5) +
   scale_size_continuous(range = c(3, 10)) + 
   guides(size ="none",color="none")+
   theme_mistborn()
 
network_Graph
 ## == Allomancy Reference ==
 
ref_table <- gt(all_chart) %>% 
  # tab_header(
    # title = "ALLOMANCY REFERENCE CHART"
  # ) %>% 
  opt_table_font(font = "Times New Roman",color = "#E6E6E6") %>% 
  tab_style(
    style = cell_text(
      align="center",
      size = px(20)),
    locations = cells_column_labels()
  ) %>% 
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom", "left", "right"),
      color = "transparent",  # Remove all borders from data cells
      weight = px(0)
    ),
    locations = cells_body()
  ) %>% 
  tab_options(
    table.border.top.color = "transparent",    # Remove top table border
    table.border.bottom.color = "transparent", # Remove bottom table border
    # data_row.padding = px(5),                  # Optional: Adjust padding for better spacing
    column_labels.border.top.color = "transparent",
    # table.background.color = "transparent",
    table.background.color = "#1a0d26",
  ) %>% 
  tab_style(
    style = cell_borders(
      sides = "bottom",        # Target only the bottom border
      color = "transparent",   # Make it invisible
      weight = px(0)
    ),
    locations = cells_title(groups = c("title", "subtitle"))
  ) %>% 
  tab_style(
    style = cell_text(
      font = "Trajan Pro",        # Set font family for title
      size = px(16),         # Set font size for title
    ),
    locations = cells_title(groups = "title")  # Target title
  ) %>% 
  tab_options(
    data_row.padding.horizontal = px(20),       # Padding for data cells
    column_labels.padding.horizontal = px(20)   # Padding for column labels
  ) 

# ref_table + plot_annotation(
#   # title = "Metals In Mistborn",
#   # caption = "made by Nathaniel Clark",
#   theme = 
#     theme(
#       plot.title = element_textbox(family = "Trajan Pro", size = 30, hjust = 0.5,color = "white"),
#       plot.background = element_rect(fill = "#1a0d26", color = NA)
#     ),
# )
ref_table
# wrap_table(ref_table,space = "fixed") + stream_graph + bar_plot + network_Graph


text_1 <- "In the In the book Mistborn: Final Empire the magic system is based around individuals, known as Allomancers, who gain supernatural abilities by ingesting and \"burning\" specific metals. Each metal grants a unique power, such as enhanced strength, heightened senses, or emotional manipulation. Allomancers are either Mistings, capable of burning one metal, or Mistborn, who can burn all metals"

print(all_chart)

ggsave("stream_mist.png",plot = stream_graph)

ggsave("bar_mist.png",plot = bar_plot)

ggsave("network_mist.png",plot = network_Graph)

gtsave(ref_table,"ref_table_mist2.png")
