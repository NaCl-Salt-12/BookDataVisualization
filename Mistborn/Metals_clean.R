library(tidyverse) 
library(tidytext) 
library(pdftools) 
library(ggstream) 
library(ggtext) 
library(widyr)
library(ggraph)
library(igraph) 
library(sysfonts) 
library(gt)


## === READ AND CLEAN TEXT ===

# Read in the text

mistborn_text <- pdf_text("./Mistborn/final_empire.pdf") 
  
info <- pdf_fonts("./Mistborn/final_empire.pdf") 

# clean text
text_lines <- unlist(strsplit(mistborn_text, "\n")) %>% # I broke the text into rows based on \n 
 str_replace_all("mistborn\\s+\\d+","") %>% # some pages had the heading mistborn (page number) so i removed that 
 str_replace_all("\\d+\\s+brandon sanderson", "") %>% # other pages had (page number) brandon sanderson 
 str_replace_all("\n", " ")  %>% # removed all \ns 
 str_replace_all("[^[:alnum:][:space:]\\.]", " ")

 
prologue_line <- which(str_detect(text_lines, "PROLOGUE$")) # Start location
ars_arcanum_line <- which(str_detect(text_lines, "ARS ARCANUM$")) # End location 
 
# Extract the story text
story_lines <- text_lines[prologue_line:(ars_arcanum_line - 1)]  
   
# Identify chapter markers
chapter_markers <- which(str_detect(story_lines, "\\b\\d{1,2}\\b") | 
                              str_detect(story_lines, "PROLOGUE$") | 
                              str_detect(story_lines, "EPILOGUE$"))
   
# Extract chapter names
chapter_names <- story_lines[chapter_markers]

# Assign chapters to each line
line_chapters <- findInterval(seq_along(story_lines), chapter_markers) # the chapter id for each line 

chapter_labels <- chapter_names[line_chapters] # the chapter name for each line
   
# Match Chapter name with ID 
chapter_id <- tibble(
  chapter_name = str_trim(chapter_names),
  chapter_id = seq(1,40), # the chapter id will become important latter for graphing
  )
   
# Create a tidy data frame
df <- tibble( # A data frame with the text broken into chapters with chapter name
  chapter = str_trim(chapter_labels),
  text = story_lines,
) %>%
 group_by(chapter) %>%
 mutate(line_number = row_number()) %>%  # each chapters lines are counted starting from one
 ungroup() %>% 
 inner_join(chapter_id, join_by(chapter==chapter_name)) # add back in chapter id 

 
# broke in to parts for graphing later
part_lines <- df %>%
  select(-line_number) %>% 
  filter(str_detect(text, "^\\s+PART")) %>% 
  mutate(text = str_trim(text)) %>% 
  rename(
    part = text
  ) %>% 
  select(chapter_id,part)
  

# break the text into individual words 
text_df <- df %>% 
  unnest_tokens(word, text)



# Create lists of metals for future 
metals_list <- c("iron","steel","tin","pewter","brass","zinc","copper","bronze","atium","gold")
metal_levels <- c("pewter", "atium","tin","steel","iron","copper","bronze","gold","zinc","brass")


# == METAL COUNTS == 

# a tibble that counts the number of mentions of each metal per chapter
metals_text <- text_df %>%
  filter(word %in% metals_list) %>% 
  mutate(word = str_to_title(word)) %>% 
  select(-line_number) %>% 
  count(chapter_id, word, name="mentions") %>% 
   left_join(part_lines, join_by(chapter_id==chapter_id)) %>% 
  fill(part)

# == WORD CORRECLATION ==

mist_section <- str_flatten(story_lines, collapse = " ") %>%  # break the text into one blob
  as_tibble() %>% 
  unnest_sentences(sentence,value) %>% # break into sentences
  mutate(section = row_number() %/% 10) %>% # Create sections made of ten sentences
  unnest_tokens(word,sentence) %>% # break into words 
  filter(!word %in% stop_words$word) # make sure none of the words are common words

word_cors<- mist_section %>% 
  group_by(word) %>% 
  filter(n() >= 20) %>% # only include words that appear 20 or more times
  pairwise_cor(word,section, sort = TRUE) %>% # create a pairwise correlation for all words
  filter(item1 %in% metals_list) # filter for only words that correlate with metals 
 

# == Metals table == 

metals_name<-  str_to_title(c("iron","steel","tin","pewter","zinc","brass","copper","bronze","atium","gold"))
allomatic_power <- c("Pulls on Nearby Metals","Pushes on Nearby Metals","Enhances Senses","Enhances Physical Abilities","Riots Emotions","Soothes Emotions","Hides Allomaic Pulses","Reveals Allomatic Pulses","See  other People's Futures","See Your Own Past")

all_chart <- tibble(
  METAL = metals_name,
  EFFECT = allomatic_power
)

# === THEME === 

# = Add fonts = 
font_add(family = "Trajan Pro", regular = "TrajanPro-Regular.ttf") 

font_add(family = "Times New Roman", regular = "times.ttf")


# First custom theme
theme_mistborn <- function() {
  theme_void(base_family = "Trajan Pro", base_size = 20) +
  theme(
    plot.margin = margin(10, 10, 10, 10),
    legend.position = "bottom", 
    legend.direction = "horizontal",
    legend.key.height = unit(3, "mm"),
    legend.spacing.y = unit(4, "cm"),
    legend.text = element_text(size = 20),
    text = element_text(color = "white"),
    plot.title = element_text(size = 24, family = "Trajan Pro"),
    plot.subtitle = element_markdown(hjust = 0.5),
    plot.caption = element_markdown(hjust = 1),
    panel.background = element_rect(fill = "#1a0d26", color = NA),
    plot.background = element_rect(fill = "#1a0d26", color = NA)
  )
    
}

# Second Theme
theme_mist <- function() {
  theme_void(base_family = "Trajan Pro", base_size = 15) +
  theme(
    plot.margin = margin(10, 10, 10, 10),
    legend.position = "bottom", 
    legend.direction = "horizontal",
    legend.key.height = unit(3, "mm"),
    legend.spacing.y = unit(4, "cm"),
    legend.text = element_text(size = 16),
    text = element_text(color = "#E6E6E6"), 
    axis.text = element_text(angle = 45, color = "#B3B3B3"), 
    axis.title = element_markdown(color = "#E6E6E6"), 
    plot.title = element_text(color = "#FFFFFF",size = 24),
    plot.subtitle = element_markdown(color = "#D9D9D9"),
    plot.caption = element_markdown(color = "#B3B3B3", hjust = 1),
    panel.grid = element_blank(),
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

# === PLOTS ===: 


# == METALS STREAM GRAPH: == 


stream_graph <- ggplot() +
  # Add part divider lines
  geom_vline(
    data = part_lines,
    aes(xintercept = chapter_id),
    color = "grey50",
    linewidth = 0.2,
    linetype = 2
  ) +
  
  # Streamgraph layer
  geom_stream(
    data = metals_text,
    aes(x = chapter_id, y = mentions, fill = word),
    type = "mirror",
    extra_span = 0.1,
    bw = 0.55
  ) +
  
  # Add part labels at the bottom of the plot
  geom_text(
    data = metals_text %>%
      group_by(part) %>%
      summarize(x = min(chapter_id) + n_distinct(chapter_id) / 2),
    aes(x, y = -Inf, label = part),
    vjust = -1,
    hjust = 0.5,
    color = "grey60",
    family = "Trajan Pro",
    size = 5
  ) +
  
  # Custom fill scale for metals
  scale_fill_manual(values = metal_colors) +
  
  # Plot labels
  labs(
    fill = "Metal",
    subtitle = "Distribution of mentions share (number of words)<br>
    per metal in each chapter. Parts are separated with vertical lines."
  ) +
  
  # Apply custom theme
  theme_mistborn()

stream_graph


# == METAL MENTIONS BARPLOT: == 

# Summarize metal mention counts and reorder factors
plot_data <- metals_text %>% 
  group_by(word) %>% 
  summarise(total_mentions = sum(mentions)) %>% 
  arrange(desc(total_mentions)) %>% 
  mutate(word = fct_reorder(word, total_mentions))

# Create horizontal bar plot
bar_plot <- plot_data %>% 
  ggplot(aes(x = word, y = total_mentions, fill = word)) +
  
  # Bar geometry
  geom_col() +
  
  # Custom fill colors
  scale_fill_manual(values = metal_colors) +
  
  # Remove legend
  guides(fill = "none") +
  
  # Labels
  labs(y = "Number of Mentions") +
  
  # Allow labels to extend past plot area
  coord_cartesian(clip = "off") +
  
  # Text labels above bars
  geom_text(
    aes(label = total_mentions), 
    color = "grey70",
    hjust = 0.5,
    vjust = -0.4,
    size = 7.5,
    family = "Trajan Pro"
  ) +
  
  # Apply custom theme
  theme_mist() +
  
  # Additional theme tweaks
  theme(
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 18, angle = 90)
  )

bar_plot

# == NETWORK GRAPH == 
 
# Set seed for reproducible layout
set.seed(2000)

# Filter word correlations and create graph
graph <- word_cors %>% 
  filter(correlation > 0.15) %>%
  graph_from_data_frame()

# Compute node degrees and tag metals
V(graph)$degree <- degree(graph)
V(graph)$is_metal <- V(graph)$name %in% metals_list  # Boolean for coloring nodes

# Create network graph with conditional styling
network_Graph <- graph %>%
  ggraph(layout = "fr") +
  
  # Draw edges with thickness and transparency based on correlation
  geom_edge_link(
    aes(edge_alpha = correlation, edge_width = correlation), 
    color = "grey40",
    show.legend = FALSE
  ) +
  
  # Draw nodes with size by degree and color by is_metal
  geom_node_point(
    aes(size = degree, color = is_metal)
  ) +
  
  # Custom colors for metal vs non-metal nodes
  scale_color_manual(
    values = c("FALSE" = "#ff79c6", "TRUE" = "lightblue")
  ) +
  
  # Add labels with soft background
  geom_node_label(
    aes(label = name),
    color = "grey95",
    fill = "#4D267380",
    repel = TRUE,
    force = 1,
    force_pull = 0.1,
    family = "Trajan Pro",
    label.size = 0,
    size = 3.5
  ) +
  
  # Control node size range
  scale_size_continuous(range = c(3, 10)) +
  
  # Remove legends
  guides(size = "none", color = "none") +
  
  # Apply custom theme
  theme_mistborn()



# == ALLOMANCY REFERENCE TABLE ==
 
ref_table <- gt(all_chart) %>% 
  
  # Set font and text color
  opt_table_font(
    font = "Times New Roman",
    color = "#E6E6E6"
  ) %>%
  
  # Style column headers
  tab_style(
    style = cell_text(
      align = "center",
      size = px(20)
    ),
    locations = cells_column_labels()
  ) %>%
  
  # Remove borders from body cells
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom", "left", "right"),
      color = "transparent",
      weight = px(0)
    ),
    locations = cells_body()
  ) %>%
  
  # General table styling
  tab_options(
    table.border.top.color = "transparent",
    table.border.bottom.color = "transparent",
    column_labels.border.top.color = "transparent",
    table.background.color = "#1a0d26"
  ) %>%
  
  # Remove bottom borders from title and subtitle
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "transparent",
      weight = px(0)
    ),
    locations = cells_title(groups = c("title", "subtitle"))
  ) %>%
  
  # Style the main title
  tab_style(
    style = cell_text(
      font = "Trajan Pro",
      size = px(16)
    ),
    locations = cells_title(groups = "title")
  ) %>%
  
  # Add horizontal padding to rows and column labels
  tab_options(
    data_row.padding.horizontal = px(20),
    column_labels.padding.horizontal = px(20)
  )


# == SAVE THE PLOTS == 

ggsave("stream_mist.png",plot = stream_graph)
ggsave("bar_mist.png",plot = bar_plot)
ggsave("network_mist.png",plot = network_Graph)
gtsave(ref_table,"ref_table_mist2.png")
