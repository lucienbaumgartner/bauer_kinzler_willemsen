library(dplyr)
library(jsonlite)
library(stringr)
library(purrr)

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

####### FUNCTIONS
### Add newline to write to connection
out <- function(input) return(cat(input, "\n"))

### Create survey header (only has to be done once)
survey_header <- function(ADVANCED = TRUE){
  return(out(ifelse(ADVANCED, "[[AdvancedFormat]]")))
}

### Create a new block
create_block <- function(BLOCKNAME) return(cat(paste0("[[Block:", BLOCKNAME,"]]"), "\n"))

### Create a question item within a block
create_item <- function(QTYPE = "MC", SINGLEANSWER = FALSE, LAYOUT = "Vertical"){
  return(out(paste0("[[Question:", QTYPE, ":", ifelse(SINGLEANSWER, "SingleAnswer:", ""), LAYOUT, "]]")))
}

### Generate question item ID
item_id <- function(ID) return(out(paste0("[[ID:",ID, "]]")))

### Create answer options
create_answer_options <- function(ANSWER_OPTIONS){
  out_vec <- unlist(sapply(ANSWER_OPTIONS, function(option) c(option, "\n")))
  return(out(c("[[Choices]]\n", out_vec)))
}

### Create text element
create_text <- function(TEXT, bold = FALSE){
  if(bold) TEXT <- paste0("<b>", TEXT, "</b>") # make it bold
  return(cat(TEXT, "\n"))
}

### Page break
page_break <- function() return(cat("[[PageBreak]]\n"))

### Helpers for IDs (from https://stackoverflow.com/questions/7659891/r-make-unique-starting-in-1)
dotify <- function(x, avoid){
  l <- length(x)
  if(l == 1L){
    return(x)
  }
  numbers <- 1L:l
  out <- paste0(x, ".", numbers)
  ndots <- 1L
  while(any(out %in% avoid)){
    ndots <- ndots + 1L
    out <- paste0(x, paste0(rep(".", ndots), collapse = ""), numbers)
  }
  out
}

make.unique2 <- function(x){
  if(anyDuplicated(x)){
    splt <- split(x, x)
    u <- names(splt)
    for(i in 1L:length(splt)){
      splt_i <- splt[[i]]
      j <- match(splt_i[1L], u)
      avoid <- u[-j]
      splt_i_new <- dotify(splt_i, avoid)
      u <- c(avoid, splt_i_new)
      splt[[i]] <- splt_i_new
    }
    x <- unsplit(splt, x)
  }
  x
}

######## SETUP SURVEY DATA
### Import data
survey_data <- fromJSON("../input/survey_data.json", simplifyVector = F)[[1]]
str(survey_data)

# -----------------------------
# Helper: extract all leaf nodes
# -----------------------------
extract_stage_df <- function(stage_name, stage_list) {
  imap_dfr(stage_list, function(topic_list, topic) {
    imap_dfr(topic_list, function(text, level) {
      tibble(
        stage = stage_name,
        topic = topic,
        level = level,
        text  = text
      )
    })
  })
}

# -----------------------------
# Flatten survey_data
# -----------------------------
upbringing_df <- extract_stage_df("upbringing", survey_data$upbringing)
reflection_df <- extract_stage_df("reflection", survey_data$reflection)
adulthood_df  <- extract_stage_df("adulthood",  survey_data$adulthood)
action_df     <- extract_stage_df("action",     survey_data$action)

all_stages_df <- bind_rows(
  upbringing_df,
  reflection_df,
  adulthood_df,
  action_df
)

# -----------------------------
# Generate all valid combinations
# (within-topic only)
# -----------------------------
vignettes <- all_stages_df %>%
  split(.$topic) %>%
  map_dfr(function(topic_df) {
    
    up  <- filter(topic_df, stage == "upbringing")
    re  <- filter(topic_df, stage == "reflection")
    ad  <- filter(topic_df, stage == "adulthood")
    ac  <- filter(topic_df, stage == "action")
    
    crossing(
      up  %>% select(upbringing_level = level, upbringing_text = text),
      re  %>% select(reflection_level = level, reflection_text = text),
      ad  %>% select(adulthood_level  = level, adulthood_text  = text),
      ac  %>% select(action_level     = level, action_text     = text)
    ) %>%
      mutate(
        topic = unique(topic_df$topic),
        combined_text = str_c(
          upbringing_text,
          reflection_text,
          adulthood_text,
          action_text,
          sep = "<br>"
        )
      )
  })

# -----------------------------
# Optional: add vignette ID
# -----------------------------
vignettes <- vignettes %>%
  mutate(
    vignette_id = paste(
      topic,
      upbringing_level,
      reflection_level,
      adulthood_level,
      action_level,
      sep = "_"
    )
  ) %>%
  relocate(vignette_id)

# Result: one row per full vignette
vignettes <- vignettes %>% mutate(qualtrics_id = make.unique2(topic))
vignettes$qualtrics_id

write.table(vignettes, file = "../output/metadata.txt", quote = F, row.names = F)

### Import intro text & consent form
intro <- readLines("../input/intro.txt")
consentForm <- readLines("../input/consent_form.txt")

######## CREATE SURVEY
sink("../output/survey.txt")

### Start Survey
survey_header()
out("\n")

### Consent Form
create_block("ConsentForm")
  create_item()
  item_id("Consent")
    create_text(consentForm)
    create_answer_options(c("I consent", "I do not consent"))
  
  page_break()

  create_item(QTYPE = "TE:SingleLine")
  item_id("ProlificID")
    create_text("Please enter your Prolific ID:")
    out("\n")

### Intro Block 
create_block("Intro")
  create_item(QTYPE = "Text")
  item_id("Intro")
    create_text(intro)
    out("\n")

### Questions
## Variants
for(idx in 1:nrow(vignettes)){
  create_block(paste0("B-", vignettes$qualtrics_id[idx]))
    create_item()
    item_id(vignettes$qualtrics_id[idx])
      create_text(vignettes$combined_text[idx])
      out("<br><br>")
      #create_text("QUESTION")
      #create_answer_options(c("It doesn't make sense", "It makes sense"))
}

## Demographics
create_block("BDemographics")
  create_item()
  item_id("Gender")
    create_text("Please tell us with which gender you identify.")
    create_answer_options(c("Female", "Male", "Non-binary"))
  
  create_item(QTYPE = "TE:SingleLine")
  item_id("Age")
    create_text("How old are you?")

## Outro
create_block("Outro")
  create_item(QTYPE = "Text")
  item_id("Outro")
    create_text("Thank you very much for participating. Please go to the next screen. You will be redirected to Prolific.")

sink()

