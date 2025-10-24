#title: "Grambank Autocomparison"
#author: "Keras Saryan"

library("magrittr")
library("tidyverse")
library("arrow")

grambank_wide <- open_dataset("grambank_wide") %>%
  dplyr::collect() %>%
  filter(Language_Level == "language") %>%
  select(-Language_Level) %>%
  relocate(
    Language_ID,
    Language_Name,
    Language_Macroarea,
    Language_Family_Name,
    Parameters_Coded
  ) %>%
  arrange(Language_ID)

similarity_mean_all <- data.frame(
  Language_ID = c(),
  Mean_Similarity = c(),
  Language_Name = c(),
  Language_Macroarea = c(),
  Language_Family_Name = c(),
  Parameters_Coded = c()
)

grambank_ids <- grambank_wide$Language_ID
grambank_meta <- grambank_wide[, 1:5]
grambank_matrix <- as.matrix(grambank_wide %>% select(GB020:GB522))

for (i in seq_len(nrow(grambank_matrix))) {
  input_lang_id <- grambank_ids[i]
  input_vec <- grambank_matrix[i, ]

  non_na <- !is.na(input_vec)

  comparison_matrix <- grambank_matrix[, non_na, drop = FALSE]
  target_values <- input_vec[non_na]

  total_compared <- rowSums(!is.na(comparison_matrix))

  match_count <- rowSums(
    sweep(comparison_matrix, 2, target_values, FUN = "=="),
    na.rm = TRUE
  )

  percentage_similarity <- 100 * match_count / total_compared

  percentage_similarity[i] <- NA

  similarity <- data.frame(
    Language_ID = grambank_ids,
    Language_Name = grambank_meta$Language_Name,
    Language_Family_Name = grambank_meta$Language_Family_Name,
    Language_Macroarea = grambank_meta$Language_Macroarea,
    Parameters_Coded = rowSums(!is.na(grambank_matrix)),
    Total_Compared = total_compared,
    Match_Count = match_count,
    Percentage_Similarity = round(percentage_similarity, 2)
  ) %>%
    filter(Language_ID != input_lang_id) %>%
    arrange(desc(Percentage_Similarity))

  write.table(
    similarity,
    file = paste0(
      "grambank_autocomparison/grambank_similarities_",
      input_lang_id,
      ".tsv"
    ),
    quote = FALSE,
    sep = "\t",
    row.names = FALSE,
    col.names = TRUE,
    fileEncoding = "UTF-8"
  )

  similarity_mean_tmp <- data.frame(
    Language_ID = input_lang_id,
    Mean_Similarity = mean(similarity$Percentage_Similarity, na.rm = TRUE)
  )

  similarity_mean <- full_join(
    similarity_mean_tmp,
    grambank_meta,
    by = "Language_ID"
  ) %>%
    drop_na()

  similarity_mean_all <- rbind(similarity_mean_all, similarity_mean)

  similarity_mean_all %<>%
    relocate(
      Language_ID,
      Language_Name,
      Language_Macroarea,
      Language_Family_Name,
      Parameters_Coded
    ) %>%
    arrange(Language_ID)

  message(sprintf(
    "Computing similarity of '%s' to Grambank (%d of %d = %.2f%%)",
    input_lang_id,
    i,
    nrow(grambank_matrix),
    (i / nrow(grambank_matrix)) * 100
  ))
}

write.table(
  similarity_mean_all,
  file = "grambank_autocomparison/grambank_mean_similarities.tsv",
  quote = FALSE,
  sep = "\t",
  row.names = FALSE,
  col.names = TRUE,
  fileEncoding = "UTF-8"
)
