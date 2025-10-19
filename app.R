  library("shiny")
  library("bslib")
  library("tidyverse")
  library("arrow")
  library("DT")
  
  grambank_wide <- open_dataset("grambank_wide") %>%
    dplyr::collect()
  
  ui <- page_fillable(
    
    tags$style(HTML("
      p {
        text-align: justify;
      }
    ")),
    
    fillable = TRUE,
    
    title = "Grambank Comparator – Keras Saryan",
    
    theme = bs_theme(
      bg = "#e3e3e3",
      fg = "#000000",
      primary = "#c00000",
      secondary = "#c00000",
      success = "#00c000",
      base_font = font_google("Noto Sans"),
      code_font = font_google("Noto Sans Mono")
    ),
    
    navset_card_tab(
      
      title = "Grambank Comparator",
      
      sidebar = sidebar(
        width = 350,
        markdown("Use the field below to upload a tab-separated file detailing the Grambank features of your language. Click the \"Compare\" button to see which languages from Grambank's database are most similar languages. (This will take a few seconds to load!) The button below this provides a downloadable TSV of the comparison table.
        
        For more details on the app and type of file required, see the \"About\" tab."),
        fileInput("file", "", accept = c(".tsv", ".csv", "text/tab-separated-values")),
        actionButton("run", "Compare", class = "btn-primary"),
        downloadButton("download_results", "Download Comparison Table", class = "btn-primary")
        
      ),
      
      nav_panel(
        
        card_header("Comparison"),
        
        card_body(
          
          uiOutput("comparison_tab")
          
        )
        
      ),
      
      nav_panel(
        
        card_header("About"),
        
        card_body(
          
          markdown("This [𝒮𝒽𝒾𝓃𝓎](https://www.shinyapps.io/) app is a tool for comparing a novel language with those found in the [Grambank](https://grambank.clld.org/) database and producing similarity measure.
          
          The input field in the sidebar expects a tab-separated file (e.g. `.tsv`, `.csv`, `.txt`) based on [Jessie Peterson](https://www.quothalinguist.com/)'s \"[Grambank Features List for Language Documentation](https://docs.google.com/spreadsheets/d/18Wdhtx7w5SHbe3GmkhFgTE7fjBiEX7SaL4O2-dFibB0/edit?gid=0#gid=0)\" Google Sheet ([see also her explanatory Fiat Lingua article](https://fiatlingua.org/2023/06/)). The easiest way to get this is to export your version of the spreadsheet as a TSV. It doesn't matter if the column names have been changed but the content of the original columns should remain in those columns (and on the same, single sheet). It is also important that the \"Y/N\" column use the same set of values as the original spreadsheet. The app will treat any other values (or any absent values) as equivalent to `NA`.
          
          Clicking \"Compare\" will produce a table in the \"Comparison\" tab which has eight columns:
          
          1. **ID:** The ID of that row's language as found in Grambank.
          2. **Language:** The name of that row's language.
          3. **Family:** The name of the family of that row's language.
          4. **Macroarea:** The name of the area in which that row's language is spoken.
          5. **Parameters Coded:** The number of features for which a known value is provided for that row's language in Grambank (range 8–199).
          6. **Total Compared:** The number of features for which that row's language and novel language are compared.
          7. **Match Count:** The number of compared features that are the same in that row's language and the novel language.
          8. **Similarity (%):** The ratio of Total Compared to Match Count expressed as a percentage.
          
          The app simply compares the (non-NA-equivalent) feature values in the user-supplied file with the corresponding values for each language in Grambank's sample to see what proportion of these match on a per-language basis. (N.B. This app uses shows data from individual *languages* in Grambank and not those entries labelled as *dialects* or *families*.)
          
          The table in the \"Comparison\" tab is equipped with a general search bar but each column can also be individual searched or the numeric columns' ranges changed to facilitate filtering.
          
          The app also produces a downloadable TSV version of the table in the \"Comparison\" tab.
          
          Consult Grambank itself to look at individual [features](https://grambank.clld.org/parameters) and [languages](https://grambank.clld.org/languages).
          
          The code and data for this app are available on [GitHub](https://github.com/keras-saryan/grambank-comparator).
          
          — [Keras Saryan](https://keras-saryan.github.io/)")
          
        )
        
      )
      
      
    )
  )
  
  server <- function(input, output, session) {
    
    results <- eventReactive(input$run, {
      
      req(input$file)
      
      withProgress(message = "Reading input and comparing with Grambank...", value = NULL, {
        
        Sys.sleep(0.1)
      
      input_lang <- read.csv(input$file$datapath, header = TRUE, sep = "\t", quote = "", stringsAsFactors = FALSE, encoding = "UTF-8") %>%
        select(-2) %>%
        rename(Parameter_ID = 1, Parameter_Category = 2, Parameter_Subcategory = 3, Parameter_Value_Long = 4, Parameter_Comment = 5) %>%
        mutate(Parameter_Value_Long = str_trim(Parameter_Value_Long),
               Parameter_Value_Long = case_when(
                 Parameter_Value_Long == "Yes" ~ "present",
                 Parameter_Value_Long == "No" ~ "absent",
                 Parameter_Value_Long == "Both" ~ "both",
                 Parameter_Value_Long == "Not sure" ~ NA_character_,
                 TRUE ~ Parameter_Value_Long)) %>%
        unique() %>%
        mutate(Language_ID = "input_lang", Language_Name = "input_lang", Language_Macroarea = "input_lang", Language_Family_Name = "input_lang", Language_Level = "input_lang") %>%
        relocate(Language_ID, Language_Name, Language_Macroarea, Language_Family_Name, Language_Level)
      
      input_lang_wide <- input_lang %>%
        select(Language_ID, Language_Name, Language_Macroarea, Language_Family_Name, Language_Level, Parameter_ID, Parameter_Value_Long) %>%
        pivot_wider(names_from = Parameter_ID, values_from = Parameter_Value_Long) %>%
        rowwise() %>%
        mutate(Parameters_Coded = sum(!is.na(c_across(-Language_ID)))) %>%
        ungroup() %>%
        relocate(Language_ID, Language_Name, Language_Macroarea, Language_Family_Name, Language_Level, Parameters_Coded)
      
      grambank_wide_plus <- bind_rows(grambank_wide, input_lang_wide)
      
      sum(!is.na(input_lang_wide))
      
      non_na_positions <- which(!is.na(input_lang_wide))
      
      similarity <- grambank_wide_plus %>%
        filter(Language_Level == "language") %>%
        filter(Language_ID != "input_lang") %>%
        rowwise() %>%
        mutate(Row_Values = list(c_across(-c(Language_ID, Language_Name, Language_Macroarea, Language_Family_Name, Language_Level, Parameters_Coded))),
               Match_Count = sum(unlist(Row_Values)[non_na_positions] == input_lang_wide[non_na_positions], na.rm = TRUE),
               Total_Compared = sum(!is.na(unlist(Row_Values)[non_na_positions]) & !is.na(input_lang_wide[non_na_positions])),
               Percentage_Similarity = 100 * Match_Count / Total_Compared) %>%
        ungroup() %>%
        select(Language_ID, Language_Name, Language_Family_Name, Language_Macroarea, Parameters_Coded, Total_Compared, Match_Count, Percentage_Similarity) %>%
        mutate(Percentage_Similarity = as.numeric(format(round(Percentage_Similarity, 2), nsmall = 2))) %>%
        arrange(desc(Percentage_Similarity)) %>%
        rename(ID = Language_ID, Language = Language_Name, Family = Language_Family_Name, Macroarea = Language_Macroarea, `Similarity (%)` = Percentage_Similarity)
      
      colnames(similarity) = gsub("_", " ", colnames(similarity))
      
      similarity
      
      })
    })
    
    output$results <- renderDataTable(server = TRUE, {
      results()
    }, options = list(pageLength = 20), filter = "top", rownames = FALSE
    )
    
    output$comparison_tab <- renderUI({
      if (is.null(input$file) || input$run == 0) {
        markdown("Upload a Grambank feature file to see comparisons here!")
      } else {
        DTOutput("results")
      }
    })
    
    output$download_results <- downloadHandler(
      filename = function() {
        paste0("input_lang_grambank_comparator.tsv")
      },
      content = function(file) {
        req(results())
        write.table(results(), file, sep = "\t", row.names = FALSE, quote = FALSE)
      }
    )
    
  }
  
  shinyApp(ui = ui, server = server)
