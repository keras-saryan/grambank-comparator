library("shiny")
library("bslib")
library("magrittr")
library("tidyverse")
library("arrow")
library("DT")
library("leaflet")

grambank_coordinates <- open_dataset("grambank_coordinates") %>%
  dplyr::collect()

grambank_wide <- open_dataset("grambank_wide") %>%
  dplyr::collect()

similarity_colours <- c("#ffffff", "#ff0000", "#0000ff", "#000000")
similarity_colours_8 <- colorRampPalette(similarity_colours)(8)
similarity_colours_10 <- colorRampPalette(similarity_colours)(10)
similarity_colours_100 <- colorRampPalette(similarity_colours)(100)

ui <- page_fillable(
  
  tags$style(HTML("
      p {
        text-align: justify;
      }
      a:link {
        text-decoration: none;
      }
      a:visited {
        text-decoration: none;
      }
      a:hover {
        text-decoration: none;
      }
      a:active {
        text-decoration: none;
      }
    ")),
  
  fillable = TRUE,
  
  title = "Grambank Comparator – Keras Saryan",
  
  theme = bs_theme(
    bg = "#e3e3e3",
    fg = "#000000",
    primary = "#c00000",
    secondary = "#c00000",
    success = "#00aa00",
    base_font = font_google("Noto Sans"),
    code_font = font_google("Noto Sans Mono")
  ),
  
  navset_card_tab(
    
    title = "Grambank Comparator",
    
    sidebar = sidebar(
      width = 350,
      markdown("Use the field below to upload a tab-separated file detailing the Grambank features of your language. Click the \"Compare\" button to see which languages from Grambank's database are most similar languages—this will take a few seconds to load! The button below this also provides a downloadable TSV of the comparison table."),
      fileInput("file", "", accept = c(".tsv", ".csv", "text/tab-separated-values")),
      actionButton("run", "Compare", class = "btn-primary"),
      downloadButton("download_results", "Download Comparison Table", class = "btn-primary"),
      
      numericInput(inputId = "compared_min",
                   label = "\"Total Compared\" Minimum:",
                   min = 1,
                   max = 199,
                   step = 1,
                   value = 11),
      
      markdown("For more details on the app and type of file required, see the \"About\" tab."),
      
    ),
    
    nav_panel(
      
      card_header("Table"),
      
      card_body(
        
        uiOutput("comparison_table")
        
      )
      
    ),
    
    nav_panel(
      
      card_header("Plot"),
      
      card_body(
        
        uiOutput("comparison_plot"),
        
        conditionalPanel(
          condition = "output.plot_visible == true",
          selectInput(
            inputId = "facet_toggle",
            label = "Facet By:",
            choices = c(
              "Nothing" = "none",
              "Macroarea" = "macroarea",
              "6 Most Similar Families" = "families"
            ),
            selected = "none"
          )
        ),
        
        conditionalPanel(
          condition = "input.facet_toggle == 'families'",
          numericInput(
            inputId = "family_min",
            label = "Minimum Compared Languages In A Family:",
            min = 1,
            max = 520,
            step = 1,
            value = 4
          )
        )
        
      )
      
    ),
    
    nav_panel(
      
      card_header("Map"),
      
      card_body(
        
        uiOutput("no_map"),
        
        leafletOutput("comparison_map")
        
      )
      
    ),
    
    nav_panel(
      
      card_header("About"),
      
      card_body(
        
        markdown("**Grambank Comparator** is a [𝒮𝒽𝒾𝓃𝓎](https://www.shinyapps.io/) app for comparing a novel language with those found in the [Grambank](https://grambank.clld.org/) database and producing a rough similarity measure.
          
          The input field in the sidebar expects a tab-separated file (e.g. `.tsv`, `.csv`, `.txt`) based on [Jessie Peterson](https://www.quothalinguist.com/)'s \"[Grambank Features List for Language Documentation](https://docs.google.com/spreadsheets/d/18Wdhtx7w5SHbe3GmkhFgTE7fjBiEX7SaL4O2-dFibB0/edit?gid=0#gid=0)\" Google Sheet ([see also her explanatory *Fiat Lingua* article](https://fiatlingua.org/2023/06/)). The easiest way to get this is to export your version of the spreadsheet as a TSV. It doesn't matter if the column names have been changed but the content of the original columns should remain in those same columns (and on the same, single sheet). It is also important that the \"Y/N\" column use the same set of values as the original spreadsheet; the app will treat any other values (or any absent values) as equivalent to `NA`.
          
          Clicking \"Compare\" will produce a table in the \"Table\" tab which has eight columns:
          
          1. **ID:** The ID of that row's language as found in Grambank.
          2. **Language:** The name of that row's language.
          3. **Family:** The name of the family of that row's language.
          4. **Macroarea:** The name of the area in which that row's language is spoken.
          5. **Parameters Coded:** The number of features for which a known value is provided for that row's language in Grambank (range 8–199).
          6. **Total Compared:** The number of features for which that row's language and novel language are compared.
          7. **Match Count:** The number of compared features that are the same in that row's language and the novel language.
          8. **Similarity (%):** The ratio of Total Compared to Match Count expressed as a percentage.
          
          The app simply compares the (non-NA-equivalent) feature values in the user-supplied file with the corresponding values for each language in Grambank's sample to see what proportion of these match on a per-language basis. (N.B. The data used include only individual *languages* in Grambank and not those entries labelled as *dialects* or *families*.)
          
          The numeric input box in the sidebar can be used to change the minimum number of comparisons between the user-supplied language and the Grambank sample languages required to be displayed in both the table and map.
          
          The table is equipped with a general search bar but each column can also be individually searched or the numeric columns' ranges changed to facilitate filtering. A TSV version of this table can be downloaded in the sidebar.
          
          The \"Plot\" tab shows the same data as in \"Table\" but plotted as a histogram, with the options of facetting by macroarea or the 6 most similar language families in the data set, which can be further tweaked by changing the required minimum number of languages in a family for plotting.
          
          The \"Map\" tab shows a world map with circles for each language in Grambank colour coded according to their similarity to the user-supplied language data. Clicking the circle will show the language's name and the percentage similarity.
          
          In the histograms and map, the <span style='color:#f00000; font-weight:bold;'>redder</span> a column/circle, the more similar a language; the <span style='color:#0000f0; font-weight:bold;'>bluer</span> a column/circle, the more different the language; however, extreme similarities/differences are represented by white/black respectively since these will usually be extreme outliers.
          
          Consult Grambank itself for more on individual [features](https://grambank.clld.org/parameters) and [languages](https://grambank.clld.org/languages).
          
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
  
  final_results <- reactive({
    req(results())
    results() %>%
      filter(`Total Compared` >= input$compared_min)
  })
  
  output$results <- renderDataTable(server = TRUE, {
    final_results()
  }, options = list(pageLength = 20), filter = "top", rownames = FALSE
  )
  
  output$comparison_table <- renderUI({
    if (is.null(input$file) || input$run == 0) {
      markdown("Upload a Grambank feature file to see a table here!")
    } else {
      DTOutput("results")
    }
  })
  
  output$download_results <- downloadHandler(
    filename = function() {
      paste0("input_lang_grambank_comparator.tsv")
    },
    content = function(file) {
      req(final_results())
      write.table(final_results(), file, sep = "\t", row.names = FALSE, quote = FALSE)
    }
  )
  
  output$comparison_hist <- renderPlot({
    req(final_results())
    
    if (input$facet_toggle == "macroarea") {
      macro_levels <- final_results() %>%
        group_by(Macroarea) %>%
        summarise(Mean_Similarity = mean(`Similarity (%)`, na.rm = TRUE)) %>%
        arrange(desc(Mean_Similarity)) %>%
        pull(Macroarea)
      
      final_results() %>%
        group_by(Macroarea) %>%
        mutate(Mean_Similarity = mean(`Similarity (%)`, na.rm = TRUE), Macroarea = factor(Macroarea, levels = macro_levels)) %>%
        ggplot(aes(`Similarity (%)`)) +
        geom_histogram(aes(y = after_stat(density), fill = after_stat(x)),
                       binwidth = 1, colour = "#e3e3e3") +
        scale_fill_gradientn(colours = rev(similarity_colours_100)) +
        geom_vline(aes(xintercept = Mean_Similarity), colour = "#00aa00", linetype = "dashed", linewidth = 2) +
        theme(
          legend.position = "none",
          panel.background = element_rect(fill = "#e3e3e3", colour = NA),
          plot.background = element_rect(fill = "#e3e3e3", colour = NA),
          panel.grid.major = element_line(colour = "#cfcfcf"),
          panel.grid.minor = element_line(colour = "#d9d9d9")
        ) +
        scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
        labs(x = "Similarity (%)", y = "Density") +
        facet_wrap(~Macroarea, scales = "free_y")
    } else if (input$facet_toggle == "families") {
      top_families <- final_results() %>%
        filter(Family != "") %>%
        group_by(Family) %>%
        summarise(avg_similarity = mean(`Similarity (%)`, na.rm = TRUE),
                  n_languages = n_distinct(ID)) %>%
        filter(n_languages >= input$family_min) %>%
        arrange(desc(avg_similarity)) %>%
        slice_head(n = 6) %>%
        pull(Family)
      
      final_results() %>%
        filter(Family %in% top_families) %>%
        group_by(Family) %>%
        mutate(Mean_Similarity = mean(`Similarity (%)`, na.rm = TRUE),
               Family = factor(Family, levels = top_families)) %>%
        ggplot(aes(`Similarity (%)`)) +
        geom_histogram(aes(y = after_stat(density), fill = after_stat(x)),
                       binwidth = 1, colour = "#e3e3e3") +
        scale_fill_gradientn(colours = rev(similarity_colours_100)) +
        geom_vline(aes(xintercept = Mean_Similarity),
                   colour = "#00aa00", linetype = "dashed", linewidth = 2) +
        theme(
          legend.position = "none",
          panel.background = element_rect(fill = "#e3e3e3", colour = NA),
          plot.background = element_rect(fill = "#e3e3e3", colour = NA),
          panel.grid.major = element_line(colour = "#cfcfcf"),
          panel.grid.minor = element_line(colour = "#d9d9d9")
        ) +
        scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
        labs(x = "Similarity (%)", y = "Density") +
        facet_wrap(~Family, scales = "free_y")
    } else {
      final_results() %>%
        ggplot(aes(`Similarity (%)`)) +
        geom_histogram(aes(y = after_stat(density), fill = after_stat(x)),
                       binwidth = 1, colour = "#e3e3e3") +
        scale_fill_gradientn(colours = rev(similarity_colours_100)) +
        geom_vline(aes(xintercept = mean(`Similarity (%)`, na.rm = TRUE)),
                   colour = "#00aa00", linetype = "dashed", linewidth = 2) +
        theme(
          legend.position = "none",
          panel.background = element_rect(fill = "#e3e3e3", colour = NA),
          plot.background = element_rect(fill = "#e3e3e3", colour = NA),
          panel.grid.major = element_line(colour = "#cfcfcf"),
          panel.grid.minor = element_line(colour = "#d9d9d9")
        ) +
        scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
        labs(x = "Similarity (%)", y = "Density")
    }
  })
  
  output$comparison_plot <- renderUI({
    if (is.null(input$file) || input$run == 0) {
      markdown("Upload a Grambank feature file to see a plot here!")
    } else {
      tagList(
        plotOutput("comparison_hist", height = "800px")
      )
    }
  })
  
  output$plot_visible <- reactive({
    !is.null(input$file) && input$run > 0
  })
  
  outputOptions(output, "plot_visible", suspendWhenHidden = FALSE)
  
  output$no_map <- renderUI({
    if (is.null(input$file) || input$run == 0) {
      markdown("Upload a Grambank feature file to see a map here!")
    }
  })
  
  output$comparison_map <- renderLeaflet({
    
    mapping <- full_join(final_results(), grambank_coordinates, by = "ID") %>%
      filter(`Similarity (%)` >= 0 & `Similarity (%)` <= 100) %>%
      mutate(Colour = case_when(
        `Similarity (%)` >= 87.5 & `Similarity (%)` <= 100 ~ similarity_colours_8[1],
        `Similarity (%)` >= 75 & `Similarity (%)` < 87.5 ~ similarity_colours_8[2],
        `Similarity (%)` >= 62.5 & `Similarity (%)` < 75 ~ similarity_colours_8[3],
        `Similarity (%)` >= 50 & `Similarity (%)` < 62.5 ~ similarity_colours_8[4],
        `Similarity (%)` >= 37.5 & `Similarity (%)` < 50 ~ similarity_colours_8[5],
        `Similarity (%)` >= 25 & `Similarity (%)` < 37.5 ~ similarity_colours_8[6],
        `Similarity (%)` >= 12.5 & `Similarity (%)` < 25 ~ similarity_colours_8[7],
        `Similarity (%)` >= 0 & `Similarity (%)` < 12.5 ~ similarity_colours_8[8],
        TRUE ~ "#00ff00"
      ))
    
    leaflet(mapping) %>%
      addTiles() %>%
      setView(lng = 0, lat = 20, zoom = 2) %>%
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        radius = 7.5,
        color = ~Colour,
        fillColor = ~Colour,
        fillOpacity = 0.75,
        stroke = FALSE,
        popup = ~paste0("<b>", Language, "</b><br>", `Similarity (%)`, "%")
      )
    
  })
  
}

shinyApp(ui = ui, server = server)
