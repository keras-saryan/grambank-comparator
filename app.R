library("shiny")
library("bslib")
library("magrittr")
library("tidyverse")
library("arrow")
library("DT")
library("leaflet")

grambank_coordinates <- open_dataset("grambank_coordinates") %>%
  dplyr::collect() %>%
  mutate(
    Continent = case_when(
      Continent == "AF" ~ "Africa",
      Continent == "AS" ~ "Asia",
      Continent == "EU" ~ "Europe",
      Continent == "NA" ~ "North America",
      Continent == "OC" ~ "Oceania",
      Continent == "SA" ~ "South America",
      TRUE ~ Continent
    )
  )

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
  )

grambank_parameters <- as.character(colnames(
  grambank_wide %>% select(GB020:GB522)
))

grambank_parameter_categories <- read_tsv("grambank_parameter_categories.tsv")

grambank_categories <- sort(unique(
  grambank_parameter_categories$Parameter_Category
))

grambank_subcategories <- sort(unique(
  grambank_parameter_categories$Parameter_Subcategory
))

fontCustom <- "Noto Sans"

dark_grey <- "#bfbfbf"
light_grey <- "#dfdfdf"
lighter_grey <- "#ededed"
mid_green <- "#00aa00"

theme_set(
  theme_bw() +
    theme(
      text = element_text(family = fontCustom, size = 15),
      axis.title = element_text(face = "bold"),
      legend.position = "none",
      panel.background = element_rect(fill = lighter_grey, colour = NA),
      plot.background = element_rect(fill = lighter_grey, colour = NA),
      panel.grid.major = element_line(colour = dark_grey),
      panel.grid.minor = element_line(colour = light_grey)
    )
)

similarity_colours <- c("#ffaaaa", "#ff0000", "#0000ff", "#000088", "#000022")
similarity_colours_8 <- colorRampPalette(similarity_colours)(8)
similarity_colours_10 <- colorRampPalette(similarity_colours)(10)
similarity_colours_100 <- colorRampPalette(similarity_colours)(100)

ui <- page_fillable(
  tags$style(HTML(
    "
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
    "
  )),

  fillable = TRUE,

  title = "Grambank Comparator – Keras Saryan",

  theme = bs_theme(
    bg = lighter_grey,
    fg = "#000000",
    primary = "#c00000",
    secondary = "#c00000",
    base_font = font_google("Noto Sans"),
    code_font = font_google("Noto Sans Mono")
  ),

  navset_card_tab(
    title = "Grambank Comparator",

    sidebar = sidebar(
      width = 350,

      markdown(
        "Use the field below to upload a tab-separated file detailing the Grambank features of your language. Click the \"Compare\" button to see which languages from Grambank's database are most similar."
      ),

      markdown(
        "For more details on the app and type of file required, see the \"About\" tab."
      ),

      fileInput(
        "file",
        "",
        accept = c(".tsv", ".csv", "text/tab-separated-values")
      ),

      actionButton("run", "Compare"),

      numericInput(
        inputId = "compared_min",
        label = "\"Total Compared\" Minimum:",
        min = 1,
        max = 195,
        step = 1,
        value = 100
      ),

      selectInput(
        inputId = "categories_to_compare",
        label = "Categories To Compare:",
        choices = grambank_categories,
        multiple = TRUE,
        selected = grambank_categories
      ),

      actionButton("reset_categories", "Reset Categories"),

      selectInput(
        inputId = "subcategories_to_compare",
        label = "Subcategories To Compare:",
        choices = grambank_subcategories,
        multiple = TRUE,
        selected = grambank_subcategories
      ),

      actionButton("reset_subcategories", "Reset Subcategories"),

      selectInput(
        inputId = "parameter_to_compare",
        label = "Select A Single Parameter:",
        choices = c("Compare All", grambank_parameters),
        selected = "Compare All"
      )
    ),

    nav_panel(
      card_header("Table"),

      card_body(
        uiOutput("comparison_table"),

        conditionalPanel(
          condition = "output.output_visible == true",
          downloadButton("download_table_tsv", "Download As TSV")
        )
      )
    ),

    nav_panel(
      card_header("Plot"),

      card_body(
        uiOutput("comparison_plot"),

        conditionalPanel(
          condition = "output.output_visible == true",
          selectInput(
            inputId = "facet_toggle",
            label = "Facet By:",
            choices = c(
              "Nothing" = "none",
              "Macroarea" = "macroarea",
              "Continent" = "continent",
              "UN Subregion" = "subregion",
              "Families" = "families"
            ),
            selected = "none"
          )
        ),

        conditionalPanel(
          condition = "input.facet_toggle == 'subregion'",
          numericInput(
            inputId = "subregion_min",
            label = "Minimum Compared Languages In A Subregion:",
            min = 1,
            max = 400,
            step = 1,
            value = 6
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
            value = 6
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
        markdown(
          "**Grambank Comparator** is a [𝒮𝒽𝒾𝓃𝓎](https://www.shinyapps.io/) app for comparing a novel language with those found in the [Grambank](https://grambank.clld.org/) database and producing a rough similarity measure.
          
          The upload field in the sidebar expects a tab-separated file (e.g. `.tsv`, `.csv`, `.txt`) based on [Jessie Peterson](https://www.quothalinguist.com/)'s \"[Grambank Features List for Language Documentation](https://docs.google.com/spreadsheets/d/18Wdhtx7w5SHbe3GmkhFgTE7fjBiEX7SaL4O2-dFibB0/edit?gid=0#gid=0)\" Google Sheet ([see also her explanatory *Fiat Lingua* article](https://fiatlingua.org/2023/06/)). The easiest way to get this is to export your version of the spreadsheet as a TSV. It doesn't matter if the column names have been changed but the content of the original columns should remain in those same columns (and on the same, single sheet). It is also important that the \"Y/N\" column use the same set of values as the original spreadsheet; the app will treat any other values (or any absent values) as equivalent to `NA`.
          
          The app simply compares the (non-NA-equivalent) feature values in the user-supplied file with the corresponding values for each language in Grambank's sample to see what proportion of these match on a per-language basis; all features are thus weighted equally when considering \"similarity\". The data used include only individual *languages* in Grambank and not those entries labelled as *dialects* or *families*.
          
          Clicking \"Compare\" will produce outputs in the \"Table\", \"Plot\" and \"Map\" tabs.
          
          A numeric input box in the sidebar can be used to change the minimum number of comparisons between the user-supplied language and the Grambank sample languages required to be displayed in the table, plots and map.
          
          You can filter the data set used in the comparison by the category and/or subcategory of parameters as coded in Jessie's spreadsheet using the \"Categories To Compare\" and \"Subcategories To Compare\" select boxes. You can also choose to view an individual parameter. Note that it may be necessary to lower the \"Total Compared\" minimum to see any results displayed.
          
          The \"Table\" tab shows a table with the following eight columns:
          
          1. **ID:** The ID of that row's language as found in Grambank.
          2. **Language:** The name of that row's language.
          3. **Family:** The name of the family of that row's language.
          4. **Macroarea:** The name of the area in which that row's language is spoken.
          5. **Parameters Coded:** The number of features for which a known value is provided for that row's language in Grambank (range 4–195).
          6. **Total Compared:** The number of features for which that row's language and novel language are compared.
          7. **Match Count:** The number of compared features that are the same in that row's language and the novel language.
          8. **Similarity (%):** The ratio of Total Compared to Match Count expressed as a percentage.
          
          The table is equipped with a general search bar but each column can also be individually searched or the numeric columns' ranges changed to facilitate filtering. A TSV version of the table can be downloaded using the button below it.
          
          The \"Plot\" tab shows the same data as in \"Table\" but plotted as a histogram, with the options of facetting by macroarea, continent, UN subregion ([sourced here](https://github.com/lukert33/united-nations-geoscheme-subregions-json)) and language family—the final two can be further tweaked by changing the required minimum number of languages in a subregion/family for plotting. Plots can be saved by right clicking and choosing the appropriate option from the context menu.
          
          The \"Map\" tab shows a world map with circles for each language in Grambank colour coded according to their similarity to the user-supplied language data. Clicking the circle will show the language's name and the percentage similarity.
          
          In the histograms and map, the <span style='color:#f00000; font-weight:bold;'>redder</span> a bar/circle, the more similar a language; the <span style='color:#0000f0; font-weight:bold;'>bluer</span> a bar/circle, the more different the language; however, extreme similarities/differences approach white/black respectively since these will usually be extreme outliers unless you are radically paring the data down.
          
          Consult Grambank itself for more on individual [features](https://grambank.clld.org/parameters) and [languages](https://grambank.clld.org/languages).
          
          The code and data for this app are available on [GitHub](https://github.com/keras-saryan/grambank-comparator). The same repo also hosts a collection of files comparing every language in Grambank against every other language and one of the means, medians and standard deviations of these intra-Grambank similarities for each language in the `./grambank_autocomparison/` folder.
          
          The original Grambank data used here are also available on [GitHub](https://github.com/grambank/grambank).
          
          — [Keras Saryan](https://keras-saryan.github.io/)
          
          [[CC-BY-NC-SA](https://creativecommons.org/licenses/by-nc-sa/4.0/deed.en)]"
        )
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$reset_categories, {
    updateSelectInput(
      session,
      "categories_to_compare",
      selected = grambank_categories
    )
  })

  observeEvent(input$reset_subcategories, {
    updateSelectInput(
      session,
      "subcategories_to_compare",
      selected = grambank_subcategories
    )
  })

  grambank_filtered <- reactive({
    req(input$categories_to_compare)
    req(input$subcategories_to_compare)
    req(input$parameter_to_compare)

    if (input$parameter_to_compare != "Compare All") {
      excluded_parameters <- grambank_parameter_categories %>%
        filter(Parameter_ID != input$parameter_to_compare) %>%
        pull(Parameter_ID)

      grambank_wide %<>%
        mutate(across(all_of(excluded_parameters), ~NA))
    } else if (input$parameter_to_compare == "Compare All") {
      excluded_parameters <- grambank_parameter_categories %>%
        filter(
          !(Parameter_Category %in% input$categories_to_compare) |
            !(Parameter_Subcategory %in% input$subcategories_to_compare)
        ) %>%
        pull(Parameter_ID)

      grambank_wide %<>%
        mutate(across(all_of(excluded_parameters), ~NA))
    }
  })

  input_lang <- eventReactive(input$run, {
    req(input$file)

    input_lang <- read.csv(
      input$file$datapath,
      header = TRUE,
      sep = "\t",
      quote = "",
      stringsAsFactors = FALSE,
      encoding = "UTF-8"
    ) %>%
      select(-2) %>%
      rename(
        Parameter_ID = 1,
        Parameter_Category = 2,
        Parameter_Subcategory = 3,
        Parameter_Value_Long = 4,
        Parameter_Comment = 5
      ) %>%
      mutate(
        Parameter_Value_Long = str_trim(Parameter_Value_Long),
        Parameter_Value_Long = case_when(
          Parameter_Value_Long == "Yes" ~ "present",
          Parameter_Value_Long == "No" ~ "absent",
          Parameter_Value_Long == "Both" ~ "both",
          Parameter_Value_Long == "Not sure" ~ NA_character_,
          TRUE ~ Parameter_Value_Long
        )
      ) %>%
      unique() %>%
      mutate(
        Language_ID = "input_lang",
        Language_Name = "input_lang",
        Language_Macroarea = "input_lang",
        Language_Family_Name = "input_lang"
      ) %>%
      relocate(
        Language_ID,
        Language_Name,
        Language_Macroarea,
        Language_Family_Name
      )

    input_lang %>%
      select(
        Language_ID,
        Language_Name,
        Language_Macroarea,
        Language_Family_Name,
        Parameter_ID,
        Parameter_Value_Long
      ) %>%
      pivot_wider(
        names_from = Parameter_ID,
        values_from = Parameter_Value_Long
      ) %>%
      rowwise() %>%
      mutate(Parameters_Coded = sum(!is.na(c_across(GB020:GB522)))) %>%
      ungroup() %>%
      relocate(
        Language_ID,
        Language_Name,
        Language_Macroarea,
        Language_Family_Name,
        Parameters_Coded
      )
  })

  results <- reactive({
    req(input$file)

    withProgress(message = "Comparing input with Grambank...", value = NULL, {
      input_lang_values <- as.character(unlist(select(
        input_lang(),
        GB020:GB522
      )))

      grambank_matrix <- as.matrix(select(grambank_filtered(), GB020:GB522))

      input_lang_matrix <- matrix(
        input_lang_values,
        nrow = nrow(grambank_matrix),
        ncol = length(input_lang_values),
        byrow = TRUE
      )

      matches <- grambank_matrix == input_lang_matrix

      total_compared <- rowSums(
        !is.na(grambank_matrix) & !is.na(input_lang_matrix)
      )
      match_count <- rowSums(matches, na.rm = TRUE)
      parameters_coded <- rowSums(!is.na(grambank_matrix))
      percentage_similarity <- 100 * match_count / total_compared

      similarity <- grambank_filtered() %>%
        mutate(
          Parameters_Coded = parameters_coded,
          Match_Count = match_count,
          Total_Compared = total_compared,
          `Similarity (%)` = round(percentage_similarity, 2)
        ) %>%
        select(
          Language_ID,
          Language_Name,
          Language_Family_Name,
          Language_Macroarea,
          Parameters_Coded,
          Total_Compared,
          Match_Count,
          `Similarity (%)`
        ) %>%
        arrange(desc(`Similarity (%)`)) %>%
        rename(
          ID = Language_ID,
          Language = Language_Name,
          Family = Language_Family_Name,
          Macroarea = Language_Macroarea
        )

      colnames(similarity) <- gsub("_", " ", colnames(similarity))

      similarity
    })
  })

  final_results <- reactive({
    req(results())
    results() %>%
      filter(`Total Compared` >= input$compared_min)
  })

  spatial_results <- reactive({
    req(final_results())
    full_join(final_results(), grambank_coordinates, by = "ID")
  })

  output$output_visible <- reactive({
    req(final_results())
    !is.null(input$file) && input$run > 0
  })

  outputOptions(output, "output_visible", suspendWhenHidden = FALSE)

  output$results <- renderDataTable({
    datatable(
      final_results(),
      options = list(pageLength = 20),
      rownames = FALSE,
      filter = "top"
    ) %>%
      formatStyle("ID", target = "cell", fontFamily = "monospace")
  })

  output$comparison_table <- renderUI({
    if (is.null(input$file) || input$run == 0) {
      markdown("Upload a Grambank feature file to see a table here!")
    } else {
      DTOutput("results")
    }
  })

  output$download_table_tsv <- downloadHandler(
    filename = function() {
      paste0("input_lang_grambank_comparator_table.tsv")
    },
    content = function(file) {
      req(final_results())
      write.table(
        final_results(),
        file,
        sep = "\t",
        row.names = FALSE,
        quote = FALSE
      )
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
        mutate(
          Mean_Similarity = mean(`Similarity (%)`, na.rm = TRUE),
          Macroarea = factor(Macroarea, levels = macro_levels)
        ) %>%
        ggplot(aes(`Similarity (%)`)) +
        geom_histogram(
          aes(y = after_stat(density), fill = after_stat(x)),
          binwidth = 1,
          colour = dark_grey
        ) +
        scale_fill_gradientn(colours = rev(similarity_colours_100)) +
        geom_vline(
          aes(xintercept = Mean_Similarity),
          colour = mid_green,
          linetype = "dashed",
          linewidth = 1.25
        ) +
        scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
        labs(x = "Similarity (%)", y = "Density") +
        facet_wrap(~Macroarea, scales = "free_y")
    } else if (input$facet_toggle == "continent") {
      continent_levels <- spatial_results() %>%
        filter(!is.na(Continent)) %>%
        group_by(Continent) %>%
        summarise(Mean_Similarity = mean(`Similarity (%)`, na.rm = TRUE)) %>%
        arrange(desc(Mean_Similarity)) %>%
        pull(Continent)

      spatial_results() %>%
        filter(!is.na(Continent)) %>%
        group_by(Continent) %>%
        mutate(
          Mean_Similarity = mean(`Similarity (%)`, na.rm = TRUE),
          Continent = factor(Continent, levels = continent_levels)
        ) %>%
        ggplot(aes(`Similarity (%)`)) +
        geom_histogram(
          aes(y = after_stat(density), fill = after_stat(x)),
          binwidth = 1,
          colour = dark_grey
        ) +
        scale_fill_gradientn(colours = rev(similarity_colours_100)) +
        geom_vline(
          aes(xintercept = Mean_Similarity),
          colour = mid_green,
          linetype = "dashed",
          linewidth = 1.25
        ) +
        scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
        labs(x = "Similarity (%)", y = "Density") +
        facet_wrap(~Continent, scales = "free_y")
    } else if (input$facet_toggle == "subregion") {
      subregion_levels <- spatial_results() %>%
        filter(!is.na(Subregion)) %>%
        group_by(Subregion) %>%
        summarise(
          Mean_Similarity = mean(`Similarity (%)`, na.rm = TRUE),
          n_languages = n_distinct(ID)
        ) %>%
        arrange(desc(Mean_Similarity)) %>%
        filter(n_languages >= input$subregion_min) %>%
        slice_head(n = 6) %>%
        pull(Subregion)

      spatial_results() %>%
        filter(Subregion %in% subregion_levels) %>%
        group_by(Subregion) %>%
        mutate(
          Mean_Similarity = mean(`Similarity (%)`, na.rm = TRUE),
          Subregion = factor(Subregion, levels = subregion_levels)
        ) %>%
        ggplot(aes(`Similarity (%)`)) +
        geom_histogram(
          aes(y = after_stat(density), fill = after_stat(x)),
          binwidth = 1,
          colour = dark_grey
        ) +
        scale_fill_gradientn(colours = rev(similarity_colours_100)) +
        geom_vline(
          aes(xintercept = Mean_Similarity),
          colour = mid_green,
          linetype = "dashed",
          linewidth = 1.25
        ) +
        scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
        labs(x = "Similarity (%)", y = "Density") +
        facet_wrap(~Subregion, scales = "free_y")
    } else if (input$facet_toggle == "families") {
      top_families <- final_results() %>%
        filter(Family != "") %>%
        group_by(Family) %>%
        summarise(
          Mean_Similarity = mean(`Similarity (%)`, na.rm = TRUE),
          n_languages = n_distinct(ID)
        ) %>%
        filter(n_languages >= input$family_min) %>%
        arrange(desc(Mean_Similarity)) %>%
        slice_head(n = 6) %>%
        pull(Family)

      final_results() %>%
        filter(Family %in% top_families) %>%
        group_by(Family) %>%
        mutate(
          Mean_Similarity = mean(`Similarity (%)`, na.rm = TRUE),
          Family = factor(Family, levels = top_families)
        ) %>%
        ggplot(aes(`Similarity (%)`)) +
        geom_histogram(
          aes(y = after_stat(density), fill = after_stat(x)),
          binwidth = 1,
          colour = dark_grey
        ) +
        scale_fill_gradientn(colours = rev(similarity_colours_100)) +
        geom_vline(
          aes(xintercept = Mean_Similarity),
          colour = mid_green,
          linetype = "dashed",
          linewidth = 1.25
        ) +
        scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
        labs(x = "Similarity (%)", y = "Density") +
        facet_wrap(~Family, scales = "free_y")
    } else {
      final_results() %>%
        ggplot(aes(`Similarity (%)`)) +
        geom_histogram(
          aes(y = after_stat(density), fill = after_stat(x)),
          binwidth = 1,
          colour = dark_grey
        ) +
        scale_fill_gradientn(colours = rev(similarity_colours_100)) +
        geom_vline(
          aes(xintercept = mean(`Similarity (%)`, na.rm = TRUE)),
          colour = mid_green,
          linetype = "dashed",
          linewidth = 1.25
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

  output$no_map <- renderUI({
    if (is.null(input$file) || input$run == 0) {
      markdown("Upload a Grambank feature file to see a map here!")
    }
  })

  output$comparison_map <- renderLeaflet({
    mapping <- spatial_results() %>%
      drop_na(`Similarity (%)`) %>%
      filter(`Similarity (%)` >= 0 & `Similarity (%)` <= 100) %>%
      mutate(
        Colour = case_when(
          `Similarity (%)` >= 87.5 & `Similarity (%)` <= 100 ~ similarity_colours_8[1],
          `Similarity (%)` >= 75.0 & `Similarity (%)` < 87.5 ~ similarity_colours_8[2],
          `Similarity (%)` >= 62.5 & `Similarity (%)` < 75.0 ~ similarity_colours_8[3],
          `Similarity (%)` >= 50.0 & `Similarity (%)` < 62.5 ~ similarity_colours_8[4],
          `Similarity (%)` >= 37.5 & `Similarity (%)` < 50.0 ~ similarity_colours_8[5],
          `Similarity (%)` >= 25.0 & `Similarity (%)` < 37.5 ~ similarity_colours_8[6],
          `Similarity (%)` >= 12.5 & `Similarity (%)` < 25.0 ~ similarity_colours_8[7],
          `Similarity (%)` >= 0.00 & `Similarity (%)` < 12.5 ~ similarity_colours_8[8],
          TRUE ~ mid_green
        )
      )

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
        popup = ~ paste0("<b>", Language, "</b><br>", `Similarity (%)`, "%")
      )
  })
}

shinyApp(ui = ui, server = server)
