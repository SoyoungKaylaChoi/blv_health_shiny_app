library(shiny)
library(readxl)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(xtable)
                                  
cesd <- function(x) {
  if (x == 0){
    result <- "Not all days or less than 1 day"
  }
  else if (x == 1){
    result <- "1-2 days"
  }
  else if (x == 2){
    result <- "3-4 days"
  }
  else if (x == 3){
    result <- "5-7 days"
  }
  return(result)
}

eye <- function(x) {
  if (x == 1){
    result <- "Cataracts"
  }
  else if (x == 2){
    result <- "Glaucoma"
  }
  else if (x == 3){
    result <- "Macular degeneration"
  }
  else if (x == 4){
    result <- "Diabetic retinopathy"
  }
  else if (x == 5){
    result <- "Retinitis pigmentoda"
  }
  else if (x == 6){
    result <- "Others"
  }
  return(result)
}

calculate_stats <- function(x) {
    c(
    N = length(x),
    Mean = round(mean(x, na.rm = TRUE), 2),
    `Std. dev.` = round(sd(x, na.rm = TRUE), 2),
    Min = min(x, na.rm = TRUE),
    `25 %` = quantile(x, 0.25, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    `75 %` = quantile(x, 0.75, na.rm = TRUE),
    Max = max(x, na.rm = TRUE)
  )
}

name_of_var <- function(x) {
  result <- case_when(
    x == "Age" ~ "age",
    x == "Years of Living With Vision Impairment" ~ "vi_years",
    x == "Gender" ~ "gender",
    x == "Race" ~ "race",
    x == "Visual Impairment Level" ~ "vi_severity",
    x == "Area of Residence" ~ "residence",
    x == "Marital Status" ~ "marital_status",
    x == "Employment" ~ "employment",
    x == "Education Level" ~ "education",
    x == "None" ~ "",
    x == "Visual Acuity With Best lense" ~ "va", 
    x == "Perceive Light" ~ "lp",
    x == "Visual Impairment Change" ~ "vi_stability", 
    x == "Tasks Require Vision" ~ "vi_functioning",
    x == "Residence" ~ "residence",
    x == "Marital Status2" ~ "marital_simplified",
    x == "Education Level2" ~ "education_simplified"
  )
}

qes_index <- function(x) {
  result <- case_when(
    x == "Health Literacy Questionnaire" ~ "hlq_index", 
    x == "eHealth Literacy Questionnaire" ~ "eheal_index",
    x == "Health-Promoting Lifestyle Profile" ~ "hplp_index",
    x == "General Self-Efficacy Scale" ~ "gse_index", 
    x == "Center for Epidemiologic Studies Depression Scale" ~ "cesd_index"
  )
}

qes_score <- function(x){
  result <- case_when(
    x == "Health Literacy Questionnaire" ~ "hql_total", 
    x == "eHealth Literacy Questionnaire" ~ "eheal_total",
    x == "Health-Promoting Lifestyle Profile" ~ "hplp_total",
    x == "General Self-Efficacy Scale" ~ "gse_total", 
    x == "Center for Epidemiologic Studies Depression Scale" ~ "cesd_total"
  )
}

# import the dataset
path_1 <- "~/Desktop/Health/PVI_Survey_Data_Ver2.xlsx"
path_2 <- "C:/Users/Windooows/Desktop/HealthLab/PVI_Survey_Data_Ver2.xlsx"
PVI_1 <-read_excel(path_1, 
                   sheet = "PVI Survey Data", col_types = c("date", 
                                                            "text", "text", "text", "text", "text", 
                                                            "text", "text", "text", "text", "text", 
                                                            "text", "text", "text", "text", "text", 
                                                            "text", "text", "text", "text", "text", 
                                                            "text", "text", "text", "text", "text", 
                                                            "text", "text", "text", "text", "text", 
                                                            "text", "text", "text", "text", "text", 
                                                            "text", "text", "text", "text", "text", 
                                                            "text", "text", "text", "text", "text", 
                                                            "text", "text", "text", "text", "text", 
                                                            "text", "text", "text", "text", "text", 
                                                            "text", "text", "text", "text", "text", 
                                                            "text", "text", "text", "text", "text", 
                                                            "text", "text", "text", "text", "text", 
                                                            "text", "text", "text", "text", "text", 
                                                            "text", "text", "text", "text", "text", 
                                                            "text", "text", "text", "text", "text", 
                                                            "text", "text", "text", "text", "text", 
                                                            "text", "text", "text", "text", "text", 
                                                            "text", "text", "text", "text", "text", 
                                                            "text", "text", "text", "text", "text", 
                                                            "text", "text", "text", "text", "text", 
                                                            "text", "text", "text", "text", "text", 
                                                            "text", "text", "text", "text", "text", 
                                                            "text", "text", "text", "text", "text", 
                                                            "text", "text", "text", "text", "text", 
                                                            "text", "text", "text", "text", "text", 
                                                            "text", "text", "text", "text", "text", 
                                                            "text", "text", "text", "text", "text", 
                                                            "text", "text", "text", "text", "text", 
                                                            "text", "text", "text", "text", "text"))
PVI_des <- read_excel(path_1)
PVI <- PVI_1[-1, ]

# First, split the eye_disorder column because multiple eye disorders
# If there is no more eye disorders, then use the value 0
PVI_eye <- str_split(PVI$eye_disorder, ",", simplify = TRUE)
PVI_eye <- lapply(as.data.frame(PVI_eye), function(x) {
  as.numeric(as.character(x))
})
PVI_eye <- as.data.frame(PVI_eye)
PVI_eye[is.na(PVI_eye)] <- 0

# Do the same thing for diagnosis
PVI_diag <- str_split(PVI$diagnosis, ",", simplify = TRUE)
PVI_diag <- lapply(as.data.frame(PVI_diag), function(x) {
  as.numeric(as.character(x))
})
PVI_diag <- as.data.frame(PVI_diag)
PVI_diag[is.na(PVI_diag)] <- 0

# Do the same thing for accessible technologies
PVI_at <- str_split(PVI$at, ",", simplify = TRUE)
PVI_at <- lapply(as.data.frame(PVI_at), function(x) {
  as.numeric(as.character(x))
})
PVI_at <- as.data.frame(PVI_at)
PVI_at[is.na(PVI_at)] <- 0

# set all wrong valuers into -1
PVI <- PVI %>%
  mutate(birth_year = as.numeric(ifelse(birth_year == "1900" | !grepl("^\\d{4}$", birth_year), -1, birth_year)),
         vi_years = case_when(
           vi_years == "Since birth" ~ as.character(2023 - birth_year),
           vi_years == "since birth" & vi_severity == "3.0" ~ as.character(2023 - birth_year),
           vi_years == "since birth" & vi_severity == "2.0" ~ as.character(2023 - birth_year),
           vi_years == "I don't want to answer that question" | vi_years == "1 8" ~ "-1",
           vi_years == "Since six months of age" ~ as.character(2023 - birth_year),
           vi_years == "Moderate myopia" ~ "-2",
           TRUE ~ vi_years
         ))
PVI$vi_years <- gsub(" years", "", PVI$vi_years)
PVI$vi_years <- as.numeric(PVI$vi_years)

# calculate the health scores
PVI[5:140] <- lapply(PVI[5:140], as.numeric)
PVI <- PVI %>%
  mutate(hql_total = rowSums(.[, 5:48]),
         eheal_total = rowSums(.[, 51:58]),
         hplp_total = rowSums(.[, 59:110]),
         gse_total = rowSums(.[, 111:120]),
         cesd_total = rowSums(.[, 121:140]))

# calculate ages
PVI <- PVI %>%
  mutate(age = ifelse(birth_year == -1, -1, 2023 - birth_year))

# numeric
PVI[141:153] <- lapply(PVI[141:153], as.numeric)

# categorical
PVI <- PVI %>%
  mutate(gender = case_when(gender == 1 ~ "Male",
                            gender == 2 ~ "Female",
                            gender == 3 ~ "Non-binary",
                            TRUE ~ "Decline"),
         residence = case_when(residence == 1 ~ "Urban",
                               residence == 2 ~ "Suburban",
                               residence == 3 ~ "Rural",
                               TRUE ~ "Decline"),
         marital_simplified = case_when(marital_status == 1 ~ "Married",
                                        marital_status == 5 ~ "Single",
                                        marital_status == 2 | marital_status == 3 | marital_status == 4 ~ "Others",
                                        TRUE ~ "Decline"),
         marital_status = case_when(marital_status == 1 ~ "Married",
                                    marital_status == 2 ~ "Widowed",
                                    marital_status == 3 ~ "Divorced",
                                    marital_status == 4 ~ "Seperated",
                                    marital_status == 5 ~ "Single",
                                    TRUE ~ "Decline"),
         race = case_when(race == 1 ~ "Black",
                          race == 2 ~ "White", 
                          race == 3 ~ "Asian",
                          race == 4 ~ "Native Hawaiin",
                          race == 5 ~ "American Indian",
                          TRUE ~ "Decline"),
         employment = case_when(employment == 1 ~ "Full time",
                                employment == 2 ~ "Part time",
                                employment == 3 ~ "Unemployed",
                                employment == 4 ~ "Retired",
                                TRUE ~ "Decline"),
         education_simplified = case_when(education == 1 | education == 2 ~ "Less or equal to high school",
                                          education == 3 | education == 4 | education == 5 ~ "Equivalent to bachelor",
                                          education == 6 | education == 7 | education == 8 ~ "Master or higher",
                                          TRUE ~ "Decline"),
         education = case_when(education == 1 ~ "Less than high school",
                                 education == 2 ~ "High school diploma or equivalent",
                                 education == 3 ~ "Some college but no degree",
                                 education == 4 ~ "Associate's degree",
                                 education == 5 ~"Bachelor's degree",
                                 education == 6 ~ "Master's degree",
                                 education == 7 ~ "Doctoral degree",
                                 education == 8 ~ "professional degree",
                                 TRUE ~ "Decline"),
         vi_severity = case_when(vi_severity == 1 ~ "Moderate low",
                                 vi_severity == 2 ~ "Severe low",
                                 vi_severity == 3 ~ "Blindness",
                                 TRUE ~ "Decline"),
         va = case_when(va == 1 ~ "Better than 20/60",
                        va == 2 ~ "Worse than 20/60 but better than 20/200",
                        va == 3 ~ "20/200 or worsse",
                        TRUE ~ "Decline"),
         lp = case_when(lp == 1 ~ "Yes",
                        lp == 2 ~ "No",
                        TRUE ~ "Decline"),
         vi_stability = case_when(vi_stability == 1 ~ "Improved",
                                  vi_stability == 2 ~ "Remained",
                                  vi_stability == 3 ~ "Worsen",
                                  TRUE ~ "Decline"),
         vi_functioning = case_when(vi_functioning == 1 ~ "minor difficulty",
                                    vi_functioning == 2 ~ "significant difficulty",
                                    vi_functioning == 3 ~ "Unable to do vision tasks",
                                    TRUE ~ "Decline"))

PVI[142:150] <- lapply(PVI[142:150], as.factor)
PVI$vi_stability <- as.factor(PVI$vi_stability)
PVI$vi_functioning <- as.factor(PVI$vi_functioning)
PVI$marital_simplified <- as.factor(PVI$marital_simplified)
PVI$education_simplified <- as.factor(PVI$education_simplified)

# adjust the scale
PVI <- PVI %>%
  mutate(hlq_index = case_when(
    hql_total <= 88 ~ "Low",
    hql_total > 88 & hql_total <= 132 ~ "Medium",
    TRUE ~ "High"
  ),
  eheal_index = case_when(
    eheal_total < 19 ~ "Low",
    eheal_total >=19 & eheal_total < 30 ~ "Medium",
    TRUE ~ "High"
  ),
  hplp_index = case_when(
    hplp_total <= 90 ~ "Poor",
    hplp_total > 90 & hplp_total <= 129 ~ "moderate",
    hplp_total > 129 & hplp_total <= 168 ~ "good",
    TRUE ~ "excellent"
  ),
  gse_index = case_when(
    gse_total <= 20 ~ "Low",
    gse_total > 20 & gse_total <= 30 ~ "Medium",
    TRUE ~ "High"
  ),
  cesd_index = case_when(
    cesd_total < 15 ~ "No Depression",
    cesd_total >=15 & cesd_total < 21 ~ "Mild Depression",
    TRUE ~ "High Depression"
  )
  )
PVI$hlq_index <- factor(PVI$hlq_index, levels = c("Low", "Medium", "High"), ordered = TRUE)
PVI$eheal_index <- factor(PVI$eheal_index, levels = c("Low", "Medium", "High"), ordered = TRUE)
PVI$hplp_index <- factor(PVI$hplp_index, levels = c("Poor", "moderate", "good", "excellent"), ordered = TRUE)
PVI$gse_index <- factor(PVI$gse_index, levels = c("Low", "Medium", "High"), ordered = TRUE)
PVI$cesd_index <- factor(PVI$cesd_index, levels = c("No Depression", "Mild Depression", "High Depression"), ordered = TRUE)

PVI <- PVI %>%
  mutate(vi_yearsC = ifelse(age == vi_years, "since birth", "after birth")) %>%
  mutate(vi_yearsC = as.factor(vi_yearsC))

PVI_cleaned <- PVI %>%
  select(gender, residence, marital_simplified, race, employment, education_simplified, 
         vi_severity, va, lp, vi_stability, vi_functioning, hplp_index, cesd_index)
PVI_cleaned <- lapply(PVI_cleaned, as.character)
PVI_cleaned <- as.data.frame(PVI_cleaned)
PVI_cleaned <- PVI_cleaned %>%
  rowwise() %>%
  filter(!any(c_across(everything()) == "Decline")) %>%
  ungroup()
PVI_cleaned <- as.data.frame(lapply(PVI_cleaned, (as.factor)))
PVI_cleaned <- PVI_cleaned %>%
  filter(gender == "Male" | gender == "Female")
  

PVI_reg <- PVI %>%
  select(gender, age, gse_total, cesd_total, vi_severity, hql_total, hplp_total, eheal_total) %>%
  filter(gender == "Male" | gender == "Female") %>%
  filter(age != -1 & vi_severity != "Decline") %>%
  mutate(gender = fct_drop(gender), vi_severity = fct_drop(vi_severity))

# begin of the shiny app
ui <- fluidPage(
  div(h2("Questionnaire Data of Adults With Visual Disabilities", style = "text-align: center;"), style = "margin-bottom: 20px;"),
  titlePanel("Introduction", windowTitle = "ShinyApp Example"),
  
  sidebarLayout(
    NULL,
    mainPanel(
              p(HTML("Dataset for this app is from an online survey that investigated
              health literacy, eHealth literacy, and health behaviors among adults with
              visual disabilities with health disparities researchers."), 
                style = "font-size: 17px; font-family: 'Arial, sans-serif';")
    ),
    position = "right", #position set to different place to show the adjustment of sidebar
    fluid = TRUE
  ),
  
  sidebarLayout(
    sidebarPanel(
      div(
        style = "font-family: 'Arial, sans-serif';",
        selectInput("freq_category", "Select a category: ",
                  # we may change the description here to make it more readable
                   choices = c(PVI_des$`Variable Name`[144], PVI_des$`Variable Name`[146:157])
                  ),
      # dynamically generate the second input based on the first input
      uiOutput("status_ui"),
      helpText("The highlighted column would be the column you choosed at the second choice.")
      ),
      width = 4,
    ),
    mainPanel(
      p(HTML("<b>Exploratory Data Analysis</b>"), 
        style = "font-size: 20px; font-family: 'Arial, sans-serif';"),
      p("Frequency Plot", 
        style = "font-size: 18px; font-family: 'Arial, sans-serif';"),
      plotOutput("Freq_table")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      div(
        style = "font-family: 'Arial, sans-serif';",
        "Summary for scores of each questionaire"
        ),
      width = 4,
    ),
    mainPanel(
      p("Summary Table", 
        style = "font-size: 18px; font-family: 'Arial, sans-serif';"),
      tableOutput("summary_table")
      
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      div(
        style = "font-family: 'Arial, sans-serif';",
        "Mean Score for Different Groups",
        selectInput("trend_category", "Select a Questionnaire",
                    choices = c("Health Literacy Questionnaire", "eHealth Literacy Questionnaire",
                                "Health-Promoting Lifestyle Profile", "General Self-Efficacy Scale", 
                                "Center for Epidemiologic Studies Depression Scale")),
        selectInput("trend_var1", "Select a Variable",
                    choices = c("Age", "Years of Living With Vision Impairment", 
                                "Visual Impairment Level", "Education Level", "Employment")),
        selectInput("trend_var2", "Select Another Variable If You Want",
                    choices = c("None", "Gender", "Race", "Area of Residence",
                                "Marital Status")),
        uiOutput("trend_ui"),
        actionButton(inputId = "trend_button", label = "Submit Choices"),
        helpText("By adding another variable, 
                you could restrict ther first variable into a more specific group. 
                Total mean score of the questionnaire is used as a reference line 
                and observations less than 5 is labelled on the plot.
                The default plot is the mean health score of Health Literacy Questionnaire among different ages.")
      ),
      width = 4,
    ),
    mainPanel(
      p("Trend Plot", 
        style = "font-size: 18px; font-family: 'Arial, sans-serif';"),
      plotOutput("trend_plot")
    )
  ),
  
  sidebarLayout(sidebarPanel(
   div( 
     style = "font-family: 'Arial, sans-serif';",
    "Descriptive Table for Different Groups",
    selectInput("group1_var1", "Select a Variable",
                choices = c("Gender", "Residence", "Marital Status",
                            "Race", "Employment", "Education Level", 
                            "Visual Impairment Level", 
                            "Visual Acuity With Best lense", 
                            "Perceive Light", "Visual Impairment Change", 
                            "Tasks Require Vision"))
    ),
   width = 4,
    ),
    mainPanel(
      p("Grouped Summary Table", 
        style = "font-size: 18px; font-family: 'Arial, sans-serif';"),
      tableOutput("group1_table")
  )
  ),
  
  sidebarLayout(sidebarPanel(
    div(
      style = "font-family: 'Arial, sans-serif';",
      h3("Heatmap for Categorical Variables"),
      helpText("This is a heatmap for correlations")
    ),
    width = 4,
  ),
  mainPanel(
    p(HTML("<b>Data Visualization</b>"), 
      style = "font-size: 20px; font-family: 'Arial, sans-serif';"),
    p("Heatmap Discrete", 
      style = "font-size: 18px; font-family: 'Arial, sans-serif';"),
    plotOutput("heatmap1")
  )
  ),
  
  sidebarLayout(sidebarPanel(
    div(
      style = "font-family: 'Arial, sans-serif';",
      h3("Heatmap for Continuous Variables"),
      helpText("This is a heatmap for correlations")
    ),
    width = 4,
  ),
  mainPanel(
    p("Heatmap Continuous", 
      style = "font-size: 18px; font-family: 'Arial, sans-serif';"),
    plotOutput("heatmap2")
  )
  ),
  
  sidebarLayout(
    sidebarPanel(
      div(
        style = "font-family: 'Arial, sans-serif';",
        h3("Distribution Plot"),
       selectInput("tree_var2", "Select a Response:", choices = c(
          "General Self-Efficacy Scale",
          "Health-Promoting Lifestyle Profile",
          "Center for Epidemiologic Studies Depression Scale"
        ), selected = "Health Literacy Questionnaire"),
        selectInput("tree_var1", "Select a Variable",
                    choices = c("Age", "Gender", "Residence", "Marital Status",
                              "Race", "Employment", "Education Level", 
                              "Visual Impairment Level", 
                              "Visual Acuity With Best lense", 
                              "Perceive Light", "Visual Impairment Change", 
                              "Tasks Require Vision"), selected = "Age")
        ),
      width = 4,
    ),
    mainPanel(
      p("Distribution Plot", 
        style = "font-size: 18px; font-family: 'Arial, sans-serif';"),
      plotOutput("treePlot")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      div(
        style = "font-family: 'Arial, sans-serif';",
        h3("Poisson Regression Model Summary"),
        actionButton("runModel1", "Run Poisson Model"),
        selectInput("response1", "Select a Response:", choices = c(
          "Health Literacy Questionnaire", "eHealth Literacy Questionnaire",
          "Health-Promoting Lifestyle Profile"
        ), selected = "Health Literacy Questionnaire")),
      width = 4,
    ),
    mainPanel(
      p(HTML("<b>Professional Analysis</b>"), 
        style = "font-size: 20px; font-family: 'Arial, sans-serif';"),
      p("Poisson Regression", 
        style = "font-size: 18px; font-family: 'Arial, sans-serif';"),
      uiOutput("ModelSummary1"),
      tabsetPanel(type = "tabs",
                  tabPanel("Residuals vs Fitted", plotOutput("residVsFitted")),
                  tabPanel("Normal Q-Q", plotOutput("qqPlot")),
                  tabPanel("Scale-Location", plotOutput("scaleLocation")),
                  tabPanel("Residuals vs Leverage", plotOutput("residVsLeverage")),
                  tabPanel("Cook's distance Plot", plotOutput("CookDistance")),
                  tabPanel("Overdispersion Test", tableOutput("Overdispersion"))
      )
    )
  )
)

server <- function(input, output, session){
  
# frequency plot
  output$status_ui <- renderUI({
    # create the second selectInput with these unique values
    if (input$freq_category %in% PVI_des$`Variable Name`[144:157]){
      selected_col <- input$freq_category
      statuses <- unique(PVI[[selected_col]])
      selectInput("status", "Choose a variable: ", choices = statuses[statuses != -1], selected = statuses[statuses != -1][1])
    }
    else if (input$freq_category == "eye_disorder") {
      statuses <- unique(PVI_eye[[1]])
      selectInput("status", "Choose a variable: ", choices = statuses, selected = statuses[statuses != -1][1])
    }
    else if (input$freq_category == "diagnosis") {
      statuses <- unique(PVI_diag[[1]])
      selectInput("status", "Choose a variable: ", choices = statuses, selected = statuses[statuses != -1][1])
    }
    else if (input$freq_category == "at") {
      statuses <- unique(PVI_at[[1]])
      selectInput("status", "Choose a variable: ", choices = statuses, selected = statuses[statuses != -1][1])
    }
  })
  
  
  output$Freq_table <- renderPlot({
    if (!is.null(input$freq_category) && input$freq_category != "" ){
      if (input$freq_category %in% PVI_des$`Variable Name`[144:157]){
        # create the frequency table
        freq_table <- table(PVI[[input$freq_category]])
        freq_table <- as.data.frame(freq_table)
        colnames(freq_table) <- c("Value", "Frequency")
        if (!is.null(input$status) && nrow(freq_table) > 0){
          freq_table <- freq_table %>%
            filter(Value != -1) %>%
            mutate(status = ifelse(Value == input$status, "Selected", "Others"))
        }
        
        req(input$status)
        # plot part
        ggplot(freq_table, aes(x = Value,  y = Frequency, fill = status)) +
          geom_bar(stat = "identity") +
          coord_flip() +
          scale_fill_manual(values = c("Selected" = "orange", "Other" = "grey")) +
          labs(title = paste0("Frequency Plot for ", input$freq_category)) +
          theme_minimal() +
          theme(axis.text.x = element_text(size = 14))
          #coord_fixed(ratio = 60/1)
      }
      else if (input$freq_category == "eye_disorder") {
        all <- as.vector(t(PVI_eye))
        all <- as.data.frame(all)
        all <- all %>%
          group_by(all) %>%
          summarise(n = n()) %>%
          filter(all != 0)
        colnames(all) <- c("eye disorder", "frequency")
        ggplot(all, aes(x = `eye disorder`,  y = frequency)) +
          geom_bar(stat = "identity") +
          coord_flip() +
          labs(title = paste0("Frequency Plot for ", input$freq_category)) +
          theme_minimal()
      }
      else if (input$freq_category == "diagnosis"){
        all <- as.vector(t(PVI_diag))
        all <- as.data.frame(all)
        all <- all %>%
          group_by(all) %>%
          summarise(n = n()) %>%
          filter(all != 0)
        colnames(all) <- c("eye disorder", "frequency")
        ggplot(all, aes(x = `eye disorder`,  y = frequency)) +
          geom_bar(stat = "identity") +
          coord_flip() +
          labs(title = paste0("Frequency Plot for ", input$freq_category)) +
          theme_minimal()
      }
      else if (input$freq_category == "at"){
        all <- as.vector(t(PVI_at))
        all <- as.data.frame(all)
        all <- all %>%
          group_by(all) %>%
          summarise(n = n()) %>%
          filter(all != 0)
        colnames(all) <- c("eye disorder", "frequency")
        ggplot(all, aes(x = `eye disorder`,  y = frequency)) +
          geom_bar(stat = "identity") +
          coord_flip() +
          labs(title = paste0("Frequency Plot for ", input$freq_category)) +
          theme_minimal()
      }
    }
  })

# summary table  
  output$summary_table <- renderTable({
    Variable <- c("hlq", "eheals", "hplp", "gse", "cesd")
    name <- c("hql_total", "eheal_total", "hplp_total", "gse_total", "cesd_total")
    summary <- as.data.frame(lapply(PVI[name], calculate_stats))
    summary <- as.data.frame(t(rbind(Variable,summary)))
    names(summary) <- c("Questionnaire","N", "Mean", "Std. dev", "Min", "25%", "Median", "75%", "Max")
    summary
  })
  
# trend plot
  trend_selected_var1 <- reactive({
    name_of_var(input$trend_var1)
  })
  trend_selected_var2 <- reactive({
    name_of_var(input$trend_var2)
  })
  trend_title_var1 <- reactive({
    input$trend_var1
  })
  trend_title_var2 <- reactive({
    input$trend_var2
  })
  trend_title <- reactive({
    input$trend_category
  })
  mean_total <- reactiveVal()
  
  output$trend_ui <- renderUI({
    if (input$trend_var2 == "None"){
      selectInput("trend_status", "Choose a Variable Above First", choices = c("None"))
    } else {
      trend_status1 <- as.character(trend_selected_var2())[1]
      # max_trend_status2 <- max(PVI[[sym(trend_status1)]])
      trend_status2 <- unique(PVI[[sym(trend_status1)]])
      selectInput("trend_status", "Choose a Category:", choices = trend_status2[trend_status2 != "Decline"])
    }
  })
  
  observeEvent(input$trend_button, {
    var1 <- as.character(trend_selected_var1())[1]
    var2 <- as.character(trend_selected_var2())[1]
    tvar1 <- trend_title_var1()
    tvar2 <- trend_title_var2()
    ttitle <- trend_title()
    
    if (input$trend_category == "Health Literacy Questionnaire") {
      mean_total(mean(PVI$hql_total, na.rm = TRUE))
    } else if (input$trend_category == "eHealth Literacy Questionnaire") {
      mean_total(mean(PVI$eheal_total, na.rm = TRUE))
    } else if (input$trend_category == "Health-Promoting Lifestyle Profile"){
      mean_total(mean(PVI$hplp_total, na.rm = TRUE))
    } else if (input$trend_category == "General Self-Efficacy Scale") {
      mean_total(mean(PVI$gse_total, na.rm = TRUE))
    } else if (input$trend_category == "Center for Epidemiologic Studies Depression Scale") {
      mean_total(mean(PVI$cesd_total, na.rm = TRUE))
    }
    
    PVI_trend <- PVI %>%
      filter({
        if(var1 == "age"){
          age != -1
        } else if(var1 == "vi_years"){
          vi_years != -1 & vi_years != -2
        } else {
          TRUE
        }
      }) %>%
      filter({
        if (input$trend_var2 != "None" && !is.na(var2) && var2 != "" && var2 %in% names(PVI)){
          !!sym(var2) == input$trend_status
        } else {
          TRUE
        }
      }) %>%
      group_by(!!sym(var1)) %>%
      summarise(mean = case_when(
        input$trend_category == "Health Literacy Questionnaire" ~ round(mean(hql_total), 2),
        input$trend_category == "eHealth Literacy Questionnaire" ~ round(mean(eheal_total), 2),
        input$trend_category == "Health-Promoting Lifestyle Profile" ~ round(mean(hplp_total), 2),
        input$trend_category == "General Self-Efficacy Scale" ~ round(mean(gse_total), 2),
        input$trend_category == "Center for Epidemiologic Studies Depression Scale" ~ round(mean(cesd_total), 2)
      ),
      n = n()) %>%
      mutate(var11 = as.numeric(!!sym(var1)), perc = n / nrow(PVI) * 100)
    output$trend_plot <- renderPlot({
      ggplot(PVI_trend, aes(x = var11, y = mean))+
        geom_line(color = "#37A1F4", size = 1) +
        geom_point(color = "#37A1F4", shape = 15, size = 2) +
        geom_text(aes(label = ifelse(n < 5, n, ""))
                      , vjust = -0.5, color = "black") +
        geom_hline(yintercept = mean_total(), linetype = "dashed", color = "red") +
        theme_minimal() +
        theme(panel.background = element_rect(fill = "#EEEFEF", colour = NA)) +
        labs(x = tvar1, y = "Mean Score", title = paste0("Mean Health Score of ", ttitle,
                                                                    " in ", tvar1, " Groups"))
    })
  })
  # set the default plot
  output$trend_plot <- renderPlot({
    PVI_trend2 <- PVI %>% 
      filter(age != -1) %>%
      group_by(age) %>%
      summarise(mean = round(mean(hql_total), 2), n = n())
    
      ggplot(PVI_trend2, aes(x = age, y = mean))+
        geom_line(color = "#37A1F4", size = 1) +
        geom_point(color = "#37A1F4", shape = 15, size = 2) +
        geom_text(aes(label = ifelse(n < 5, n, ""))
                  , vjust = -0.5, color = "black") +
        geom_hline(yintercept = 135.83, linetype = "dashed", color = "red") +
        theme_minimal() +
        theme(panel.background = element_rect(fill = "#EEEFEF", colour = NA)) +
        labs(x = input$trend_var1, y = "Mean Score", title = "Mean Health Score of Health Literacy Questionnaire in Age Groups")
  })

# description table
  output$group1_table <- renderTable(
    PVI %>%
      group_by(!!sym(name_of_var(input$group1_var1))) %>%
      filter(!!sym(name_of_var(input$group1_var1)) != "Decline") %>%
      summarise(
        Mean_hlq = mean(hql_total),
        Std_hlq = sd(hql_total),
        Mean_eheal = mean(eheal_total),
        Std_eheal = sd(eheal_total),
        Mean_hplp = mean(hplp_total),
        Std_hplp = sd(hplp_total),
        Mean_gse = mean(gse_total),
        Std_gse = sd(gse_total),
        Mean_cesd = mean(cesd_total),
        Std_cesd = sd(cesd_total),
        N = n()
      )
  )
  output$heatmap1 <- renderPlot({
    
    PVI_cleaned <- data.frame(lapply(PVI_cleaned, factor))
    cramersV_matrix <- matrix(data = NA, nrow = ncol(PVI_cleaned), ncol = ncol(PVI_cleaned),
                              dimnames = list(names(PVI_cleaned), names(PVI_cleaned)))
    
    for (i in 1:ncol(PVI_cleaned)) {
      for (j in 1:ncol(PVI_cleaned)) {
        tbl <- table(PVI_cleaned[, i], PVI_cleaned[, j])
        
        # Calculate expected frequencies
        row_sums <- margin.table(tbl, 1)
        col_sums <- margin.table(tbl, 2)
        expected <- outer(row_sums, col_sums) / sum(tbl)
          test <- chisq.test(tbl)
          cramersV_matrix[i, j] <- sqrt(test$statistic / (sum(tbl) * (min(ncol(tbl) - 1, nrow(tbl) - 1))))
    }}

    cramersV_melted <- melt(cramersV_matrix)

    ggplot(cramersV_melted, aes(Var1, Var2, fill = value)) +
      geom_tile() +
      geom_text(aes(label = round(value, 2)), size = 3) +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0.5, limit = c(0, 1), space = "Lab", name="Cramér's V") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
            axis.title = element_blank())
  })
  
  output$heatmap2 <- renderPlot({
    data_cor2 <- cbind(PVI[ ,157:162], vi_years = PVI$vi_years)
    data_cor2 <- data_cor2 %>%
      filter(vi_years != -2 & vi_years != -1)
    cor_matrix2 <- cor(data_cor2)
    
    correlationMatrixMelted <- melt(cor_matrix2)

    ggplot(data = correlationMatrixMelted, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile() +
      geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                           midpoint = 0, limit = c(-1, 1), space = "Lab", 
                           name="Pearson\nCorrelation") +
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.title = element_blank())

  })
  
  model1 <- eventReactive(input$runModel1, {
    glm(as.formula(paste(qes_score(input$response1), "~ .")), family = "poisson", data = PVI_reg)
  })
  
  output$ModelSummary1 <- renderUI({
    if (input$runModel1 > 0) {
      model <- model1()
      coefs <- summary(model)$coefficients
      coefs_df <- as.data.frame(coefs)
      
      # Generate an HTML table using xtable
      coefs_table <- xtable(coefs_df, digits = 4)
      HTML(print(coefs_table, type = "html"))
    }
  })
  
  output$residVsFitted <- renderPlot({
    input$runModel1
    plot(residuals(model1(), type = "pearson") ~ fitted(model1()), 
         xlab = "Fitted values", ylab = "Pearson Residuals")
    abline(h = 0, col = "red")
  })
  
  # Plot: Normal Q-Q
  output$qqPlot <- renderPlot({
    input$runModel1
    qqnorm(residuals(model1(), type = "pearson"))
    qqline(residuals(model1(), type = "pearson"), col = "red")
  })
  
  # Plot: Scale-Location
  output$scaleLocation <- renderPlot({
    input$runModel1
    plot(sqrt(abs(residuals(model1(), type = "pearson"))) ~ fitted(model1()), 
         xlab = "Fitted values", ylab = "Square root of the absolute value of Pearson residuals")
    abline(h = 0, col = "red")
  })
  
  # Plot: Residuals vs Leverage
  output$residVsLeverage <- renderPlot({
    input$runModel1
    plot(hatvalues(model1()) ~ residuals(model1(), type = "pearson"), 
         xlab = "Leverage", ylab = "Pearson Residuals")
    abline(h = 0, col = "red")
  })
  
  output$CookDistance <- renderPlot({
    input$runModel1
    plot(model1(), which = 4)
  })
  
  output$Overdispersion <- renderTable({
    input$runModel1
    model <- model1()
    overdispersion_ratio <- model$deviance / model$df.residual
    p <- pchisq(model$deviance, model$df.residual, lower.tail = FALSE)
    disp <- data.frame(`Overdispersion Ratio` = overdispersion_ratio, `p-value` = p)
    disp
  })
  
  output$treePlot <- renderPlot({
    tree_var1 <- name_of_var(input$tree_var1)
    tree_var2 <- qes_index(input$tree_var2)
    values_tree <- list(cesd_index = c("No Depression" = "#DF3B02", "Mild Depression" = "#A02BAB",
                                                                        "High Depression" = "#0E88A9"),
              gse_index = c("Low" = "#DF3B02", "Medium" = "#A02BAB",
                            "High" = "#0E88A9"),
              hplp_index = c("Poor" = "#00EDBB", "moderate" = "#00A9ED",
                             "good" = "#ED6C00", "excellent" == "#DF3B02"))
    tree_color <- values_tree[[tree_var2]] %||% c("Level1" = "grey")
    tree_data <- PVI %>%
      group_by(!!sym(tree_var1), vi_years) %>%
      mutate(count = n()) %>%
      ungroup()
    ggplot(tree_data, aes(x = !!sym(tree_var1), y = vi_years, color = !!sym(tree_var2))) + 
             geom_point(aes(alpha = count)) +
      scale_color_manual(values = tree_color) +
      theme_minimal() +
      labs(color = "CESD Levels", x = input$tree_var1, y = "Years of Vision Impairment")
  })
  
}


shinyApp(ui = ui, server = server)
 
# gender (M F), age, gse socres, csed scores, serverity of vi,  hlq scores, hplp scores, eheal scores
# QQ plot, residuals vs fitted, scale-location, residuals vs leverage












