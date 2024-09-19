library(googlesheets4)
library(shiny)
library(shinyjs)
library(plotly)
library(dplyr)
library(lubridate)

# UI for the Shiny app
ui <- fluidPage(
  titlePanel(title = span(img(src = "NASCENT_logo_horizontal.jpg", height = 50), "Scenario content coding survey")),
  useShinyjs(),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("language", "Language / Idioma", choices = c("English" = "en", "Español" = "es")),
      
      # Initial panel to capture name and start the introduction
      conditionalPanel(
        condition = "!output.showSurvey && !output.showSubmissionInProgress && !output.showThankYou && !output.showIntroduction",
        textInput("participant_name", "Name / Nombre"),
        checkboxInput("checkbox", "Check if you do NOT want your Name to be published on the NASCENT-Peru Website / Marque si NO desea que su nombre se publique en el sitio web de NASCENT-Peru"),
        actionButton("start_button", "Start Introduction / Comenzar Introducción"),
        textOutput("warning")
      ),
      
      conditionalPanel(
        condition = "output.showSurvey",
        br(), br(),
        textOutput("statement_title"),
        textOutput("statement"),
        tags$head(tags$style(HTML("#statement_title {font-weight: bold;} #statement {font-size: 20px;}"))),
        br(),
        textOutput("explanation_title"),
        textOutput("explanation"),
        tags$head(tags$style(HTML("#explanation_title {font-weight: bold;} #explanation {font-size: 18px;}"))),
        br(), br(),
        uiOutput("unsure_button_ui", style = 'display: inline-block; margin-left: 15px;'),
        uiOutput("next_button_ui", style = 'display: inline-block;'),
        uiOutput("back_button_ui", style = 'display: inline-block;'),
        uiOutput("submit_button_ui", style = 'display: inline-block;'),
        textOutput("warning_submission"),
        br(),
        div(style = 'overflow-y:scroll; max-height:300px;', tableOutput("progress_table"))
      ),
      
      # Panel to show submission in progress
      conditionalPanel(
        condition = "output.showSubmissionInProgress",
        h3(textOutput("submit_progress_message"))
      ),
      
      # Panel to show the thank-you message after submission
      conditionalPanel(
        condition = "output.showThankYou",
        h3(textOutput("thank_you_message"))
      ),
      
      # Conditional panel for introduction content
      conditionalPanel(
        condition = "output.showIntroduction",
        uiOutput("intro_content")
      )
    ),
    
    mainPanel(
      plotlyOutput("ternaryPlot", height = "750px")
    )
  )
)

# Server for the Shiny app
server <- function(input, output, session) {
  # Set authentication token to be stored in a folder called .secrets
  options(gargle_oauth_cache = ".secrets")
  
  # Authenticate using the cached token
  gs4_auth(cache = ".secrets", email = "nascentperu@gmail.com")
  
  # Google Sheet URL
  ss <- "https://docs.google.com/spreadsheets/d/1bh6OJxxZrVjQGEv_ca76hDPKqgLTfQ3OzSNQA_X1nOs/edit?usp=sharing"
  
  # Function to read statements based on language
  read_statements <- function(language) {
    sheet_name <- paste0("statements_", language)
    read_sheet(ss, sheet = sheet_name) %>%
      select(Statement_ID, Statement, Explanation)
  }
  
  # Read both language statements at startup
  en_statements <- read_statements("en")
  es_statements <- read_statements("es")
  statements_data <- list(en = en_statements, es = es_statements)
  
  # Reactive value to store the selected language
  selected_language <- reactive(input$language)
  
  # Reactive value to store the statements based on selected language
  statements <- reactive({
    req(selected_language())
    statements_data[[selected_language()]]
  })
  
  # Reactive values to control the panel visibility
  showSubmissionInProgress <- reactiveVal(FALSE)
  showThankYou <- reactiveVal(FALSE)
  
  # Reactive value to control the survey display
  show_survey <- reactiveVal(FALSE)
  
  # Reactive value to control the introduction display
  show_introduction <- reactiveVal(FALSE)
  
  # Introduction steps
  intro_step <- reactiveVal(1)
  
  # Initialize reactiveValues
  rv <- reactiveValues(
    current_statement = 1,
    selected_points = data.frame(Statement_ID = integer(), A = numeric(), B = numeric(), C = numeric(), Statement = character(), stringsAsFactors = FALSE),
    progress = data.frame(
      Question = 1,
      Status = "Pending",
      stringsAsFactors = FALSE
    ),
    saved_data = NULL,
    last_valid_coords = data.frame(A = 0, B = 0, C = 0)
  )
  
  # Create grid points for the ternary plot
  grid_points <- expand.grid(
    A = seq(0, 30, by = 1),
    B = seq(0, 30, by = 1),
    C = seq(0, 30, by = 1)
  )
  grid_points <- grid_points[round(grid_points$A + grid_points$B + grid_points$C, 1) == 30, ]
  
  # Function to format the hover text
  hover_text <- function(df) {
    df %>%
      group_by(A, B, C) %>%
      summarize(Text = paste("Statement", Statement_ID, ": ", Statement, collapse = "<br>"), .groups = 'drop') %>%
      select(A, B, C, Text)
  }
  
  # Function to generate circle points in barycentric coordinates
  generate_circle_points_barycentric <- function(center, radius, n_points = 100) {
    angles <- seq(0, 2 * pi, length.out = n_points)
    lambda <- center[1]
    mu <- center[2]
    nu <- center[3]
    
    circle_points <- data.frame(
      a = lambda + radius * cos(angles),
      b = mu + radius * cos(angles + 2 * pi / 3),
      c = nu + radius * cos(angles - 2 * pi / 3)
    )
    
    circle_points <- circle_points[round(circle_points$a + circle_points$b + circle_points$c, 1) == 30, ]
    return(circle_points)
  }
  
  # Function to calculate linearly increasing opacities
  calculate_opacities_linear <- function(num_circles) {
    opacities <- numeric(num_circles)
    T_prev <- 0
    for (i in 1:num_circles) {
      T_i <- i / num_circles
      opacities[i] <- (T_i - T_prev) / (1 - T_prev)
      T_prev <- T_i
    }
    return(opacities)
  }
  
  # Define centers and radius for the circles (in barycentric coordinates)
  centers <- list(
    c(0, 0, 30),  # Red circle center
    c(30, 0, 0),  # Green circle center
    c(0, 30, 0)   # Blue circle center
  )
  radius <- 22  # Radius of the circles
  
  # Define colors with base color and calculate opacities
  num_circles <- 35
  opacities <- calculate_opacities_linear(num_circles)
  colors <- list(
    list(base_color = 'rgba(219, 114, 114, %.2f)', # Red color
         opacities = rev(opacities)),  # Reverse to start with highest opacity
    list(base_color = 'rgba(175, 194, 120, %.2f)', # Green color
         opacities = rev(opacities)),  # Reverse to start with highest opacity
    list(base_color = 'rgba(94, 135, 189, %.2f)', # Blue color
         opacities = rev(opacities))  # Reverse to start with highest opacity
  )
  
  # Generate concentric circles with varying opacities
  concentric_circles <- list()
  for (i in 1:length(centers)) {
    center <- centers[[i]]
    color_info <- colors[[i]]
    for (j in 1:length(color_info$opacities)) {
      opacity <- color_info$opacities[j]
      circle_color <- sprintf(color_info$base_color, opacity)
      points <- generate_circle_points_barycentric(center, radius * j / length(color_info$opacities))
      concentric_circles[[length(concentric_circles) + 1]] <- list(points = points, color = circle_color)
    }
  }
  
  # Function to adjust points to respect ternary constraints
  adjust_points <- function(points) {
    points$a <- pmin(pmax(points$a, 0), 30)
    points$b <- pmin(pmax(points$b, 0), 30)
    points$c <- 30 - points$a - points$b
    return(points)
  }
  
  # Adjust all concentric circle points
  concentric_circles <- lapply(concentric_circles, function(circle) {
    circle$points <- adjust_points(circle$points)
    return(circle)
  })
  
  # Reactive value to store the hover coordinates
  hover_coords <- reactiveVal(NULL)
  
  observe({
    statements_data <- statements()
    rv$progress <- data.frame(
      Question = 1:nrow(statements_data),
      Status = rep("Pending", nrow(statements_data)),
      stringsAsFactors = FALSE
    )
  })
  
  observeEvent(input$start_button, {
    if (input$participant_name == "") {
      output$warning <- renderText({
        if (selected_language() == "en") {
          "Please enter your name to start the survey."
        } else {
          "Por favor ingrese su nombre para comenzar la encuesta."
        }
      })
    } else {
      output$warning <- renderText("")
      show_introduction(TRUE)
      intro_step(1)
    }
  })
  
  observeEvent(input$next_intro_button, {
    step <- intro_step()
    if (step < 2) {
      intro_step(step + 1)
    }
  })
  
  observeEvent(input$back_intro_button, {
    step <- intro_step()
    if (step > 1) {
      intro_step(step - 1)
    }
  })
  
  observeEvent(input$start_survey_button, {
    show_introduction(FALSE)
    show_survey(TRUE)
  })
  
  output$showIntroduction <- reactive({
    show_introduction()
  })
  
  outputOptions(output, "showIntroduction", suspendWhenHidden = FALSE)
  
  output$showSurvey <- reactive({
    show_survey()
  })
  
  outputOptions(output, "showSurvey", suspendWhenHidden = FALSE)
  
  output$showSubmissionInProgress <- reactive({
    showSubmissionInProgress()
  })
  outputOptions(output, "showSubmissionInProgress", suspendWhenHidden = FALSE)
  
  output$showThankYou <- reactive({
    showThankYou()
  })
  outputOptions(output, "showThankYou", suspendWhenHidden = FALSE)
  
  output$intro_content <- renderUI({
    step <- intro_step()
    if (selected_language() == "en") {
      tagList(
        h2("Practical instructions"),
        span("Below is an image of what the survey panel will look like once you press the"), strong("Start survey"), span("button below."),
        tags$br(),
        p("The top of the panel shows the current statement to be coded with an accompanying explanation where necessary. Below this you will see four buttons:"),
        HTML("<ul><li> <b>Unsure</b> : This button is to be used for the statements you do not know how to allocate.
             </li><li> <b>Next</b>: This button will load the next statement.
             </li><li> <b>Back</b>: This button will load the previous statement.
             </li><li> <b> Submit</b>: This button will submit your answers at the end of the survey.</li></ul>"),
        tags$br(),
        p("The table below these buttons lists all of the statements to be allocated and a status for each statement:"),
        HTML("<ul><li> <b>Pending</b> : indicates that the allocation for this statement still needs to be done.
             </li><li> <b>Done</b>: indicates that the allocation of this statement is complete. </li></ul>"),
        span("Note: The "), strong("Submit"), span("button will only work if all statements have the status"), strong("done"), span("."),
        tags$br(),
        tags$br(),
        img(src = "www/Introduction/Intro_EN.png", height = "80%", width = "80%", style = "box-shadow: 0px 0px 5px black; display: block; margin-left: auto; margin-right: auto;"),
        tags$br(),
        tags$br(),
        actionButton("back_intro_button", "Back"),
        actionButton("start_survey_button", "Start Survey")
      )
    } else {
      tagList(
        h2("Instrucciones prácticas"),
        span("A continuación se muestra una imagen del aspecto que tendrá el panel de la encuesta una vez que pulse el botón"), strong("Comenzar Encuesta"), span("de abajo."),
        tags$br(),
        p("La parte superior del panel muestra la declaración actual que se va a codificar, con una explicación adjunta cuando sea necesario. Debajo, verá cuatro botones:"),
        HTML("<ul><li> <b>No seguro</b> : Este botón debe utilizarse para las declaraciones que no sabe cómo asignar.
              </li><li> <b>Siguiente</b>: Este botón cargará la siguiente declaración.
              </li><li> <b>Atrás</b>: Este botón cargará la declaración anterior.
              </li><li> <b>Enviar</b>: Este botón enviará sus respuestas al final de la encuesta.</li></ul>"),
        tags$br(),
        p("La tabla que aparece debajo de estos botones enumera todas las declaraciones que se van a asignar y el estado de cada una de ellas:"),
        HTML("<ul><li> <b>Pendiente</b> : Indica que aún no se ha realizado la asignación para esta declaración.
             </li><li> <b>Hecho</b>: Indica que la asignación de esta declaración está completa. </li></ul>"),
        span("Nota: El "), strong("Enviar"), span("botón sólo funcionará si todas las declaraciones tienen el estado"), strong("hecho"), span("."),
        tags$br(),
        tags$br(),
        img(src = "www/Introduction/Intro_ES.png", height = "80%", width = "80%", style = "box-shadow: 0px 0px 5px black; display: block; margin-left: auto; margin-right: auto;"),
        tags$br(),
        tags$br(),
        actionButton("back_intro_button", "Atrás"),
        actionButton("start_survey_button", "Comenzar Encuesta")
      )
    }
  })
  
  # Initial saved data with placeholders
  initial_data <- reactive({
    req(statements())
    data.frame(
      Statement_ID = statements()$Statement_ID,
      Statement = statements()$Statement,
      NAT_Coords = rep(NA, nrow(statements())),
      CUL_Coords = rep(NA, nrow(statements())),
      SOC_Coords = rep(NA, nrow(statements())),
      time = rep(NA, nrow(statements())),
      stringsAsFactors = FALSE
    )
  })
  
  observe({
    rv$saved_data <- initial_data()
  })
  
  # Next button observer
  observeEvent(input$next_button, {
    prog <- rv$progress
    if (prog$Status[rv$current_statement] == "Selected" || prog$Status[rv$current_statement] == "Seleccionado") {
      prog$Status[rv$current_statement] <- ifelse(selected_language() == "en", "Done", "Hecho")
      rv$progress <- prog
    }
    new_index <- rv$current_statement %% nrow(statements()) + 1
    rv$current_statement <- new_index
    # Scroll the table to the current statement
    session$sendCustomMessage("scrollToRow", new_index)
  })
  
  # Back button observer
  observeEvent(input$back_button, {
    new_index <- ifelse(rv$current_statement > 1, rv$current_statement - 1, nrow(statements()))
    rv$current_statement <- new_index
    # Scroll the table to the current statement
    session$sendCustomMessage("scrollToRow", new_index)
  })
  
  observeEvent(input$unsure_button, {
    coords <- data.frame(A = 0, B = 0, C = 0)
    current_statement_id <- rv$current_statement
    selected_points_df <- rv$selected_points
    
    # Remove existing point for the current statement
    selected_points_df <- selected_points_df[selected_points_df$Statement_ID != current_statement_id, ]
    
    # Add the new point for the current statement
    selected_points_df <- rbind(selected_points_df, data.frame(Statement_ID = current_statement_id, A = coords$A, B = coords$B, C = coords$C, Statement = statements()$Statement[current_statement_id]))
    rv$selected_points <- selected_points_df
    
    data <- rv$saved_data
    data[current_statement_id, c("NAT_Coords", "CUL_Coords", "SOC_Coords")] <- coords
    rv$saved_data <- data
    
    prog <- rv$progress
    if (prog$Status[rv$current_statement] == "Pending" || prog$Status[rv$current_statement] == "Pendiente") {
      prog$Status[rv$current_statement] <- ifelse(selected_language() == "en", "Selected", "Seleccionado")
    }
    rv$progress <- prog
  })
  
  output$statement_title <- renderText({
    if (selected_language() == "en") {
      paste("Statement", rv$current_statement, ":")
    } else {
      paste("Declaración", rv$current_statement, ":")
    }
  })
  
  output$statement <- renderText({
    statements()$Statement[rv$current_statement]
  })
  
  output$explanation_title <- renderText({
    if (selected_language() == "en") {
      "Explanation:"
    } else {
      "Explicación:"
    }
  })
  
  output$explanation <- renderText({
    statements()$Explanation[rv$current_statement]
  })
  
  output$unsure_button_ui <- renderUI({
    actionButton("unsure_button", ifelse(selected_language() == "en", "Unsure", "No seguro"))
  })
  
  output$next_button_ui <- renderUI({
    actionButton("next_button", ifelse(selected_language() == "en", "Next", "Siguiente"))
  })
  
  output$back_button_ui <- renderUI({
    actionButton("back_button", ifelse(selected_language() == "en", "Back", "Atrás"))
  })
  
  output$submit_button_ui <- renderUI({
    actionButton("submit_button", ifelse(selected_language() == "en", "Submit", "Enviar"))
  })
  
  # Render the submit progress message
  output$submit_progress_message <- renderText({
    if (selected_language() == "en") {
      "Your submission is in progress, please wait."
    } else {
      "Su envío está en progreso, por favor espere."
    }
  })
  
  # Render the thank-you message
  output$thank_you_message <- renderText({
    if (selected_language() == "en") {
      "Thank you for taking part in this survey. Your answers will help us greatly. You can close the application now."
    } else {
      "Gracias por participar en esta encuesta. Sus respuestas nos ayudarán enormemente. Puede cerrar la aplicación ahora."
    }
  })
  
  # Create the ternary plot function
  create_ternary_plot <- function(selected_points_df) {
    plot <- plot_ly(source = "ternaryPlot") %>%
      layout(
        showlegend = FALSE,
        hovermode = "closest", # Ensure closest point hovermode
        ternary = list(
          sum = 30,
          aaxis = list(title = "", min = 0, max = 30, tickvals = FALSE),
          baxis = list(title = ifelse(selected_language() == "en", "<b>Nature as Culture</b><br>Living in harmony<br>People one with nature", "<b>Naturaleza como Cultura</b><br>Viviendo en armonía<br>Personas una con la naturaleza"), min = 0, max = 30, tickvals = FALSE, titlefont = list(size = 14)),
          caxis = list(title = ifelse(selected_language() == "en", "<b>Nature for Society</b><br>Nature’s benefits to people<br>Ecosystem services", "<b>Naturaleza para la Sociedad</b><br>Beneficios de la naturaleza para las personas<br>Servicios ecosistémicos"), min = 0, max = 30, tickvals = FALSE, titlefont = list(size = 14)),
          bgcolor = "white",
          showgrid = FALSE
        ),
        annotations = list(
          list(
            x = 0.5, y = 1, text = ifelse(selected_language() == "en", "<b>Nature for Nature</b><br>Intrinsic value of nature<br>Space allocated for nature", "<b>Naturaleza por Naturaleza</b><br>Valor intrínseco de la naturaleza<br>Espacio asignado para la naturaleza"),
            showarrow = FALSE, xref = "paper", yref = "paper", xanchor = "center", yanchor = "bottom",
            font = list(size = 14)
          )
        ),
        margin = list(t = 100, b = 100, l = 50, r = 50)
      ) %>%
      config(displayModeBar = FALSE) # Disable plotly mode bar
    
    # Add concentric circles with varying opacities
    for (circle in concentric_circles) {
      plot <- plot %>%
        add_trace(
          type = "scatterternary",
          mode = "none",
          a = circle$points$a,
          b = circle$points$b,
          c = circle$points$c,
          fill = "toself",
          fillcolor = circle$color,
          showlegend = FALSE,
          hoverinfo = "none" # Disable hover info for circles
        )
    }
    
    # Add grid points with invisible markers
    plot <- plot %>% add_trace(
      type = "scatterternary",
      mode = "markers",
      a = grid_points$A,
      b = grid_points$B,
      c = grid_points$C,
      hoverinfo = "none", # Disable hover info for grid points
      marker = list(opacity = 0, size = 5) # Set opacity to 0 to make markers invisible
    )
    
    # Add selected points
    if (!is.null(selected_points_df) && nrow(selected_points_df) > 0) {
      hover_data <- hover_text(selected_points_df)
      plot <- plot %>% add_trace(
        type = "scatterternary",
        mode = "markers",
        a = hover_data$A,
        b = hover_data$B,
        c = hover_data$C,
        marker = list(size = 10, color = "red", opacity = 0.8),
        text = hover_data$Text,
        hoverinfo = "text",
        showlegend = FALSE
      )
    }
    
    plot
  }
  
  # Initial plot creation
  output$ternaryPlot <- renderPlotly({
    create_ternary_plot(rv$selected_points)
  })
  
  # Update points on the plot using plotlyProxy
  observe({
    selected_points_df <- rv$selected_points
    proxy <- plotlyProxy("ternaryPlot", session)
    
    # Update or add the selected points trace
    plotlyProxyInvoke(proxy, "deleteTraces", list(1))
    if (nrow(selected_points_df) > 0) {
      hover_data <- hover_text(selected_points_df)
      plotlyProxyInvoke(proxy, "addTraces", list(
        type = "scatterternary",
        mode = "markers",
        a = hover_data$A,
        b = hover_data$B,
        c = hover_data$C,
        marker = list(size = 10, color = "red", opacity = 0.8),
        text = hover_data$Text,
        hoverinfo = "text",
        showlegend = FALSE
      ))
    }
  })
  
  # Store hover information
  observeEvent(event_data("plotly_hover", source = "ternaryPlot"), {
    hover_info <- event_data("plotly_hover", source = "ternaryPlot")
    if (!is.null(hover_info) && hover_info$curveNumber != 106) {
      point_index <- hover_info$pointNumber + 1 # R indexes start at 1
      coords <- grid_points[point_index, ]
      hover_coords(coords) # Store the hover coordinates in reactive value
    }
  })
  
  # Observe clicks on the plot to directly capture coordinates
  observeEvent(event_data("plotly_click", source = "ternaryPlot"), {
    click_data <- event_data("plotly_click", source = "ternaryPlot")
    if (!is.null(click_data)) {
      if (click_data$curveNumber == 106) {
        # Use the hover coordinates if hover text is active
        coords <- hover_coords()
        if (is.null(coords)) {
          coords <- rv$last_valid_coords
        }
      } else {
        # Capture the click data
        point_index <- click_data$pointNumber + 1 # R indexes start at 1
        coords <- grid_points[point_index, ]
        
        # Update the last valid coordinates
        rv$last_valid_coords <- coords
      }
      current_statement_id <- rv$current_statement
      selected_points_df <- rv$selected_points
      
      # Remove existing point for the current statement
      selected_points_df <- selected_points_df[selected_points_df$Statement_ID != current_statement_id, ]
      
      # Add the new point for the current statement
      selected_points_df <- rbind(selected_points_df, data.frame(Statement_ID = current_statement_id, A = coords$A, B = coords$B, C = coords$C, Statement = statements()$Statement[current_statement_id]))
      rv$selected_points <- selected_points_df
      
      # Update the plot immediately
      hover_data <- hover_text(selected_points_df)
      proxy <- plotlyProxy("ternaryPlot", session)
      plotlyProxyInvoke(proxy, "deleteTraces", list(1)) # Remove existing points trace
      plotlyProxyInvoke(proxy, "addTraces", list(
        type = "scatterternary",
        mode = "markers",
        a = hover_data$A,
        b = hover_data$B,
        c = hover_data$C,
        marker = list(size = 10, color = "red", opacity = 0.8),
        text = hover_data$Text,
        hoverinfo = "text",
        showlegend = FALSE
      ))
      
      # Update the table
      data <- rv$saved_data
      data[rv$current_statement, c("NAT_Coords", "CUL_Coords", "SOC_Coords")] <- coords
      rv$saved_data <- data
      
      prog <- rv$progress
      if (prog$Status[rv$current_statement] == "Pending" || prog$Status[rv$current_statement] == "Pendiente") {
        prog$Status[rv$current_statement] <- ifelse(selected_language() == "en", "Selected", "Seleccionado")
      }
      rv$progress <- prog
    }
  })
  
  # Submit handler for the Google Sheet
  observeEvent(input$submit_button, {
    # First, hide the survey panel and show the "submission in progress" message
    show_survey(FALSE)  # Hide survey content
    showSubmissionInProgress(TRUE)  # Show "submission in progress" message
    
    # Use shinyjs::delay to delay further actions and allow UI update
    shinyjs::delay(100, {
      # Proceed with the data submission
      prog <- rv$progress
      if (all(prog$Status %in% c("Done", "Selected", "Hecho", "Seleccionado"))) {
        
        data <- rv$saved_data
        
        # Generate a unique participant ID using a timestamp
        participant_id <- as.numeric(Sys.time())
        
        # Add submission time in Central European Time (MEZ)
        data$time <- with_tz(now(), tzone = "Europe/Zurich")
        
        # Handle checkbox selection for Name_publ column
        name_publ_value <- ifelse(input$checkbox, FALSE, TRUE)
        data$Name_publ <- name_publ_value
        
        # Prepare the data to be written in the correct order
        data_to_write <- data %>%
          mutate(Participant_ID = participant_id,
                 Participant_Name = input$participant_name) %>%
          select(Participant_ID, Participant_Name, Statement_ID, Statement, NAT_Coords, CUL_Coords, SOC_Coords, time, Name_publ)
        
        # Write the data to the Google Sheet
        sheet_append(ss, data_to_write, sheet = "responses")
        
        # Once submission is complete, hide the submission in progress message and show the thank-you message
        showSubmissionInProgress(FALSE)  # Hide "submission in progress" message
        showThankYou(TRUE)  # Show "thank you" message
        
        # Clear the selected points and saved data
        rv$selected_points <- data.frame(Statement_ID = integer(), A = numeric(), B = numeric(), C = numeric(), Statement = character(), stringsAsFactors = FALSE)
        rv$saved_data <- initial_data()
        
      } else {
        # Handle case when not all statements are completed
        output$warning_submission <- renderText({
          if (selected_language() == "en") {
            "Not all statements are done."
          } else {
            "No todas las declaraciones están completas."
          }
        })
      }
    })
  })
  
  output$progress_table <- renderTable(
    {
      prog <- rv$progress
      prog$Status <- ifelse(prog$Status == "Pending", ifelse(selected_language() == "en", "Pending", "Pendiente"), prog$Status)
      prog$Status <- ifelse(prog$Status == "Done", ifelse(selected_language() == "en", "Done", "Hecho"), prog$Status)
      prog$Status <- ifelse(prog$Status == "Selected", ifelse(selected_language() == "en", "Selected", "Seleccionado"), prog$Status)
      prog$Status <- ifelse(prog$Question == rv$current_statement, paste0("<b>", prog$Status, "</b>"), prog$Status)
      prog$Question <- ifelse(prog$Question == rv$current_statement, paste0("<b>", prog$Question, "</b>"), prog$Question)
      prog <- prog[, c("Question", "Status")]
      if (selected_language() == "en") {
        colnames(prog) <- c("Question", "Status")
      } else {
        colnames(prog) <- c("Declaración", "Estado")
      }
      prog
    },
    sanitize.text.function = identity
  )
}

# Start the Shiny app
shinyApp(ui = ui, server = server)
