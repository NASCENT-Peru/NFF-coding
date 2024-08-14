library(googlesheets4)
library(shiny)
library(shinyjs)
library(plotly)
library(dplyr)
library(lubridate)

# Set authentication token to be stored in a folder called .secrets
#options(gargle_oauth_cache = ".secrets")

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

# Register the www directory
addResourcePath("www", "www")

# UI for the Shiny app
ui <- fluidPage(
  titlePanel("Ternary Plot Survey"),
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      selectInput("language", "Language / Idioma", choices = c("English" = "en", "Español" = "es")),
      conditionalPanel(
        condition = "!output.showSurvey && !output.showIntroduction",
        textInput("participant_name", "Name / Nombre"),
        checkboxInput("checkbox", 
                      "Check if you do NOT want your Name to be published on the NASCENT-Peru Website / Marque si NO desea que su nombre se publique en el sitio web de NASCENT-Peru"),
        actionButton("start_button", "Start Introduction / Comenzar Introducción")
      ),
      textOutput("warning"),
      conditionalPanel(
        condition = "output.showIntroduction",
        uiOutput("intro_content")
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
        uiOutput("unsure_button_ui"),
        uiOutput("next_button_ui"),
        uiOutput("back_button_ui"),
        uiOutput("submit_button_ui"),
        textOutput("warning_submission"),
        div(style = 'overflow-y:scroll; max-height:300px;', tableOutput("progress_table"))
      )
    ),
    mainPanel(
      tags$style(type = "text/css", "#ternaryPlot {margin-top: 50px;}"),
      plotlyOutput("ternaryPlot", height = "750px")
    )
  )
)

# Server for the Shiny app
server <- function(input, output, session) {
  
  # Reactive value to store the selected language
  selected_language <- reactive(input$language)
  
  # Reactive value to store the statements
  statements <- reactive({
    req(selected_language())
    read_statements(selected_language())
  })
  
  # Current statement index
  current_statement <- reactiveVal(1)
  
  # Reactive value to store the selected points
  selected_points <- reactiveVal(data.frame(Statement_ID = integer(), A = numeric(), B = numeric(), C = numeric(), Statement = character(), stringsAsFactors = FALSE))
  
  # Reactive value to store the last valid coordinates
  last_valid_coords <- reactiveVal(data.frame(A = 0, B = 0, C = 0))
  
  # Reactive value to store the hover coordinates
  hover_coords <- reactiveVal(NULL)
  
  # Reactive value to control the survey display
  show_survey <- reactiveVal(FALSE)
  
  # Reactive value to control the introduction display
  show_introduction <- reactiveVal(FALSE)
  
  # Reactive value to track progress
  progress <- reactiveVal(data.frame(
    Question = 1,
    Status = "pending",
    stringsAsFactors = FALSE
  ))
  
  observe({
    statements_data <- statements()
    progress(data.frame(
      Question = 1:nrow(statements_data),
      Status = rep("pending", nrow(statements_data)),
      stringsAsFactors = FALSE
    ))
  })
  
  # Introduction steps
  intro_step <- reactiveVal(1)
  
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
  
  output$intro_content <- renderUI({
    step <- intro_step()
    if (selected_language() == "en") {
      tagList(
        p(style = "font-size: 18px;", "At the top the current statement and an explanation where necessary is included."),
        p(style = "font-size: 18px;", "Next you will find four buttons: 'unsure', 'next', 'back', and 'submit'. The 'unsure' button is equivalent to a click on the diagram for the statements you do not know how to allocate. The 'next' button will load the next statement. The 'back' button will load the previous statement. The 'submit' button will submit your answers at the end of the survey."),
        p(style = "font-size: 18px;", "The table below shows the total amount of statements to be allocated and a status for each statement. The status 'pending' indicates that the allocation for this statement still needs to be done. The status 'done' shows that the allocation of this statement is complete. The 'submit' button will only work if all statements have the status 'done'."),
        img(src = "www/Introduction/Intro_EN.png", height = "100%", width = "100%"),
        actionButton("back_intro_button", "Back"),
        actionButton("start_survey_button", "Start Survey")
      )
    } else {
      tagList(
        p(style = "font-size: 18px;", "En la parte superior se incluye la declaración actual y una explicación donde sea necesario."),
        p(style = "font-size: 18px;", "A continuación encontrará cuatro botones: 'no seguro', 'siguiente', 'atrás' y 'enviar'. El botón 'no seguro' es equivalente a un clic en el diagrama para las declaraciones que no sabe cómo asignar. El botón 'siguiente' cargará la siguiente declaración. Con el botón 'atrás' se carga la declaración anterior. El botón 'enviar' enviará sus respuestas al final de la encuesta."),
        p(style = "font-size: 18px;", "La tabla a continuación muestra la cantidad total de declaraciones para asignar y un estado para cada declaración. El estado 'pendiente' indica que la asignación para esta declaración aún necesita ser hecha. El estado 'hecho' muestra que la asignación de esta declaración está completa. El botón 'enviar' solo funcionará si todas las declaraciones tienen el estado 'hecho'."),
        img(src = "www/Introduction/Intro_ES.png", height = "100%", width = "100%"),
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
      SOC_Coords = rep(NA, nrow(statements())),
      CUL_Coords = rep(NA, nrow(statements())),
      time = rep(NA, nrow(statements())),
      stringsAsFactors = FALSE
    )
  })
  
  # Saved data as a reactive value
  saved_data <- reactiveVal()
  
  observe({
    saved_data(initial_data())
  })
  
  # Display the next statement
  observeEvent(input$next_button, {
    prog <- progress()
    if (prog$Status[current_statement()] == "selected" || prog$Status[current_statement()] == "seleccionado") {
      prog$Status[current_statement()] <- ifelse(selected_language() == "en", "done", "hecho")
      progress(prog)
    }
    new_index <- current_statement() %% nrow(statements()) + 1
    current_statement(new_index)
  })
  
  observeEvent(input$back_button, {
    new_index <- ifelse(current_statement() > 1, current_statement() - 1, nrow(statements()))
    current_statement(new_index)
  })
  
  observeEvent(input$unsure_button, {
    coords <- data.frame(A = 0, B = 0, C = 0)
    current_statement_id <- current_statement()
    selected_points_df <- selected_points()
    
    # Remove existing point for the current statement
    selected_points_df <- selected_points_df[selected_points_df$Statement_ID != current_statement_id, ]
    
    # Add the new point for the current statement
    selected_points_df <- rbind(selected_points_df, data.frame(Statement_ID = current_statement_id, A = coords$A, B = coords$B, C = coords$C, Statement = statements()$Statement[current_statement_id]))
    selected_points(selected_points_df)
    
    data <- saved_data()
    data[current_statement_id, c("NAT_Coords", "CUL_Coords", "SOC_Coords")] <- coords
    saved_data(data)
    
    prog <- progress()
    if (prog$Status[current_statement()] == "pending" || prog$Status[current_statement()] == "pendiente") {
      prog$Status[current_statement()] <- ifelse(selected_language() == "en", "selected", "seleccionado")
    }
    progress(prog)
  })
  
  output$statement_title <- renderText({
    if (selected_language() == "en") {
      paste("Statement", current_statement(), ":")
    } else {
      paste("Declaración", current_statement(), ":")
    }
  })
  
  output$statement <- renderText({
    statements()$Statement[current_statement()]
  })
  
  output$explanation_title <- renderText({
    if (selected_language() == "en") {
      "Explanation:"
    } else {
      "Explicación:"
    }
  })
  
  output$explanation <- renderText({
    statements()$Explanation[current_statement()]
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
  
  # Create the ternary plot function
  create_ternary_plot <- function(selected_points_df) {
    plot <- plot_ly(source = "ternaryPlot") %>%
      layout(
        showlegend = FALSE,
        hovermode = 'closest',  # Ensure closest point hovermode
        ternary = list(
          sum = 30,
          aaxis = list(title = "", min = 0, max = 30, tickvals = FALSE),
          baxis = list(title = ifelse(selected_language() == "en", "<b>Nature for Society</b><br>Nature’s benefits to people<br>Ecosystem services", "<b>Naturaleza para la Sociedad</b><br>Beneficios de la naturaleza para las personas<br>Servicios ecosistémicos"), min = 0, max = 30, tickvals = FALSE, titlefont = list(size = 14)),
          caxis = list(title = ifelse(selected_language() == "en", "<b>Nature as Culture</b><br>Living in harmony<br>People one with nature", "<b>Naturaleza como Cultura</b><br>Viviendo en armonía<br>Personas una con la naturaleza"), min = 0, max = 30, tickvals = FALSE, titlefont = list(size = 14)),
          bgcolor = 'white',
          showgrid = FALSE
        ),
        annotations = list(
          list(
            x = 0.5, y = 1, text = ifelse(selected_language() == "en", "<b>Nature for Nature</b><br>Intrinsic value of nature<br>Space allocated for nature", "<b>Naturaleza por Naturaleza</b><br>Valor intrínseco de la naturaleza<br>Espacio asignado para la naturaleza"),
            showarrow = FALSE, xref = 'paper', yref = 'paper', xanchor = 'center', yanchor = 'bottom',
            font = list(size = 14)
          )
        ),
        margin = list(t = 100, b = 100, l = 50, r = 50)
      ) %>%
      config(displayModeBar = FALSE)  # Disable plotly mode bar
    
    # Add concentric circles with varying opacities
    for (circle in concentric_circles) {
      plot <- plot %>%
        add_trace(
          type = 'scatterternary',
          mode = 'none',
          a = circle$points$a,
          b = circle$points$b,
          c = circle$points$c,
          fill = 'toself',
          fillcolor = circle$color,
          showlegend = FALSE,
          hoverinfo = 'none'  # Disable hover info for circles
        )
    }
    
    # Add grid points with invisible markers
    plot <- plot %>% add_trace(
      type = 'scatterternary',
      mode = 'markers',
      a = grid_points$A,
      b = grid_points$B,
      c = grid_points$C,
      hoverinfo = 'none',  # Disable hover info for grid points
      marker = list(opacity = 0, size = 5)  # Set opacity to 0 to make markers invisible
    )
    
    # Add selected points
    if (!is.null(selected_points_df) && nrow(selected_points_df) > 0) {
      hover_data <- hover_text(selected_points_df)
      plot <- plot %>% add_trace(
        type = 'scatterternary',
        mode = 'markers',
        a = hover_data$A,
        b = hover_data$B,
        c = hover_data$C,
        marker = list(size = 10, color = 'red', opacity = 0.8),
        text = hover_data$Text,
        hoverinfo = 'text',
        showlegend = FALSE
      )
    }
    
    plot
  }
  
  # Initial plot creation
  output$ternaryPlot <- renderPlotly({
    create_ternary_plot(selected_points())
  })
  
  # Update points on the plot using plotlyProxy
  observe({
    selected_points_df <- selected_points()
    proxy <- plotlyProxy("ternaryPlot", session)
    
    # Update or add the selected points trace
    plotlyProxyInvoke(proxy, "deleteTraces", list(1))
    if (nrow(selected_points_df) > 0) {
      hover_data <- hover_text(selected_points_df)
      plotlyProxyInvoke(proxy, "addTraces", list(
        type = 'scatterternary',
        mode = 'markers',
        a = hover_data$A,
        b = hover_data$B,
        c = hover_data$C,
        marker = list(size = 10, color = 'red', opacity = 0.8),
        text = hover_data$Text,
        hoverinfo = 'text',
        showlegend = FALSE
      ))
    }
  })
  
  # Store hover information
  observeEvent(event_data("plotly_hover", source = "ternaryPlot"), {
    hover_info <- event_data("plotly_hover", source = "ternaryPlot")
    if (!is.null(hover_info) && hover_info$curveNumber != 106) {
      point_index <- hover_info$pointNumber + 1  # R indexes start at 1
      coords <- grid_points[point_index, ]
      hover_coords(coords)  # Store the hover coordinates in reactive value
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
          coords <- last_valid_coords()
        }
      } else {
        # Capture the click data
        point_index <- click_data$pointNumber + 1  # R indexes start at 1
        coords <- grid_points[point_index, ]
        
        # Update the last valid coordinates
        last_valid_coords(coords)
      }
      current_statement_id <- current_statement()
      selected_points_df <- selected_points()
      
      # Remove existing point for the current statement
      selected_points_df <- selected_points_df[selected_points_df$Statement_ID != current_statement_id, ]
      
      # Add the new point for the current statement
      selected_points_df <- rbind(selected_points_df, data.frame(Statement_ID = current_statement_id, A = coords$A, B = coords$B, C = coords$C, Statement = statements()$Statement[current_statement_id]))
      selected_points(selected_points_df)
      
      # Update the plot immediately
      hover_data <- hover_text(selected_points_df)
      proxy <- plotlyProxy("ternaryPlot", session)
      plotlyProxyInvoke(proxy, "deleteTraces", list(1))  # Remove existing points trace
      plotlyProxyInvoke(proxy, "addTraces", list(
        type = 'scatterternary',
        mode = 'markers',
        a = hover_data$A,
        b = hover_data$B,
        c = hover_data$C,
        marker = list(size = 10, color = 'red', opacity = 0.8),
        text = hover_data$Text,
        hoverinfo = 'text',
        showlegend = FALSE
      ))
      
      # Update the table
      data <- saved_data()
      data[current_statement(), c("NAT_Coords", "CUL_Coords", "SOC_Coords")] <- coords
      saved_data(data)
      
      prog <- progress()
      if (prog$Status[current_statement()] == "pending" || prog$Status[current_statement()] == "pendiente") {
        prog$Status[current_statement()] <- ifelse(selected_language() == "en", "selected", "seleccionado")
      }
      progress(prog)
    }
  })
  
  # Submit handler for the Google Sheet
  observeEvent(input$submit_button, {
    prog <- progress()
    if (all(prog$Status %in% c("done","selected", "hecho", "seleccionado"))) {
      data <- saved_data()
      
      # Check the highest Participant_ID in the sheet
      existing_data <- read_sheet(ss, sheet = "responses")
      if(nrow(existing_data) == 0) {
        participant_id <- 1
      } else {
        participant_id <- max(existing_data$Participant_ID, na.rm = TRUE) + 1
      }
      
      # Add submission time in MEZ (Central European Time)
      data$time <- with_tz(now(), tzone = "Europe/Zurich")
      
      # Handle checkbox selection for Name_publ column
      name_publ_value <- ifelse(input$checkbox, FALSE, TRUE)
      data$Name_publ <- name_publ_value
      
      # Prepare the data to be written in the correct order
      data_to_write <- data %>%
        mutate(Participant_ID = participant_id,
               Participant_Name = input$participant_name) %>%
        select(Participant_ID, Participant_Name, Statement_ID, Statement, NAT_Coords, SOC_Coords, CUL_Coords, time, Name_publ)
      
      # Write the data to the sheet
      sheet_append(ss, data_to_write, sheet = "responses")
      
      # Clear the selected points and saved data
      selected_points(data.frame(Statement_ID = integer(), A = numeric(), B = numeric(), C = numeric(), Statement = character(), stringsAsFactors = FALSE))
      saved_data(initial_data())
      
      # Close the session after successful submission
      session$close()
    } else {
      output$warning_submission <- renderText({
        if (selected_language() == "en") {
          "Not all statements are done."
        } else {
          "No todas las declaraciones están completas."
        }
      })
    }
  })
  
  
  output$progress_table <- renderTable({
    prog <- progress()
    prog$Status <- ifelse(prog$Status == "pending", ifelse(selected_language() == "en", "Pending", "Pendiente"), prog$Status)
    prog$Status <- ifelse(prog$Status == "done", ifelse(selected_language() == "en", "Done", "Hecho"), prog$Status)
    prog$Status <- ifelse(prog$Status == "selected", ifelse(selected_language() == "en", "Selected", "Seleccionado"), prog$Status)
    prog$Status <- ifelse(prog$Question == current_statement(), paste0("<b>", prog$Status, "</b>"), prog$Status)
    prog$Question <- ifelse(prog$Question == current_statement(), paste0("<b>", prog$Question, "</b>"), prog$Question)
    prog <- prog[, c("Question", "Status")]
    if (selected_language() == "en") {
      colnames(prog) <- c("Question", "Status")
    } else {
      colnames(prog) <- c("Declaración", "Estado")
    }
    prog
  }, sanitize.text.function = identity)
}

# Start the Shiny app
shinyApp(ui = ui, server = server)

# To run the app, add local IP as host for local hosting
#shiny::runApp(app)

#})

