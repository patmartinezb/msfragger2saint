
# Load libraries ----------------------------------------------------------

lib <-  c("shiny", "readr", "stringr", "dplyr", "purrr", "tidyr", "ggplot2",
          "corrplot", "plotly", "stats", "reshape2", "zip")

# Checking missing packages from list

new.packages <- setdiff(lib, installed.packages())

if (length(new.packages) != 0) invisible(lapply(new.packages, install.packages, update = FALSE))

# Load

invisible(lapply(lib, require, character.only = TRUE))


# Define UI ---------------------------------------------------------------

ui <- fluidPage(
  titlePanel("msFragger (SPC) to SAINT files"),
  br(),
  theme = bslib::bs_theme(bootswatch = "sandstone"),
  tabsetPanel(
    tabPanel("Import data", 
             br(),
             fileInput("upload", "Upload data:", buttonLabel = "Upload .tsv data", 
                       multiple = TRUE, accept = ".tsv"),
             br(),
             tableOutput("files"),
             hr(),
             HTML(paste0("<b>","Merging files","</b>")),
             br(),
             radioButtons("merge", "Do you want to merge the data?", 
                          choices = c("Yes", "No"),
                          selected = "No"),
             textOutput("mergeTable"),
             br(),
             uiOutput("cityControls"),
             hr(),
             HTML(paste0("<b>","SAINT files","</b>")),
             br(),
             "To download the three files to feed to SAINT (Bait, Prey and Interaction files)",
             "as a .zip file, press the download button below.",
             br(), br(),
             downloadButton("download.saint", label = "Download SAINT files"),
             hr()
             
    ),
    
    tabPanel("Data viz",
             tabsetPanel(type = "pills",
                         selected = "Histogram",
               tabPanel("Histogram",
                        plotlyOutput("hist"),
                        ),
               
               tabPanel("Correlation plot",
                        plotOutput("corr", width = "600px", height = "600px"),
                        )
               )
             )
  )
)


# Define server logic -----------------------------------------------------

server <- function(input, output, session) {
  
  # Uploaded data
  
  data <- reactive({
    
    req(input$upload)
    
    temp <- input$upload$datapath
    
    if ((str_detect(temp, ".tsv$")) && (length(temp) > 1)){
      
      # Create list with the uploaded files - if there are more than one
      dfs <- lapply(temp, function(x){
        
        read_delim(x, "\t", escape_double = FALSE, trim_ws = TRUE)
        
      })
      
      return(dfs)
      
    } else if ((str_detect(temp, ".tsv$")) && (length(temp) == 1)) {
      
      # No list if it's just one file
      dfs <- read_delim(temp, "\t", escape_double = FALSE, trim_ws = TRUE)
      
      return(dfs)
      
    } else {
      
      validate("One or more invalid files; Please upload .tsv files")
      
    }
    
  })
  
  
  output$files <- renderTable(input$upload) # Table showing what the user uploaded
  
  
  # Merge different files or keep the one uploaded if it's just one
  
  merged.data <- reactive({
    
    if ((input$merge == "Yes") && (length(input$upload$datapath) > 1)){
      
      merged <- merge.files(data())
      
      return(merged)
      
    } else if ((input$merge == "Yes") && (length(input$upload$datapath == 1))){
      
      validate("2 or more files are needed in order to merge - Only 1 is loaded")
      
    } else {
      
      return(data())
      
    }
    
  }) # Merge files or keep it as it was if it's only one file
  
  output$mergeTable <- renderText({
    
    if (input$merge == "Yes"){
      
      paste("The merged dataset is made of", 
          dim(merged.data())[1], "rows (proteins), and", dim(merged.data())[2],
          "columns (samples), including the variables", colnames(merged.data())[1], ",",
          colnames(merged.data())[2], "and", colnames(merged.data())[3], ".")
      
    }
    
  }) # Shows characteristics of merged data - if any
  
  output$cityControls <- renderUI({
    
    if (input$merge == "Yes" && (length(input$upload$datapath) > 1)){
      
      req(input$upload)
      
      downloadButton("download.merged", label = "Download merged data")
      
    }
  })  # Download button only shows if users choose "Yes"
  
  
  # Plot histogram
  
  output$hist <- renderPlotly({
    
    req(input$upload)
    
    p <- histogram_prot(merged.data())
    
    height <- session$clientData$output_p_height
    width <- session$clientData$output_p_width
    
    ggplotly(p, height = height, width = width)
    
  })
  
  
  # Plot corrplot
  
  output$corr <- renderPlot({
    
    corrplot_prot(merged.data())
    
  }, res = 96)
  
  
  
  # .zip SAINT files
  
  # Bait file
  
  keysFile <- reactive({
    
    df <- bait_file(merged.data())
    
    return(df)
    
  })
  
  # Interaction file - no NAs
  
  saint.file <- reactive({
    
    df <- interaction_file(merged.data())
    
    return(df)
    
  })
  
  # Prey file
  
  prey.file <- reactive({
    
    df <- prey_file(merged.data())
  
    return(df)
    
  })
  
  # Download files as .zip
  
  output$download.saint <- downloadHandler(
    filename = function(){
      paste("SAINT_files_", Sys.Date(), ".zip", sep = "")
    },
    content = function(fname) {
      tmpdir <- tempdir()
      setwd(tempdir())
      print(tempdir())
      
      fs <- c("keysFile.tsv", "saint_file.tsv", "prey_file.tsv")
      write.table(keysFile(), file = "keysFile.tsv", sep = "\t", 
                  col.names = TRUE, row.names = FALSE, quote = FALSE)
      write.table(saint.file(), file = "saint_file.tsv", sep = "\t", 
                  col.names = TRUE, row.names = FALSE, quote = FALSE)
      write.table(prey.file(), file = "prey_file.tsv", sep = "\t", 
                  col.names = TRUE, row.names = FALSE, quote = FALSE)
      
      zip(zipfile=fname, files=fs)
      if(file.exists(paste0(fname, ".zip"))) {file.rename(paste0(fname, ".zip"), fname)}
    },
    contentType = "application/zip"
  ) 
  
}

# Run the application 
shinyApp(ui = ui, server = server)
