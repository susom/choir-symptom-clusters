#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
#library(here)
library(tidyverse)
library(ggplot2)
library(plotly)
library(FactoMineR)
library(DT)
library(shinycssloaders)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("CHOIR Symptom Cluster Tool"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # Input: Select a file ----
            fileInput("file1", "Upload Your CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv"))
            
            # Horizontal line ----
            , tags$hr()
            
            # Input: Checkbox if file has header ----
            , checkboxInput("header", "Header", TRUE)
            
            # Input: Select separator ----
            , radioButtons("sep", "Separator",
                           choices = c(Comma = ",",
                                       Semicolon = ";",
                                       Tab = "\t"),
                           selected = ",")
            
            # Input: Select quotes ----
            , radioButtons("quote", "Quote",
                           choices = c(None = "",
                                       "Double Quote" = '"',
                                       "Single Quote" = "'"),
                           selected = '"')
            
            # Horizontal line ----
            , tags$hr()
            
            # Input: Select number of rows to display ----
            # , radioButtons("disp", "Display",
            #              choices = c(Head = "head",
            #                          All = "all"),
            #              selected = "head")
            # Horizontal line ----
            # , tags$hr()
            
            # button for labeling data
            , actionButton("labelButton", "Label Data")
            , br()
            , br()
            # go button for plotting
            , actionButton("plotButton", "Plot Data")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            # Output: Tabset w/ plot, summary, and table ----
            tabsetPanel(
                type = "tabs",
                tabPanel(
                    "Overview/Wiki"
                    , br()
                    , h3("Project Description")
                    , p("This app implements a classification system of chronic 
                        pain severity using PROMIS-based multidimensional 
                        pain-agnostic symptom assessments. The output provides 
                        a label (the number 1, 2, or 3) indicating a general 
                        graded scale of severity such that label 1 reflects the 
                        least severe condition, label 2 reflects medium 
                        severity, and label 3 reflects the worst severity. The 
                        labels for these three different clusters were developed 
                        in a sample of patients with chronic pain, indicating 
                        diagnostic- and prognostic-like properties. For more 
                        information on the development and validation of this 
                        classification system, please see the referenced paper. 
                        Please see the instructions for guidance on usability.")
                    , br()
                    , img(src = "cluster_pca_3d.png", height = 480, width = 720)
                    #, h4("3D Visualization of Clusters")
                    #, withSpinner(plotlyOutput("plot", height = "480px"), color="#0dc5c1")
                    , br()
                    , p("Click below if you would like to download an example 
                        dataset to try with the classification tool.")
                    , downloadButton("downloadExampleData", "Download Example Data")
                    , h3("Instructions")
                    , HTML("
                        <div>
                           <ol>
                            <li>Upload your data using the \'Browse\' button on the left panel.</li>
                            <li>View your data on the \'Table\' tab. Make sure the columns are in the appropriate order.</li>
                            <li>Click the \'Label Data\' button to assign your data to clusters.</li>
                            <li>Download your labeled data from the \'Cluster\' tab.</li>
                            <li>Use the \'Plot Data\' button to visualize the clusters in your
                            dataset in three dimensions.</li>
                            <ol>
                        </div>
                           ")
                    , h3("References")
                    , tags$blockquote("Classifying chronic pain using multidimensional pain-agnostic symptom assessments and clustering analysis\n
                    Gadi Gilam, Eric M. Cramer, Kenneth A. Webber II, Maisa S. Ziadni, Ming-Chih Kao, Sean C. Mackey\n
                                      medRxiv 2021.04.21.21255885; doi: https://doi.org/10.1101/2021.04.21.21255885")
                )
                , tabPanel(
                    "Table"
                    , br()
                    , h2("Visually Inspect Data Upload")
                    , p("If a table does not appear here, use the \'Browse\' button to 
                        load your data set.")
                    , dataTableOutput("contents")
                    , br()
                    , HTML("
                        <div>
                         <p>Please check to make sure your columns are in the 
                         following order from left to right before proceeding.</p>
                         <ol>
                         <li>Anonymous Patient Identifier Column</li>
                         <li>PROMIS_FATIGUE</li>
                         <li>PROMIS_DEPRESSION</li>
                         <li>PROMIS_ANXIETY</li>
                         <li>PROMIS_SLEEP_DISTURBANCE</li>
                         <li>PROMIS_SLEEP_IMPAIRMENT</li>
                         <li>PROMIS_ANGER</li>
                         <li>PROMIS_SOCIAL_ISOLATION</li>
                         <li>PROMIS_EMOTIONAL_SUPPORT</li>
                         <li>PROMIS_SATISFACTION_WITH_SOCIAL_ROLES_AND_ACTIVITIES</li>
                         </ol>
                         <p>If the columns match, then you may proceed with labeling and
                            plotting your data. Otherwise, please return to the original
                            file to make any changes/fixes, and re-upload to the application.</p>
                        </div>
                           ")
                )  # show data
                , tabPanel(
                    "Cluster"
                    , br()
                    , h2("Cluster-Labeled Data")
                    , p("If no data table appears here, click the \'Label Data\' button.
                        Otherwise, scroll to the far right to view the cluster assignments.")
                    , withSpinner(DT::dataTableOutput("clusteredDF"), color="#0dc5c1")
                    # , DTOutput("clusteredDF")
                    , withSpinner(plotlyOutput("clusterBarplot"), color="#0dc5c1")
                    , downloadButton("downloadData", "Download Labeled Data")
                )
                , tabPanel(
                    "Plot"
                    , br()
                    , h2("Plot Labeled Data")
                    , p("If no chart appears here, click the \'Plot Data\' button. 
                        The 3D plot is interactive. You may zoom, pan, select, and rotate the data.
                        If you would like to save a png image of your data, click the grey
                        camera icon in top-right of the plot.")
                    , withSpinner(plotlyOutput("userPlot", height = "780px"), color="#0dc5c1")
                ) # pca plot
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    # values object to store user's labeled data
    values <- reactiveValues(labeled_data = NULL)
    
    # read in points to plot
    pca_res <- readRDS("pca.rds")

    # load the cluster centroids
    centroids <- readRDS("centroids.rds")
    
    # function to label new data
    centroid_classifier <- function(x, centers = centroids) {
        # return the cluster assignment based on the centroids
        return(which.min(apply(centers, 1, function(c, y){dist(rbind(c,y))}, y=x)))
    }
    
    # label the testing data
    label_data <- function(df) {
        df[['cluster']] <- as.factor(apply(df[,-1], 1, centroid_classifier))
        df
    }
    
    # get the uploaded data set
    dataSet <- reactive({
        # check if data is uploaded...
        req(input$file1)
        
        # load the dataset
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        # when reading semicolon separated files,
        # having a comma separator causes `read.csv` to error
        tryCatch(
            {
                df <- read.csv(input$file1$datapath,
                               header = input$header,
                               sep = input$sep,
                               quote = input$quote)
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )
        # if(input$disp == "head") {
        #     return(df)
        # }
        # else {
        #     return(df)
        # }
        return(df)
    })
    
    
    # loading and rendering the uploaded data
    output$contents <- DT::renderDataTable({
        # generate a DT of the uploaded data
        req(input$file1)
        dataSet() %>%
            datatable(options = list(
                pageLength = 5
                ,lengthMenu = c(5, 10, 25, 50, 100)
                , scrollX = T)
            )
    })
    
    # plot the user's points in PCA
    observeEvent(input$plotButton,{
        output$userPlot <- renderPlotly({
            input$plotButton # reload on the plotButton
            
            # generate the data to plot
            pca_df <- values$labeled_data[, -c(1, ncol(values$labeled_data))]
            user_coords <- as.data.frame(predict.PCA(pca_res, pca_df)$coord)
            user_coords$cluster <- values$labeled_data$cluster
            fig <- plot_ly(
                user_coords
                , x = ~Dim.1
                , y = ~Dim.2
                , z = ~Dim.3
                , color = ~cluster
                , marker = list(line = list(color = "black", width = 0.25, opacity = .50))
                , colors = c("#DEEBF7", "#9ECAE1", "#3182BD")
                , opacity = 0.75
            ) %>%
                add_markers() %>%
                layout(
                    scene = list(
                        xaxis = list(title = "Dimension 1")
                        , yaxis = list(title = "Dimension 2")
                        , zaxis = list(title = "Dimension 3")
                    )
                    , legend = list(orientation = "h", title=list(text='<b> Cluster </b>'))
                    , title = "Clustered Data Visualized with PCA"
                )
            
            isolate({
                
            })
            
            fig
        })
    })
    
    # plot the pca points
    # output$plot <- renderPlotly({
    #     # input$plotButton # reload on the plotButton
    #     
    #     fig <- plot_ly(
    #         pca_coords
    #         , x = ~Dim.1
    #         , y = ~Dim.2
    #         , z = ~Dim.3
    #         , color = ~cluster
    #         , marker = list(line = list(color = "grey20", width = 0.25, opacity = 0.5))
    #         , colors = c("#DEEBF7", "#9ECAE1", "#3182BD")
    #         , opacity = 0.9
    #     ) %>%
    #         add_markers() %>%
    #         layout(
    #             scene = list(
    #                 xaxis = list(title = "Dimension 1")
    #                 , yaxis = list(title = "Dimension 2")
    #                 , zaxis = list(title = "Dimension 3")
    #             )
    #             , legend = list(orientation = "h", title=list(text='<b> Cluster </b>'))
    #             , title = "Clustering with Nine Pain-Agnostic Factors"
    #         )
    #     
    #     isolate({
    #         
    #     })
    #     
    #     fig
    # })
    
    ### Cluster and label the data
    # loading and rendering the uploaded data
    observeEvent(input$labelButton, {
        # label the data
        values$labeled_data <- dataSet() %>%
            label_data()
    })
    
    output$clusteredDF <- DT::renderDataTable({
        # generate a DT of the uploaded data
        # req(input$file1)
        values$labeled_data %>%
            datatable(options = list(
                pageLength = 5
                ,lengthMenu = c(5, 10, 25, 50, 100)
                , scrollX = T)
            )
    })
    
    # show graph with # patients per cluster
    output$clusterBarplot <- renderPlotly({
        req(input$labelButton)
        (values$labeled_data %>%
                ggplot() +
                geom_bar(
                    aes(x = cluster)
                    , stat = "count"
                    , fill = c("#DEEBF7", "#9ECAE1", "#3182BD")
                    , color = "black"
                ) +
                labs(
                    title = "Number of Observations in each Cluster"
                    , x = "Cluster"
                    , y = "Number of Patients"
                ) +
                theme_minimal() +
                theme(
                    panel.grid.major = element_blank()
                    , panel.grid.minor = element_blank()
                    , legend.position = 'none'
                )
        ) %>%
            ggplotly()
    })
    
    # downloading the labeled data
    # Downloadable csv of selected dataset ----
    output$downloadData <- downloadHandler(
        filename = function() {
            nm <- gsub(".csv", "", input$file1$name)
            nm <- paste(nm, "_labeled.csv", sep = "")
            return(nm)
        },
        content = function(file) {
            write.csv(values$labeled_data, file, row.names = FALSE)
        }
    )
    
    # downloading the labeled data
    # Downloadable csv of selected dataset ----
    output$downloadExampleData <- downloadHandler(
        filename = function() {
            nm <- "example_dataset.csv"
            return(nm)
        },
        content = function(file) {
            write.csv(
                readRDS("example_dataset.rds")
                , file
                , row.names = FALSE)
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
