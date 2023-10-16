
library(shinydashboard)
library(shiny)
library(plotly)



#data cleaning

df <- read.csv("BodyFat.csv")
center1 <- round((4.57/df$DENSITY)-4.142,3)*100
density_out_ind <- which(((df$BODYFAT<center1+1) & (df$BODYFAT>center1-1))==F)
df$BODYFAT[density_out_ind[1:3]] <-round((4.95/df$DENSITY[density_out_ind[1:3]]-4.5)*100,1) 


center2 <- round((df$WEIGHT/df$HEIGHT^2)*703,3)
bmi_out_ind <- which(((df$ADIPOSITY<center2+1) & (df$ADIPOSITY>center2-1))==F)

df_clear <- df[-c(density_out_ind[4:5],bmi_out_ind),]
df_clear$waist_hip_ratio <- df_clear$ABDOMEN/df_clear$HIP
df_clear <- df_clear[!colnames(df_clear)%in% c("IDNO","DENSITY","WEIGHT","HEIGHT","ABDOMEN","HIP")]
lm_model <- lm(BODYFAT ~ ADIPOSITY + waist_hip_ratio, data = df_clear)

x1 <- df_clear$waist_hip_ratio
x2 <- df_clear$ADIPOSITY
y <- df_clear$BODYFAT
# Create a grid of points for the hyperplane
x1_grid <- seq(min(x1), max(x1), length = 200)
x2_grid <- seq(min(x2), max(x2), length = 200)
grid <- expand.grid(waist_hip_ratio = x1_grid, ADIPOSITY = x2_grid)

# Make predictions for the grid
grid$y_pred <- predict(lm_model, newdata = grid)

ui <- dashboardPage(
  dashboardHeader(title = "Adult Male Body Fat Study"),
  dashboardSidebar(
    sidebarMenu(
      #sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
      #              label = "Search..."),
      menuItem("Body Fat Calculator", tabName = "Data", 
               icon = icon("calculator", lib = "font-awesome")),
      menuItem("Visualization", tabName = "visua", 
               icon = icon("chart-line", lib = "font-awesome")),
      menuItem("Contact us", tabName = "contact", 
               icon = icon("circle-question", lib = "font-awesome"))
    )
  ),
  dashboardBody(
    tabItems(
      # tab data
      tabItem(tabName = "Data",
              fluidRow(
                # valueBoxOutput("row"),
                box(h2("Body Fat Calculator"),
                    numericInput("bmi", "Body Mass Index (Weight (kg) / (height (meters))^2", 0),
                    numericInput("waist", "Abdomen (cm)", 0),
                    numericInput("hip", "Hip (cm)", 0)
                ),
                box(h4("Your Body Fat is:"),
                    textOutput("output"))
              )
      ),
      # tab visualization
      tabItem(tabName = "visua",
              h2("See where you at in our model!"),
              box(plotlyOutput("plot1", height = 400))
      ),
      # tab contact us
      tabItem(tabName = "contact",
              h2("CONTACT US"),
              p("Feel free to contact us if you have any comments, questions or suggestions!"),
              p(strong("Email:"), p("fyan27@wisc.edu"), p("zeng86@wisc.edu"),p("skumar237@wisc.edu")))
    )
  )
)

server <- function(input, output){
  
  output$output <- renderText({
    input_data <- data.frame(ADIPOSITY = input$bmi,
                             waist_hip_ratio = input$waist / input$hip)
    predictions <- predict(lm_model, newdata = input_data)
    paste(predictions, "%")
  })
  
  output$plot1 <- renderPlotly({
    input_data2 <- data.frame(ADIPOSITY = input$bmi,
                              waist_hip_ratio = input$waist / input$hip)
    pred <- predict(lm_model, newdata = input_data2)
    scatt3d <- plot_ly(data = grid, x = ~x1,
                       y = ~x2, z = ~y_pred, mode = "markers", 
                       marker = list(size = 5, opacity = 0.7)) |>
      add_trace(data = data.frame(x1, x2, y), x = ~x1, y = ~x2, z = ~y, 
                type = "scatter3d", mode = "markers", 
                marker = list(size = 5, color = ~ y, colorscale = "Viridis"),
                name = "Model observations") |>
      layout(scene = list(title = "3D Scatter Plot with Hyperplane", 
                          xaxis = list(title = "waist_hip_ratio"), 
                          yaxis = list(title = "ADIPOSITY"), 
                          zaxis = list(title = "BODYFAT"))) |>
      add_trace(x = input$waist / input$hip, y = input$bmi, z = pred, name = "You", marker = list(color = "red"))
    scatt3d
  })
  
}

shinyApp(ui, server)

