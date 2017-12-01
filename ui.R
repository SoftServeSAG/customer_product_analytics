#Function to add nice collapse in menu sidebar
convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  if(length(mi$attribs$class)>0 && mi$attribs$class=="treeview"){
    mi$attribs$class=NULL
  }
  mi
}

#------------------------------------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(id="sbmenu",
              #Customer-based analytics
              convertMenuItem(
                menuItem(text = "Customer-based analytics", tabName = "customer_analytics", icon = icon("area-chart"),
                         
                         numericInput(inputId = "ca_user_id", 
                                      label = "Select customer", 
                                      value = 1, min = 1, max = 2500, step = 1),
                         
                         uiOutput("ca_render_departments_list"),
                         
                         dateRangeInput(inputId = "ca_dates_to_predict", 
                                        label = "Enter dates to for visit estimation", 
                                        min = "2016-02-27", start = "2016-02-27", end = "2016-03-27"),
                         
                         div(class = "shiny-input-container shiny-bound-input",
                             actionButton("ca_analyse_customer", "Analyse customer")
                         )
                ), "customer_analytics"
              ),
              
              #Product-based analytics
              
              convertMenuItem(
                menuItem(text = "Product-based analytics", tabName = "product_analysis", icon = icon("pie-chart"),
                         
                         uiOutput("pa_render_departments_list"),
                         
                         uiOutput("pa_render_commodity_list"),
                         
                         uiOutput("pa_render_sub_commodity_list"),
                         
                         uiOutput("pa_render_products_list"),
                         
                         selectInput(inputId = "pa_date_range", label = "Select time frame for prediction", choices = list("month" = 30, "3 months" = 90, "1/2 year" = 180, "year" = 360), multiple = FALSE),
                         
                         div(class = "shiny-input-container shiny-bound-input",
                             actionButton("pa_analyse_product", "Analyse product")
                         )
                ), "product_analysis")
  )
)

#------------------------------------------------------
body <- dashboardBody(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "shiny.css")
  ),
  
  tabItems(
    tabItem(tabName = "customer_analytics",
            
            box(title = "Customer visit history",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                fluidRow(
                  valueBoxOutput("ca_box_first_user_visit"),
                  valueBoxOutput("ca_box_last_user_visit"),
                  valueBoxOutput("ca_box_average_number_visits_per_week")
                ),
                fluidRow(
                  box(title = "Number of visits per week", 
                      status = "primary",
                      solidHeader=FALSE,
                      width = 12,
                      plotlyOutput("ca_plot_weeks_visits"))
                )
            ),
            
            box(title = "Customer profile",
                status = "primary",
                solidHeader = TRUE, 
                width = 12,
                box(title = "Products Profile", 
                    status = "primary",
                    solidHeader=FALSE,
                    width = 6,
                    plotlyOutput("products_profile")),
                box(title = "Prices profile", 
                    status = "primary",
                    solidHeader=FALSE,
                    width = 6,
                    plotlyOutput("prices_profile"))
            ),
            
            box(title = "Probabilities of next 3 visits",
                status = "primary",
                solidHeader = TRUE, 
                width = 12,
                box(title = "Visualization",
                    status = "primary",
                    solidHeader=FALSE,
                    width = 6,
                    plotOutput("next_visit_plot")
                ),
                box(title = "Data",
                    status = "primary",
                    solidHeader=FALSE,
                    width = 6,
                    dataTableOutput("next_visits_table")
                )
            ),
            
            fluidRow(
              box(title = "Recommendations for customer",
                  status = "primary",
                  solidHeader = TRUE, 
                  width = 12,
                  box(title = "Top customer preferences", 
                      status = "primary",
                      solidHeader=FALSE,
                      width = 6,
                      dataTableOutput("customer_preferences")),
                  box(title = "Recommendations for customer",
                      status = "primary",
                      solidHeader=FALSE,
                      width = 6,
                      dataTableOutput("customer_recommendations"))
              )
            )
    ),
    
    tabItem(tabName = "product_analysis",
            box(title = "Product purchase history",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                fluidRow(
                  valueBoxOutput("box_first_product_bought"),
                  valueBoxOutput("box_last_product_bought"),
                  valueBoxOutput("box_average_number_purchases_per_week")
                ),
                fluidRow(
                  box(title = "Number of purchases per week", 
                      status = "primary",
                      solidHeader=FALSE,
                      width = 12,
                      plotlyOutput("plot_weeks_purchases"))
                )
            ),
            
            box(title = "Product Profile",
                status = "primary",
                solidHeader = TRUE, 
                width = 12,
                box(title = "Customers age profile", 
                    status = "primary",
                    solidHeader=FALSE,
                    width = 6,
                    plotlyOutput("age_profile")),
                box(title = "Customers income profile", 
                    status = "primary",
                    solidHeader=FALSE,
                    width = 6,
                    plotlyOutput("income_profile"))
            ),
            
            box(title = "Demand prediction",
                  status = "primary",
                  solidHeader = TRUE, 
                  width = 12,
                  box(title = "Visualization", 
                      status = "primary",
                      solidHeader=FALSE,
                      width = 7,
                      dygraphOutput("demand_predictions_chart")),
                  box(title = "Data",
                      status = "primary",
                      solidHeader=FALSE,
                      width = 5,
                      dataTableOutput("demand_predictions_data"))
            ),
            fluidRow(
              box(title = "Customers to recommend",
                  status = "primary",
                  solidHeader = TRUE, 
                  width = 12,
                  box(title = "Customers, who like product", 
                      status = "primary",
                      solidHeader=FALSE,
                      width = 6,
                      dataTableOutput("product_preferences")),
                  box(title = "Recommended customers for product",
                      status = "primary",
                      solidHeader=FALSE,
                      width = 6,
                      dataTableOutput("product_recommendations"))
              )
            )
    )
  )
)

dashboardPage(
  skin = "black",
  dashboardHeader(title = "Analytics Prototype"),
  sidebar,
  body
)