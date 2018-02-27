
# Define UI for application
fluidPage(
  
  headerPanel('House Research Federal Tax Law Calculator'),
  mainPanel(
    tabsetPanel(
      tabPanel("Federal Taxes",
             tableOutput("taxtable"),
             htmlOutput("summary"),
             br(),
             htmlOutput("disclaimer")
        ),
        tabPanel("Minnesota Taxes",
                 tableOutput("mntax"),
                 htmlOutput("mnsummary")),
        tabPanel("Assumptions and Explanation",
                 htmlOutput("caveats"))
    )
  ),
  sidebarPanel(h3("Example Families"),
               h4("Married Taxpayers Filing Jointly; 2 Children"),
               fluidRow(
                 column(6,actionButton("example1", "$40,000 Income")),
                 column(6,actionButton("example2", "$75,000 Income"))
               ),
               fluidRow(
                 column(6,actionButton("example3", "150,000 Income")),
                 column(6,actionButton("example4", "$500,000 Income"))
               ),
               h4("Head of Househhold Taxpayer; 1 Child"),
               fluidRow(
                 column(6,actionButton("example5", "$40,000 Income")),
                 column(6,actionButton("example6", "$75,000 Income"))
               ),
               fluidRow(
                 column(6,actionButton("example7", "$150,000 Income")),
                 column(6,actionButton("example8", "$500,000 Income"))
               ),
               h4("Single Taxpayer; No Children"),
               fluidRow(
                 column(6,actionButton("example9", "$40,000 Income")),
                 column(6,actionButton("example10", "$75,000 Income"))
               ),
               fluidRow(
                 column(6,actionButton("example11", "$150,000 Income")),
                 column(6,actionButton("example12", "$500,000 Income"))
               ),
               h4("Taxpayer Details"),
               htmlOutput("deductionspiel"),
               tableOutput("tpdetails"),
               p(glue(
"Itemized deductions estimated based on average amounts deducted by other txpayer with the same filing status with 
similar incomes."
                  )
               )
  )

)

