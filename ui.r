
# Define UI for application
fluidPage(
  
  headerPanel('TCJA Tax Calculator'),

  mainPanel(
    tabsetPanel(
      tabPanel("Federal Taxes", 
               tableOutput("taxtable"),
               htmlOutput("summary"),
               br(),
               h3("Example Families"),
               h4("Married Taxpayers Filing Jointly; 2 Children"),
               fluidRow(
                 column(3,actionButton("example1", "Example 1: $40,000 Income")),
                 column(3,actionButton("example2", "Example 2: $75,000 Income")),
                 column(3,actionButton("example3", "Example 3: $150,000 Income")),
                 column(3,actionButton("example4", "Example 4: $500,000 Income"))
               ),
               h4("Single Taxpayer; 1 Child"),
               fluidRow(
                 column(3,actionButton("example5", "Example 5: $40,000 Income")),
                 column(3,actionButton("example6", "Example 6: $75,000 Income")),
                 column(3,actionButton("example7", "Example 7: $150,000 Income")),
                 column(3,actionButton("example8", "Example 8: $500,000 Income"))
               )
               ),
      tabPanel("Minnesota Taxes", 
               tableOutput("mntax"),
               htmlOutput("mnsummary")),
      tabPanel("Assumptions and Explanation", 
               htmlOutput("caveats"))
    )
  ),
  
  sidebarPanel(h3("Taxpayer Information"),
    selectInput('status', 'Filing Status', choices = c("Married Filing Jointly",
                                                       "Single",
                                                       "Head of Household",
                                                       "Married Filing Separately")),
    numericInput('dependents', "# of Dependents", 2, min=0),
    numericInput('child', "Children 16 and Younger", 2, min=0),
    numericInput('agi', 'Adjusted Gross Income', 63000),
    numericInput('capgains', 'Long-term Dividends/Qualified Capital Gains', 0),
    h4("Itemized Deductions"),
    p("1040 Schedule A line in parentheses"),
    numericInput('med', 'Deductible Medical and Dental Expenses (4)', 0, min = 0),
    numericInput('state', 'State and Local Taxes Paid (5)', 0, min = 0),
    numericInput('realest', 'Real Estate Taxes Paid (6)', 0, min = 0),
    numericInput('pptax', 'Personal Property Taxes Paid (7)', 0, min = 0),
    numericInput('othtax', 'Other Taxes Paid (8)', 0, min = 0),
    numericInput('intpd', 'Interest Paid (15)', 0, min = 0),
    numericInput('char', 'Gifts to Charity (19)', 0, min = 0),
    numericInput('caustheft', 'Deductible Casualty & Theft Losses (20)', 0, min = 0),
    numericInput('jobmisc', 'Deductible Job Expenses/Miscellaneous (27)', 0, min = 0),
    numericInput('othermisc', 'Other Misc Deductions (28)', 0, min = 0)
  )

)

