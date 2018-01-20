
# Define UI for application
pageWithSidebar(
  headerPanel('Tax Cuts and Jobs Act; Example Calculations'),
  sidebarPanel(
    selectInput('status', 'Filing Status', choices = c("Married Filing Jointly",
                                                       "Single",
                                                       "Head of Household",
                                                       "Married Filing Separately")),
    numericInput('dependents', "# of Dependents", 2, min=0),
    numericInput('child', "Children 17 and Younger", 2, min=0),
    numericInput('agi', 'Adjusted Gross Income', 63217),
    numericInput('capgains', 'Long Term Dividends/Capital Gains', 0),
    h3("Itemized Deductions"),
    p("1040 Schedule A line in parentheses"),
    numericInput('med', 'Medical and Dental Expenses (4)', 0, min = 0),
    numericInput('txpaid', 'Taxes Paid (9)', 0, min = 0),
    numericInput('intpd', 'Interest Paid (15)', 0, min = 0),
    numericInput('char', 'Gifts to Charity (19)', 0, min = 0),
    numericInput('caustheft', 'Casualty & Theft Losses (20)', 0, min = 0),
    numericInput('jobmisc', 'Job Expenses/Miscellaneous (27)', 0, min = 0),
    numericInput('othermisc', 'Other Misc Deductions (28)', 0, min = 0)
  ),
  mainPanel(
    tableOutput("taxtable")
    #plotOutput("taxgraph")
    
  )
)

