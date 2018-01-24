
# Define UI for application
pageWithSidebar(
  headerPanel('TCJA Tax Calculator'),
  sidebarPanel(
    selectInput('status', 'Filing Status', choices = c("Married Filing Jointly",
                                                       "Single",
                                                       "Head of Household",
                                                       "Married Filing Separately")),
    numericInput('dependents', "# of Dependents", 2, min=0),
    numericInput('child', "Children 16 and Younger", 2, min=0),
    numericInput('agi', 'Adjusted Gross Income', 63217),
    numericInput('capgains', 'Long-term Dividends/Qualified Capital Gains', 0),
    h3("Itemized Deductions"),
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
  ),
  mainPanel(
    h3("Federal Taxes"),
    tableOutput("taxtable"),
    htmlOutput("summary"),
    h3("Minnesota Taxes"),
    tableOutput("mntax")
    #plotOutput("taxgraph")
    
  )
)

