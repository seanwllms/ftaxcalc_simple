
function(input, output) {
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
  
    tot_item_base <- input$med + input$intpd + input$txpaid + input$char + 
      input$jobmisc + input$othermisc + input$caustheft
    
    st_ded_base <- filter(stded, status ==input$status, basealt == "Base") %>% 
      pull(standard_deduction)
    
    exemptions_base <- 4050 * (ifelse(input$status == "Married Filing Jointly", 2, 1) + input$dependents)
    
    taxable_income_base <- input$agi - max(st_ded_base, tot_item_base) - exemptions_base
    
    tax_base <- calculate_tax(input$status, taxable_income_base, "Base")
    
    data_base <- as_tibble(list(
      scenario = "Base",
      AGI = input$agi,
      `Capital Gains` = input$capgains,
      `Itemized Deductions` = tot_item_base,
      `Standard Deduction` = st_ded_base,
      `Exemptions` = exemptions_base,
      `Federal Taxable Income` = taxable_income_base,
      Tax = tax_base
    )) %>% 
      select(-scenario) %>% 
      t() 
    
    names(data) <- c("Pre-TCJA (TY 2017)")
               #, "Post-TCJA (TY 2018)")
    
    data_base
    
    tot_item_alt <- input$med + input$intpd + min(10000,input$txpaid) + input$char 
    
    st_ded_alt <- filter(stded, status ==input$status, basealt == "Alt") %>% 
      pull(standard_deduction)
    
    exemptions_alt <- 0
    
    taxable_income_alt <- input$agi - max(st_ded_alt, tot_item_alt) - exemptions_alt
    
    tax_alt <- 0 #calculate_tax(input$status, taxable_income_base, "Alt")
    
  }))
  
}