
function(input, output) {
  
  calctax <- reactive({
    
    #base itemized deductions
    tot_item_base <- input$med + input$intpd + input$txpaid + input$char + 
      input$jobmisc + input$othermisc + input$caustheft
    
    #get pease agi threshhold
    pease_agi <- filter(pease_limits, status == input$status) %>%
      pull(limit)
    
    pease_base <- max(min(.03*(input$agi-pease_agi), .8*tot_item_base),0)
    
    st_ded_base <- filter(stded, status ==input$status, basealt == "Base") %>% 
      pull(standard_deduction)
    
    exemptions_base <- 4050 * (ifelse(input$status == "Married Filing Jointly", 2, 1) + input$dependents)
    
    taxable_income_base <- max(0,
                               input$agi - max(st_ded_base, tot_item_base-pease_base) - exemptions_base)  
    
    capgains_tax_base <- max(0,
                             input$capgains * capgains_rate(input$status, taxable_income_base,"Base"))
    
    tax_base <- max(0,
                    calculate_tax(input$status, taxable_income_base-input$capgains, "Base"))
    
    cc_phase_floor_base <- filter(childcr, status==input$status, basealt == "Base") %>% 
      pull(ccred_phaseout)
    
    child_phaseout_base <- ceiling((max(0,as.numeric(input$agi) - cc_phase_floor_base))/1000)*50
    
    child_credit_base <- max(0,input$child * 1000 - child_phaseout_base)
    
    tax_post_cc_base <- max(0,
                            round(capgains_tax_base + tax_base, 0)-child_credit_base)
    
    data_base <- as_tibble(list(
      scenario = "Base",
      AGI = input$agi,
      `Capital Gains` = input$capgains,
      `Itemized Deductions` = tot_item_base,
      `Pease Limitation on Deductions` = pease_base,
      `Standard Deduction` = st_ded_base,
      `Deductions Claimed` = max(st_ded_base, tot_item_base-pease_base),
      `Exemptions` = exemptions_base,
      `Federal Taxable Income` = taxable_income_base,
      `Capital Gains/Dividends Tax` = capgains_tax_base,
      `Ordinary Income Tax` = round(tax_base,0),
      `Total Federal Tax` = round(capgains_tax_base + tax_base, 0),
      `Child Credit` = child_credit_base,
      `Tax After (Non-Refundable) Child Credit` = tax_post_cc_base
    )) %>% 
      select(-scenario) %>% 
      map_df(scales::comma) %>% 
      gather(Item, `Pre TCJA (TY 2017)`, AGI:`Tax After (Non-Refundable) Child Credit`)
    
    #data_alt
    salt_limit <- ifelse(input$status == "Married Filing Separately", 5000, 10000)
    
    tot_item_alt <- input$med + input$intpd + min(salt_limit,input$txpaid) + input$char 
    
    st_ded_alt <- filter(stded, status ==input$status, basealt == "Alt") %>% 
      pull(standard_deduction)
    
    exemptions_alt <- 0
    
    taxable_income_alt <- max(0,
                              input$agi - max(st_ded_alt, tot_item_alt)) 
    
    capgains_tax_alt <- max(0,
                            input$capgains * capgains_rate(input$status, taxable_income_alt,"Alt"))
    
    tax_alt <- max(0,
                   calculate_tax(input$status, taxable_income_alt-input$capgains, "Alt"))
    
    cc_phase_floor_alt <- filter(childcr, status==input$status, basealt == "Alt") %>% 
      pull(ccred_phaseout)
    
    child_phaseout_alt <- ceiling((max(0,as.numeric(input$agi) - cc_phase_floor_alt))/1000)*50
    
    child_credit_alt <- max(0,input$child * 2000 - child_phaseout_alt)
    
    tax_post_cc_alt <- max(0,
                           round(capgains_tax_alt + tax_alt, 0)-child_credit_alt)
    
    data_alt <- as_tibble(list(
      scenario = "Alt",
      AGI = input$agi,
      `Capital Gains` = input$capgains,
      `Itemized Deductions` = tot_item_alt,
      `Pease Limitation on Deductions` = 0,
      `Standard Deduction` = st_ded_alt,
      `Deductions Claimed` = max(st_ded_alt, tot_item_alt),
      `Exemptions` = exemptions_alt,
      `Federal Taxable Income` = taxable_income_alt,
      `Capital Gains/Dividends Tax` = capgains_tax_alt,
      `Ordinary Income Tax` = round(tax_alt,0),
      `Total Federal Tax` = round(capgains_tax_alt + tax_alt, 0),
      `Child Credit` = child_credit_alt,
      `Tax After (Non-Refundable) Child Credit` = tax_post_cc_alt
    )) %>% 
      select(-scenario) %>%
      map_df(scales::comma) %>% 
      gather(Item, `Post TCJA (TY 2018)`, AGI:`Tax After (Non-Refundable) Child Credit`)
    
    output <- left_join(data_base, data_alt) 
    
    list(output = output,
         oldtax = tax_post_cc_base,
         newtax = tax_post_cc_alt)
  })
  
  # output$taxgraph <- renderPlot({
  #   things_to_graph <- c("AGI", 
  #                        "Deductions Claimed", 
  #                        "Exemptions",
  #                        "Tax")
  #   
  #   plotdata <- calctax() %>% 
  #     set_names(c("Item", "Pre_TCJA", "Post_TCJA")) %>%
  #     gather(scen, value, Pre_TCJA, Post_TCJA) %>% 
  #     filter(Item %in% things_to_graph) 
  #   
  #   plot <-  ggplot(plotdata, aes(x=Item, y=value, fill = scen)) +
  #     geom_bar(stat="identity",
  #              position = position_dodge()) +
  #     coord_flip()
  #   
  #   plot
  # })
  # 
  
  output$summary <- renderText({
    oldtax <- scales::comma(calctax()[[2]])
    newtax <- scales::comma(calctax()[[3]])
    
    text <- paste0("After subtracting the child credit, the filer's 2017 federal tax under old law would have been <b>$",
                   oldtax,
                   ".</b> Under the Tax Cuts and Jobs Act in 2018, that will be <b>$", 
                   newtax, 
                   "</b>.")
    HTML(text)
    
  })
  output$taxtable <- renderTable({
    calctax()[[1]]
  })
  
}