
function(input, output) {
  
  calctax <- reactive({
    ###############################################
    ########## Calculate Federal Taxes  ###########
    ###############################################
    
    #############PRE-TCJA#############
    
    #base itemized deductions
    tot_item_base <- input$med + input$intpd + input$state + input$realest +
    input$pptax + input$othtax + input$char + input$jobmisc + input$othermisc + input$caustheft
    
    #get pease agi threshhold
    pease_agi <- filter(pease_limits, status == input$status) %>%
      pull(limit)
    
    pease_base <- max(min(.03*(input$agi-pease_agi), .8*tot_item_base),0)
    
    st_ded_base <- filter(stded, status ==input$status, basealt == "Base") %>% 
      pull(standard_deduction)
    
    deductions_claimed_base <- max(st_ded_base, tot_item_base-pease_base)
    
    personal_exemptions <- ifelse(input$status == "Married Filing Jointly", 2, 1)*4150
    
    exemptions_base <- 4150 * input$dependents + personal_exemptions
    
    exemptions_phaseout_base <- max(0,
                                    min(personal_exemptions,
                                        personal_exemptions*.02*ceiling((input$agi-pease_agi)/2500))
    )
    
    exemptions_allowed <- max(0, exemptions_base-exemptions_phaseout_base)
    
    taxable_income_base <- max(0,
                               input$agi - max(st_ded_base, tot_item_base-pease_base) - exemptions_allowed)  
    
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
      `Deductions Claimed` = deductions_claimed_base,
      `Exemptions` = exemptions_base,
      `Personal Exemption Phaseout` = exemptions_phaseout_base,
      `Exemptions Allowed` = exemptions_allowed,
      `Federal Taxable Income` = taxable_income_base,
      `Capital Gains/Dividends Tax` = capgains_tax_base,
      `Ordinary Income Tax` = round(tax_base,0),
      `Total Federal Tax` = round(capgains_tax_base + tax_base, 0),
      `Child Credit` = child_credit_base,
      `Tax After (Non-Refundable) Child Credit` = tax_post_cc_base
    )) %>% 
      select(-scenario) %>% 
      map_df(scales::comma) %>% 
      gather(Item, `Pre TCJA (TY 2018 projected)`, AGI:`Tax After (Non-Refundable) Child Credit`)

    #############POST-TCJA#############
    
    #data_alt
    salt_limit <- ifelse(input$status == "Married Filing Separately", 5000, 10000)
    
    salt_alt <- min(salt_limit, input$state + input$realest + input$pptax + input$othtax)
    
    tot_item_alt <- input$med + input$intpd + salt_alt + input$char 
    
    st_ded_alt <- filter(stded, status ==input$status, basealt == "Alt") %>% 
      pull(standard_deduction)
    
    deductions_claimed_alt <- max(st_ded_alt, tot_item_alt)
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
    
    otherdepcred <- ifelse(input$child == input$dependents, 0,
                           ifelse(input$child < input$dependents, 500*(input$dependents-input$child), 0))
    
    child_credit_alt <- max(0,(otherdepcred+input$child * 2000) - child_phaseout_alt) 
    
    tax_post_cc_alt <- max(0,
                           round(capgains_tax_alt + tax_alt, 0)-child_credit_alt)
    
    data_alt <- as_tibble(list(
      scenario = "Alt",
      AGI = input$agi,
      `Capital Gains` = input$capgains,
      `Itemized Deductions` = tot_item_alt,
      `Pease Limitation on Deductions` = 0,
      `Standard Deduction` = st_ded_alt,
      `Deductions Claimed` =deductions_claimed_alt,
      `Exemptions` = exemptions_alt,
      `Personal Exemption Phaseout` = 0,
      `Exemptions Allowed` = exemptions_alt,
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
    
    ###############################################
    ######### Calculate Minnesota Taxes ###########
    ###############################################
    
    #calculate add-back in the base
    sttax_addback_base <- ifelse(deductions_claimed_base == st_ded_base, 0,
                       min(input$state, tot_item_base-pease_base-st_ded_base))
    
    #calculate subtraction for disallowed i.d. and personal exemptions
    disallowed_id_sub <- pease_base
    disallowed_exemp <- exemptions_phaseout_base
    disallowed_subtr <- pease_base + exemptions_phaseout_base
    
    #calculate mn deduction and exemption add-back
    state_id_limit <- 100000
    disallowed_id_add <- min(.03*input$agi - state_id_limit, 
                             .8*(tot_item_base- input$med - input$intpd - input$caustheft))
    
    disallowed_pe_add <- 0
    
    id_pe_limit_addback <- disallowed_id_add + disallowed_pe_add
    
    #calculate taxable income.
    mti_base <- max(0,taxable_income_base + sttax_addback_base-disallowed_subtr+id_pe_limit_addback)
    
    #calculate MN tax.
    mn_tax_base <- calculate_mntax(input$status, mti_base, "Base")
    
    

    
    #minnesota taxes base
    mntax_base <-  as_tibble(list(
      `Federal Taxable Income` = taxable_income_base,
      `State Taxes Add-back` = sttax_addback_base,
      `Disallowed Itemized Deductions and Exemptions Subtraction` = disallowed_subtr,
      `State Itemized Deduction and Personal Exemption Limitation` = id_pe_limit_addback,
      `Minnesota Taxable Income` = mti_base,
      `Minnesota Income Tax` = mn_tax_base
      
    )) %>% 
    map_df(round, digits = 0) %>% 
    map_df(scales::comma) %>%
    gather(Item, `Non-conformity`, `Federal Taxable Income`:`Minnesota Income Tax`)
  
    
    #calculate add-back under conformity  
    income_deducted <- min(min(salt_limit, input$state),
                           salt_limit - input$pptax -input$othtax - input$realest)
      
    sttax_addback_alt <- ifelse(deductions_claimed_alt == st_ded_alt, 0,
                           min(income_deducted, tot_item_alt-st_ded_alt))
    
    
    mti_alt <- max(0,taxable_income_alt + sttax_addback_alt)
    
    mn_tax_alt <- calculate_mntax(input$status, mti_alt, "Alt")
    
    #minnesota taxes base
    mntax_alt <-  as_tibble(list(
      `Federal Taxable Income` = taxable_income_alt,
      `State Taxes Add-back` = sttax_addback_alt,
      `Disallowed Itemized Deductions and Exemptions Subtraction` = 0, 
      `Minnesota Taxable Income` = mti_alt,
      `Minnesota Income Tax` = mn_tax_alt
    )) %>% 
    map_df(round, digits = 0) %>% 
    map_df(scales::comma) %>%
    gather(Item, `Conformity`, `Federal Taxable Income`:`Minnesota Income Tax`)
    
    mntax_output <- left_join(mntax_base, mntax_alt)
    
    #Output list to be used by tables and graphs and text
    list(output = output,
         oldtax = tax_post_cc_base,
         newtax = tax_post_cc_alt,
         mntax = mntax_output)
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
  
  output$mntax <- renderTable({
    calctax()[[4]]
  })

}