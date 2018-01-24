
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
    
    #SALT data_alt
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
    
    ############################################
    ########   TAXES UNDER NONCONFORMITY #######
    ############################################
    

####calculate state taxes add-back####   
  
    #calculate add-back in the base
    sttax_addback_base <- max(0,
                              ifelse(deductions_claimed_base == st_ded_base, 0, input$state)
    )
    
    #calculate subtraction for disallowed i.d. and personal exemptions
    disallowed_id_sub <- ifelse(deductions_claimed_base == st_ded_base, 0, pease_base)
    disallowed_exemp <- exemptions_phaseout_base
    disallowed_subtr <- pease_base + exemptions_phaseout_base
  
####calculate mn itemized deduction add back####
    
    #get threshold
    state_id_limit_base <- state_id_lim %>%
      filter(status == input$status, basealt == "Base") %>%
      pull(id_lim)
      
    #only add back i.d. if itemizing
    if (deductions_claimed_base == st_ded_base) {
      disallowed_id_add_base <- 0
    } else {
      #limit i.d. to limits
      disallowed_id_add_base <- min(.03*(input$agi - state_id_limit_base), 
                               .8*(tot_item_base- input$med - input$intpd - input$caustheft))
      
      #don't add back negative numbers
      disallowed_id_add_base <- max(disallowed_id_add_base, 0)
    }
    
####calculate mn personal exemption add back####
    
    #get limit
    state_pe_limit_base <- state_pe_lim %>%
      filter(status == input$status, basealt == "Base") %>%
      pull(pe_lim)
     
    #get phaseout rate
    phaseoutrate <- ifelse(input$status == "Married Filing Separately", 1250, 2500)
    
    #calculate disallowed p.e.
    disallowed_pe_add_base <- personal_exemptions*ceiling((input$agi-state_pe_limit_base)/phaseoutrate)*.02
    
    #limit to exemptions claimed and disallow negative numbers
    disallowed_pe_add_base <- max(0,min(disallowed_pe_add_base, exemptions_base))

#### Organize all of the add-back stufff ####
    
    #### Get combined add-back of i.d. and p.e #####
    id_pe_limit_addback_base <- disallowed_pe_add_base + disallowed_id_add_base
    
    #calculate add-backs total
    addbacks_base <-  sttax_addback_base + disallowed_id_add_base 
      
    if ((deductions_claimed_base - disallowed_id_add_base - sttax_addback_base) < st_ded_base) {
        addbacks_base <- deductions_claimed_base - st_ded_base
    }
    
  
##### Calculate Minnesota Taxable Income & Tax ##### 
   
    #Calculate MTI
    mti_base <- max(0,
                    taxable_income_base - disallowed_subtr + addbacks_base + disallowed_pe_add_base)
    
    #calculate MN tax.
    mn_tax_base <- calculate_mntax(input$status, mti_base, "Base")

      
##### Pull tax table togetehr for base ######
    #minnesota taxes base
    mntax_base <-  as_tibble(list(
      `Federal Taxable Income` = taxable_income_base,
      `- Disallowed Itemized Deductions and Exemptions Subtraction` = disallowed_subtr,
      `+ State Taxes Add-back` = sttax_addback_base,
      `+ State Itemized Deduction and Personal Exemption Limitation` = id_pe_limit_addback_base,
      `Minnesota Taxable Income` = mti_base,
      `Minnesota Income Tax` = mn_tax_base
    )) %>% 
    map_df(round, digits = 0) %>% 
    map_df(scales::comma) %>%
    gather(Item, `Non-conformity`, `Federal Taxable Income`:`Minnesota Income Tax`)
  
    ############################################
    ########   TAXES UNDER NONCONFORMITY #######
    ############################################
    
    
####calculate state taxes add-back####       
    income_deducted <- min(min(salt_limit, input$state),
                           salt_limit - input$pptax -input$othtax - input$realest)
      
    sttax_addback_alt <- max(0,
                             ifelse(deductions_claimed_alt == st_ded_alt, 0, income_deducted))
    
####calculate mn itemized deduction add back####
    
    #get threshold
    state_id_limit_alt <- state_id_lim %>%
      filter(status == input$status, basealt == "Alt") %>%
      pull(id_lim)
    
    #only add back i.d. if itemizing
    if (deductions_claimed_alt == st_ded_alt) {
      disallowed_id_add_alt <- 0
    } else {
    #limit i.d. to limits
      disallowed_id_add_alt <- min(.03*(input$agi - state_id_limit_alt), 
                                  .8*(tot_item_alt - input$med - input$intpd - input$caustheft))
    #don't add back negative numbers
      disallowed_id_add_alt <- max(disallowed_id_add_alt, 0)    
    }

####calculate mn personal exemption add back####
    disallowed_pe_add_alt <- 0

        
#### Organize all of the add-back stufff ####
    
    #### Get combined add-back of i.d. and p.e #####
    id_pe_limit_addback_alt <- disallowed_pe_add_alt + disallowed_id_add_alt
    
    #calculate add-backs total
    addbacks_alt <-  sttax_addback_alt + disallowed_id_add_alt
    
    if ((deductions_claimed_alt - disallowed_id_add_alt - sttax_addback_alt) < st_ded_alt) {
      addbacks_alt <- deductions_claimed_alt - st_ded_alt
    }
    
##### Calculate Minnesota Taxable Income & Tax ##### 
    
    #Calculate MTI
    mti_alt <- max(0,
                    taxable_income_alt + addbacks_alt)

    
    mn_tax_alt <- calculate_mntax(input$status, mti_alt, "Alt")

##### Pull tax table togetehr for alt ######
    #minnesota taxes alt
    mntax_alt <-  as_tibble(list(
      `Federal Taxable Income` = taxable_income_alt,
      `- Disallowed Itemized Deductions and Exemptions Subtraction` = 0,
      `+ State Taxes Add-back` = sttax_addback_alt,
      `+ State Itemized Deduction and Personal Exemption Limitation` = id_pe_limit_addback_alt,
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
    
    text <- paste0("After subtracting the child credit, the filer's 2018 federal tax under old law would have been <b>$",
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
    calctax()[[4]]},
    striped = TRUE, 
    spacing = 'xs'
  )

}