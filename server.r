function(input, output, session) {
  
  agi <- reactiveVal(40000)
  status <- reactiveVal("Married Filing Jointly")
  dependents <- reactiveVal(2)
  child <- reactiveVal(2)
  capgains <- reactiveVal(0)
  med <- reactiveVal(0)
  intpd  <- reactiveVal(0)
  state <- reactiveVal(0)
  realest <- reactiveVal(0)
  pptax <- reactiveVal(0)
  othtax  <- reactiveVal(0)
  char <- reactiveVal(0)
  caustheft <- reactiveVal(0)
  jobmisc <- reactiveVal(0)
  othermisc <- reactiveVal(0)
  pct_itemize <- reactiveVal(.168)
  
  observeEvent(input$linktotp, {
    updateTabsetPanel(session, "tabs","tpdetailstab"
    )
  })
  
  observeEvent(input$example1, {
    status("Married Filing Jointly")
    agi(40000)
    intpd(0)
    state(0)
    realest(0)
    pptax(0)
    othtax(0)
    char(0)
    dependents(2)
    child(2)
    pct_itemize(.168)
  })

  observeEvent(input$example2, {
    status("Married Filing Jointly")
    agi(75000)
    intpd(0)
    state(0)
    realest(0)
    pptax(0)
    othtax(0)
    char(0)
    dependents(2)
    child(2)
    pct_itemize(.300)
  })

    observeEvent(input$example3, {
      status("Married Filing Jointly")
      agi(150000)
      intpd(7783)
      state(8249)
      realest(4162)
      pptax(374)
      othtax(41)
      char(3315)
      dependents(2)
      child(2)
      pct_itemize(.891)
    })
    
    observeEvent(input$example4, {
      status("Married Filing Jointly")
      agi(500000)
      intpd(12334)
      state(39038)
      realest(9102)
      pptax(426)
      othtax(240)
      char(13655)
      dependents(2)
      child(2)
      pct_itemize(.940)
    })

    observeEvent(input$example5, {
      status("Head of Household")
      agi(40000)
      intpd(0)
      state(0)
      realest(0)
      pptax(0)
      othtax(0)
      char(0)
      dependents(1)
      child(1)
      pct_itemize(.121)
    })

    observeEvent(input$example6, {
      status("Head of Household")
      agi(75000)
      intpd(6917)
      state(3704)
      realest(2249)
      pptax(127)
      othtax(21)
      char(901)
      dependents(1)
      child(1)
      pct_itemize(.562)
    })

    observeEvent(input$example7, {
      status("Head of Household")
      agi(150000)
      intpd(8917)
      state(9407)
      realest(4061)
      pptax(211)
      othtax(77)
      char(2366)
      dependents(1)
      child(1)
      pct_itemize(.870)
    })

    observeEvent(input$example8, {
      status("Head of Household")
      agi(500000)
      intpd(11311)
      state(37198)
      realest(7993)
      pptax(211)
      othtax(77)
      char(6296)
      dependents(1)
      child(1)
      pct_itemize(.975)
    })


    observeEvent(input$example9, {
      status("Single")
      agi(40000)
      intpd(0)
      state(0)
      realest(0)
      pptax(0)
      othtax(0)
      char(0)
      dependents(0)
      child(0)
      pct_itemize(.239)
    })

    observeEvent(input$example10, {
      status("Single")
      agi(75000)
      intpd(4459)
      state(4338)
      realest(2358)
      pptax(145)
      othtax(11)
      char(1497)
      dependents(0)
      child(0)
      pct_itemize(.635)
    })

    observeEvent(input$example11, {
      status("Single")
      agi(150000)
      intpd(6582)
      state(9009)
      realest(3620)
      pptax(232)
      othtax(12)
      char(2819)
      dependents(0)
      child(0)
      pct_itemize(.936)
    })

    observeEvent(input$example12, {
      status("Single")
      agi(500000)
      intpd(7373)
      state(49057)
      realest(7586)
      pptax(157)
      othtax(147)
      char(17393)
      dependents(0)
      child(0)
      pct_itemize(.979)
    })

    calctax <- reactive({
      ###############################################
      ########## Calculate Federal Taxes  ###########
      ###############################################

      #############PRE-TCJA#############

      #base itemized deductions
      tot_item_base <- med() + intpd() + state() + realest() +
      pptax() + othtax() + char() + jobmisc() + othermisc() + caustheft()

      #get pease agi threshhold
      pease_agi <- filter(pease_limits, status == status()) %>%
        pull(limit)

      pease_base <- max(min(.03*(agi()-pease_agi), .8*tot_item_base),0)

      st_ded_base <- filter(stded, status ==status(), basealt == "Base") %>%
        pull(standard_deduction)

      deductions_claimed_base <- max(st_ded_base, tot_item_base-pease_base)
      
      itemizer_base <- ifelse(deductions_claimed_base == st_ded_base, "Standard Ded.", "Itemizer")

      personal_exemptions <- ifelse(status() == "Married Filing Jointly", 2, 1)*4150

      exemptions_base <- 4150 * dependents() + personal_exemptions

      exemptions_phaseout_base <- ceiling((agi()-pease_agi)/2500)*.02*exemptions_base

      #limit to exemptions claimed and disallow numbers below zero.
      exemptions_phaseout_base <- max(0,
                                      min(exemptions_base, exemptions_phaseout_base))



      exemptions_allowed <- max(0, exemptions_base-exemptions_phaseout_base)

      taxable_income_base <- max(0,
                                 agi() - max(st_ded_base, tot_item_base-pease_base) - exemptions_allowed)

      capgains_tax_base <- max(0,
                               capgains() * capgains_rate(status(), taxable_income_base,"Base"))

      tax_base <- max(0,
                      calculate_tax(status(), taxable_income_base-capgains(), "Base"))

      cc_phase_floor_base <- filter(childcr, status==status(), basealt == "Base") %>%
        pull(ccred_phaseout)

      child_phaseout_base <- ceiling((max(0,as.numeric(agi()) - cc_phase_floor_base))/1000)*50

      child_credit_base <- max(0,child() * 1000 - child_phaseout_base)

      tax_post_cc_base <- max(0,
                              round(capgains_tax_base + tax_base, 0)-child_credit_base)

      data_base <- as_tibble(list(
        scenario = "Base",
        AGI = agi(),
        `Capital Gains` = capgains(),
        `Itemized Deductions` = tot_item_base,
        `Pease Limitation on Deductions` = pease_base,
        `Standard Deduction` = st_ded_base,
        `Deductions Claimed` = deductions_claimed_base,
        `Itemizer/Standard Deduction` = itemizer_base,
        `Exemptions` = exemptions_base,
        `Personal Exemption Phaseout` = exemptions_phaseout_base,
        `Exemptions Allowed` = exemptions_allowed,
        `Federal Taxable Income` = taxable_income_base,
        #`Capital Gains/Dividends Tax` = capgains_tax_base,
        `Tax` = round(tax_base,0),
        #`Total Federal Tax` = round(capgains_tax_base + tax_base, 0),
        `Child Credit` = child_credit_base,
        `Tax After (Non-Refundable) Child Credit` = tax_post_cc_base
      )) %>%
        select(-scenario) %>%
        map_df(scales::comma) %>%
        gather(Item, `Pre TCJA (TY 2018 projected)`, AGI:`Tax After (Non-Refundable) Child Credit`)

      #############POST-TCJA#############

      #SALT data_alt
      salt_limit <- ifelse(status() == "Married Filing Separately", 5000, 10000)

      salt_alt <- min(salt_limit, state() + realest() + pptax() + othtax())

      tot_item_alt <- med() + intpd() + salt_alt + char()

      st_ded_alt <- filter(stded, status ==status(), basealt == "Alt") %>%
        pull(standard_deduction)

      deductions_claimed_alt <- max(st_ded_alt, tot_item_alt)
      
      itemizer_alt <- ifelse(deductions_claimed_alt == st_ded_alt, "Standard Ded.", "Itemizer")
      
      
      exemptions_alt <- 0

      taxable_income_alt <- max(0,
                                agi() - max(st_ded_alt, tot_item_alt))

      capgains_tax_alt <- max(0,
                              capgains() * capgains_rate(status(), taxable_income_alt,"Alt"))

      tax_alt <- max(0,
                     calculate_tax(status(), taxable_income_alt-capgains(), "Alt"))

      cc_phase_floor_alt <- filter(childcr, status==status(), basealt == "Alt") %>%
        pull(ccred_phaseout)

      child_phaseout_alt <- ceiling((max(0,as.numeric(agi()) - cc_phase_floor_alt))/1000)*50

      otherdepcred <- ifelse(child() == dependents(), 0,
                             ifelse(child() < dependents(), 500*(dependents()-child()), 0))

      child_credit_alt <- max(0,(otherdepcred+child() * 2000) - child_phaseout_alt)

      tax_post_cc_alt <- max(0,
                             round(capgains_tax_alt + tax_alt, 0)-child_credit_alt)

      data_alt <- as_tibble(list(
        scenario = "Alt",
        AGI = agi(),
        `Capital Gains` = capgains(),
        `Itemized Deductions` = tot_item_alt,
        `Pease Limitation on Deductions` = 0,
        `Standard Deduction` = st_ded_alt,
        `Deductions Claimed` =deductions_claimed_alt,
        `Itemizer/Standard Deduction` = itemizer_alt,
        `Exemptions` = exemptions_alt,
        `Personal Exemption Phaseout` = 0,
        `Exemptions Allowed` = exemptions_alt,
        `Federal Taxable Income` = taxable_income_alt,
#        `Capital Gains/Dividends Tax` = capgains_tax_alt,
        `Tax` = round(tax_alt,0),
        #`Total Federal Tax` = round(capgains_tax_alt + tax_alt, 0),
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
                                ifelse(deductions_claimed_base == st_ded_base, 0, state())
      )

      #calculate subtraction for disallowed i.d. and personal exemptions
      disallowed_id_sub <- ifelse(deductions_claimed_base == st_ded_base, 0, pease_base)
      disallowed_exemp <- exemptions_phaseout_base
      disallowed_subtr <- pease_base + exemptions_phaseout_base

  ####calculate mn itemized deduction add back####

      #get threshold
      state_id_limit_base <- state_id_lim %>%
        filter(status == status(), basealt == "Base") %>%
        pull(id_lim)

      #only add back i.d. if itemizing
      if (deductions_claimed_base == st_ded_base) {
        disallowed_id_add_base <- 0
      } else {
        #limit i.d. to limits
        disallowed_id_add_base <- min(.03*(agi() - state_id_limit_base),
                                 .8*(tot_item_base- med() - intpd() - caustheft()))

        #don't add back negative numbers
        disallowed_id_add_base <- max(disallowed_id_add_base, 0)
      }

  ####calculate mn personal exemption add back####

      #get limit
      state_pe_limit_base <- state_pe_lim %>%
        filter(status == status(), basealt == "Base") %>%
        pull(pe_lim)

      #get phaseout rate
      phaseoutrate <- ifelse(status() == "Married Filing Separately", 1250, 2500)

      #calculate disallowed p.e.
      disallowed_pe_add_base <- exemptions_base*ceiling((agi()-state_pe_limit_base)/phaseoutrate)*.02

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
      mn_tax_base <- calculate_mntax(status(), mti_base, "Base")


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
      income_deducted <- min(min(salt_limit, state()),
                             salt_limit - pptax() -othtax() - realest())

      sttax_addback_alt <- max(0,
                               ifelse(deductions_claimed_alt == st_ded_alt, 0, income_deducted))

  ####calculate mn itemized deduction add back####

      #get threshold
      state_id_limit_alt <- state_id_lim %>%
        filter(status == status(), basealt == "Alt") %>%
        pull(id_lim)

      #only add back i.d. if itemizing
      if (deductions_claimed_alt == st_ded_alt) {
        disallowed_id_add_alt <- 0
      } else {
      #limit i.d. to limits
        disallowed_id_add_alt <- min(.03*(agi() - state_id_limit_alt),
                                    .8*(tot_item_alt - med() - intpd() - caustheft()))
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


      mn_tax_alt <- calculate_mntax(status(), mti_alt, "Alt")

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
           mntax = mntax_output,
           oldmntx = mn_tax_base,
           newmntx = mn_tax_alt)
    })

    output$tpdetails <- renderTable({
      
      as_tibble(list(`Filing Status` = status(),
             `Dependents` = dependents(),
             `Children 16 and Younger` = child(),
             `AGI` = agi(),
             `Interest Paid (Mortgage)` = intpd(),
             `State Income/Sales Taxes` = state(),
             `Real Estate Taxes` = realest(),
             `Personal Property Taxes` = pptax(),
             `Other Taxes` = othtax(),
             `Charitable Contributions` = char())) %>%
      map_df(scales::comma) %>%
      gather(Item, `Value`, `Filing Status`:`Charitable Contributions`)
    },
    striped = TRUE,
    spacing = 'xs')
  
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
      
      text <- glue("
After subtracting the child credit, \\
the taxpayer's 2018 federal tax under old \\
law would have been <b>${oldfedtax}.</b> \\
Under the Tax Cuts and Jobs Act \\
the tax will be <b>${newtax}.</b>
These numbers do not account for federal credits
other than the nonrefundable portion of the 
child credit.
                    ",
                    oldfedtax = oldtax,
                    newfedtax = newtax)

      text

    })


    output$mnsummary <- renderText({
      oldtax <- scales::comma(round(calctax()[[5]]),0)
      newtax <- scales::comma(round(calctax()[[6]]),0)

      text <- glue("
                    The filer's 2018 Minnesota tax before credits under \\
                    non-conformity would be <b>${oldmntax}.</b> \\
                    If Minnesota conforms and makes no other \\
                    changes, the filer's tax before credits will be <b>${newmntax}.</b>
                    This assumes Minnesota conforms to the TCJA by updating
                    its statutory references to the Internal Revenue Code to reflect the TCJA
                    and makes no other policy changes.
                    ",
                   oldmntax = oldtax,
                   newmntax = newtax)

      text

    })

    output$caveats <- renderText({
      text <- glue("

  <h3>How this works</h3>
  <p>This calculator only models the effects of the Tax Cuts and
  Jobs Act (TCJA) on individual tax liability; <b>it does not account
  for changes to federal law regarding pass-through entities and
  corporations.</b></p>
  <p>Estimates of Minnesota liability under conformity assume
  Minnesota conforms to the TCJA by updating
  its statutory references to the Internal Revenue Code,
  while making no other policy changes in response to the new law. </p>
  <p>In order to accurately model the effects of the TJCA on individual
  federal and state taxes, the application makes a few assumptions:</p>
  <ol><li>The calculator starts with federal adjusted gross income.
  This means that it does not account for changes to \"above the line\"
  deductions under the TCJA. For example, it does
  not account for the elimination of the federal moving expense deduction.</li>
  <li>The calculator assumes all filers are under age 65, and therefore
  inelgible for the additional standard deduction for filers ages 65 and older.</li>
  <li>The calculator does not account for all changes to federal itemized
  deductions. Provisions not accounted for include:
  <ul><li>The decrease in the AGI threshhold for the medical
  expense deduction.</li>
  <li>The elimination of the deduction for home equity loan interest.</li>
  <li>The reduction on the debt principal limitaiton for the
  mortgage interest deduction.</ul>
   </li>
  <li>The calculator does not account for the effects of the TCJA on
  the state and federal Alternative Minimum tax.</li>
  <li>The Department of Revenue has interpreted Minnesota law as
  requiring consistent federal and state elections for the purposes of claiming
  either the standard deduction or itemizing. In the case of non-conformity,
  filers with itemized deductions that are greater than the Minnesota
  standard deduction but less than the federal standard deduction may
  benefit from itemizing their deductions. The calculator assumes
  that the filer would claim the federal standard deduction, even if that would
  increase their Minnesota tax liability.</li>
  <li>The calculator does not account for taxpayers who become newly eligible for
  the Minnesota Non-itemizer Charitable Contribution subtraction under conformity.
  Taxpayers who itemized under old law but claim the standard deduction under conformity
  may become newly eligible for the non-itemizer subtraction. This may reduce their
  Minnesota tax liability.</li>
  <li>When calculating state income tax add-back under conformity, filers with
  more than $10,000 in combined property and income taxes are assumed to
  deduct their property taxes first. This is in accorance with the Department of Revenue's
  assumption about how conformity would be administered.</li>
  </ol>
  <p align=right>Created by the <b>Minnesota House Research Department</b>, February 2018.</p>
                   ")
      text
    })
    
    output$deductionspiel <- renderText({
      pct <- scales::percent(pct_itemize())
      filingstatus <- status()
      assignment <- ifelse(pct_itemize() < .5, 
                          "Filer claims the standard deduction.",
                          "Filer itemizes.")
      text <-glue(
"About <b>{pct}</b> of {filingstatus} filers with similar incomes itemize their deductions. {assignment}"
      )
      text
    })
    
    output$taxtable <- renderTable({
      calctax()[[1]]},
      striped = TRUE,
      spacing = 'xs'
    )

    output$mntax <- renderTable({
      calctax()[[4]]},
      striped = TRUE,
      spacing = 'xs'
    )
    output$disclaimer <- renderText({
      disclaimer <- glue(
"<b>Disclaimer:</b> This website is intended to inform the policymaking process of the Minnesota legislature. 
The numbers presented are estimates, and do not account for all of the changes included in 
the recent federal tax law. These estimates are intended as illustrations and examples and 
are not suitable for judging specific impacts on any individual or for tax preparation or 
personal planning purposes. Please do not cite without permission."
      )
      disclaimer
    })
}