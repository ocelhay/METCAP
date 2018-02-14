# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# server.R
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# METCAP Shiny App
# Authors: Olivier Celhay - olivier.celhay@gmail.com, Sheetal Silal - Sheetal.Silal@uct.ac.za
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



shinyServer(
  function(input, output, session) {
    
    
    # help videos
    observeEvent(input$help_video_welcome, {
      showModal(modalDialog(
        title = NULL, size="m",
        includeMarkdown("./www/markdown/help_video_welcome.md"),
        easyClose = TRUE,
        footer = modalButton("Close", icon = NULL)
      ))
    })
    
    observeEvent(input$help_video_data, {
      showModal(modalDialog(
        title = NULL, size="m",
        includeMarkdown("./www/markdown/help_video_data.md"),
        easyClose = TRUE,
        footer = modalButton("Close", icon = NULL)
      ))
    })
    
    observeEvent(input$help_video_model1, {
      showModal(modalDialog(
        title = NULL, size="m",
        includeMarkdown("./www/markdown/help_video_model1.md"),
        easyClose = TRUE,
        footer = modalButton("Close", icon = NULL)
      ))
    })
    
    observeEvent(input$help_video_model2, {
      showModal(modalDialog(
        title = NULL, size="m",
        includeMarkdown("./www/markdown/help_video_model2.md"),
        easyClose = TRUE,
        footer = modalButton("Close", icon = NULL)
      ))
    })
    
    observeEvent(input$help_video_model3, {
      showModal(modalDialog(
        title = NULL, size="m",
        includeMarkdown("./www/markdown/help_video_model3.md"),
        easyClose = TRUE,
        footer = modalButton("Close", icon = NULL)
      ))
    })
    
    
    
    # select/deselect countries based on selected region
    observe({
      if (input$region == 'Asia-Pacific'){
        updateCheckboxGroupInput(session = session, "countries", "Countries:",
                                 choiceNames = mapply(vector_countries, flags, FUN = function(vector_countries, flagUrl) {
                                   tagList(
                                     tags$img(src=flagUrl, width=20, height=20),
                                     vector_countries)}, SIMPLIFY = FALSE, USE.NAMES = FALSE), choiceValues = vector_countries, inline = FALSE, selected=vector_countries)
      }
      
      if (input$region == 'Western Pacific Region'){
        updateCheckboxGroupInput(session=session, "countries", 'Countries:',
                                 choiceNames = mapply(vector_WP, flags_WP, FUN = function(vector_WP, flagUrl) {
                                   tagList(
                                     tags$img(src=flagUrl, width=20, height=20),
                                     vector_WP)}, SIMPLIFY = FALSE, USE.NAMES = FALSE), choiceValues = vector_WP, inline = FALSE, selected=vector_WP)
      }
      if (input$region == 'South-East Asia Region'){
        updateCheckboxGroupInput(session=session, "countries", 'Countries:',
                                 choiceNames = mapply(vector_SEA, flags_SEA, FUN = function(vector_SEA, flagUrl) {
                                   tagList(
                                     tags$img(src=flagUrl, width=20, height=20),
                                     vector_SEA)}, SIMPLIFY = FALSE, USE.NAMES = FALSE), choiceValues = vector_SEA, inline = FALSE, selected=vector_SEA)
      }
      if (input$region == 'Eastern Mediterranean Region'){
        updateCheckboxGroupInput(session=session, "countries", 'Countries:',
                                 choiceNames = mapply(vector_EMR, flags_EMR, FUN = function(vector_EMR, flagUrl) {
                                   tagList(
                                     tags$img(src=flagUrl, width=20, height=20),
                                     vector_EMR)}, SIMPLIFY = FALSE, USE.NAMES = FALSE), choiceValues = vector_EMR, inline = FALSE, selected=vector_EMR)
      }
      if (input$region == 'Greater Mekong Subregion'){
        updateCheckboxGroupInput(session=session, "countries", 'Countries:',
                                 choiceNames = mapply(vector_GMS, flags_GMS, FUN = function(vector_GMS, flagUrl) {
                                   tagList(
                                     tags$img(src=flagUrl, width=20, height=20),
                                     vector_GMS)}, SIMPLIFY = FALSE, USE.NAMES = FALSE), choiceValues = vector_GMS, inline = FALSE, selected=vector_GMS)
      }
      if (input$region == 'Custom'){
        updateCheckboxGroupInput(session=session, "countries", 'Countries:',
                                 choiceNames = mapply(vector_countries, flags, FUN = function(vector_countries, flagUrl) {
                                   tagList(
                                     tags$img(src=flagUrl, width=20, height=20),
                                     vector_countries)}, SIMPLIFY = FALSE, USE.NAMES = FALSE), choiceValues = vector_countries, inline = FALSE, selected=NULL)
      }
    })
    
    
    # Data section
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # Map
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # subset of shapefile with selected countries (also used in model1 section)
    shp_APLMA_sub <- reactive({
      if(length(input$countries)==0) return(shp_APLMA)
      subset(shp_APLMA, shp_APLMA$country %in% input$countries)
    })
    
    # define a palette related to selected indicator
    pal <- reactive({
      switch(input$indicator_map,
             population_at_risk=colorNumeric(palette = rev(brewer.pal(n=11, name='RdYlBu')), domain = c(0, max(shp_APLMA_sub()$population_at_risk, na.rm=T))),
             G6PD_rates=colorBin(palette = rev(brewer.pal(n=8, name='RdYlGn')), domain = c(0, 24), bins=6),
             API_total=colorNumeric(palette = rev(brewer.pal(n=11, name='RdYlBu')), domain = c(0, max(shp_APLMA_sub()$API_total, na.rm=T))),
             ratio_nc_pf=colorNumeric(palette = rev(brewer.pal(n=11, name='RdYlBu')), domain = c(0, 100)),
             API_ncpf=colorNumeric(palette = rev(brewer.pal(n=11, name='RdYlBu')), domain = c(0, max(shp_APLMA_sub()$API_ncpf, na.rm=T))),
             API_ncpv=colorNumeric(palette = rev(brewer.pal(n=11, name='RdYlBu')), domain = c(0, max(shp_APLMA_sub()$API_ncpv, na.rm=T))),
             IRS_cov=colorBin(palette = 'RdYlGn', domain = c(0, 100), bins=10),
             ITN_cov=colorBin(palette = 'RdYlGn', domain = c(0, 100), bins=10),
             AM_cov=colorBin(palette = 'RdYlGn', domain = c(0, 100), bins=10),
             ACT_cov=colorBin(palette = 'RdYlGn', domain = c(0, 100), bins=10)
      )})
    
    # define the title
    title_map <- reactive({
      switch(input$indicator_map,
             population_at_risk='Population at risk',
             G6PD_rates='G6PD deficiency rates',
             API_total='1000*(Total Cases/Population At Risk)',
             ratio_nc_pf='Ratio of Pf cases \n(Non Community Cases)',
             # API_ncpf='Non Community Pf cases<br>/1000 pop@risk',
             # API_ncpv='Non Community Pv cases<br>/1000 pop@risk',
             IRS_cov='%IRS Coverage',
             ITN_cov='%ITN Coverage',
             AM_cov='% Any Antimalarial Coverage',
             ACT_cov='% ACT Coverage')
    })
    
    # map of selected countries
    map_dist <- reactive({
      shp_APLMA_sub <- shp_APLMA_sub() 
      shp_APLMA_sub@data <- shp_APLMA_sub@data %>% mutate_(indicator=input$indicator_map)
      shp_APLMA_sub$popup <- paste0('<em>', shp_APLMA_sub$country, '</em><br>',
                                    'Population At Risk: ', format(shp_APLMA_sub$population_at_risk, big.mark=','), '<br>',
                                    'G6PD Deficiency Rate: ', shp_APLMA_sub$G6PD_rates, '<br>',
                                    'Ratio of Pf cases \n(Non Community Cases): ', shp_APLMA_sub$ratio_nc_pf, '<br>',
                                    '1000*(Total Cases/Population At Risk): ', shp_APLMA_sub$API_total, '<br>',
                                    '%IRS Coverage: ', shp_APLMA_sub$IRS_cov, '<br>',
                                    '%ITN Coverage: ', shp_APLMA_sub$ITN_cov, '<br>',
                                    '% Any Antimalarial Coverage: ', shp_APLMA_sub$AM_cov, '<br>',
                                    '% ACT Coverage: ', shp_APLMA_sub$ACT_cov, '<br>')
      palette <- pal()
      
      return(leaflet(shp_APLMA_sub) %>%
               addProviderTiles('Esri.WorldGrayCanvas', options = tileOptions(minZoom=3, maxZoom=5)) %>%
               addPolygons(fillOpacity=0.6, dashArray=1, weight=1.5, color='#B6BCB3', fillColor=~palette(indicator), popup=~popup) %>%
               addLegend('bottomleft', pal = palette, values=~indicator, title = title_map(), labFormat=labelFormat(), opacity=1)
      )
      
    })
    
    output$map_data <- renderLeaflet({map_dist()})
    
    
    # Longitudinal data
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # whole region
    output$ts_data_all <- renderPlot({
      if(input$indicator_ts %in% c('pop-risk-high', 'cover-itn', 'cover-irs', 'cover-antimal', 'cover-act', 'ratio_nc_pf')) return({
        plot(1, xaxt='n', yaxt='n', bty='n', pch='', ylab='', xlab='')
        text(1, 1, 'No Regional Aggregation for selected indicator', cex=2)
      })
      
      df <- data_year %>% filter(indic==input$indicator_ts, country %in% input$countries) %>%
        group_by(year) %>%
        summarise(total=sum(total, na.rm=T)) %>%
        ungroup()
      
      ggplot(df, aes(year, total)) +
        geom_line() + geom_point() +
        ggtitle('All selected countries') +
        scale_y_continuous(labels = comma) +
        theme_bw() + theme(axis.title=element_blank(), axis.text=element_text(size = 12))
    })
    
    # grid of countries
    output$ts_data <- renderPlot({
      if(length(input$countries)==0) return(NULL)
      
      
      if(input$indicator_ts %in% c('cover-itn', 'cover-irs', 'cover-antimal', 'cover-act')) return({
        ggplot(data_year %>% filter(indic==input$indicator_ts, country %in% input$countries), 
               aes(year, total/100)) +
          geom_line() + geom_point() +
          scale_y_continuous(labels = scales::percent) +
          facet_wrap(~country, scale='free_y') +
          theme_bw() + theme(axis.title=element_blank(), axis.text=element_text(size = 12))
      })
      
      ggplot(data_year %>% filter(indic==input$indicator_ts, country %in% input$countries), 
             aes(year, total)) +
        geom_line() + geom_point() +
        scale_y_continuous(labels = comma) +
        facet_wrap(~country, scale='free_y') +
        theme_bw() + theme(axis.title=element_blank(), axis.text=element_text(size = 12))
      
    })
    
    
    # Model Phase 1 section
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # map
    map_dist_model1 <- reactive({
      shp_APLMA_sub <- shp_APLMA_sub() 
      shp_APLMA_sub@data <- shp_APLMA_sub@data %>% mutate_(indicator=input$indicator_map_model1)
      shp_APLMA_sub$popup <- paste0('<em>', shp_APLMA_sub$country, '</em><br><br>',
                                    'Health Seeking Proportions:',
                                    '<ul>',
                                    '<li>Minimum: ', shp_APLMA_sub$minimum, '</li>',
                                    '<li>Median: ', shp_APLMA_sub$median, '</li>',
                                    '<li>Maximum:', shp_APLMA_sub$maximum, '</li>',
                                    '</ul>')
      
      palette <- colorNumeric(palette = brewer.pal(n=11, name='RdYlBu'), domain = c(0, 1))
      
      return(leaflet(shp_APLMA_sub) %>%
               addProviderTiles('Esri.WorldGrayCanvas', options = tileOptions(minZoom=3, maxZoom=5)) %>%
               addPolygons(fillOpacity=0.6, dashArray=1, weight=1.5, color='#B6BCB3', fillColor=~palette(indicator), popup=~popup) %>%
               addLegend('bottomleft', pal = palette, values=c(0,1), title = 'Proportions', labFormat=labelFormat(), opacity=1))
      
    })
    
    output$map_model1 <- renderLeaflet({map_dist_model1()})
    
    # longitudinal data, Pf
    output$ts_model1_pf <- renderPlot({
      if(length(input$countries)==0) return(NULL)
      
      ggplot(burden %>% 
               filter(country %in% input$countries), aes()) +
        geom_ribbon(aes(year, ymin=`Minimum total burden Pf`, ymax=`Maximum total burden Pf`), alpha=0.2) +
        geom_line(aes(year, `Median total burden Pf`)) +
        scale_y_continuous(labels = comma) +
        labs(title='Estimated clinical Pf burden, 2000 to 2015', subtitle='[Minimum, Median, Maximum]') +
        facet_wrap(~country, scale='free_y') +
        theme_bw() + theme(axis.title=element_blank())
    })
    
    # longitudinal data, Pv
    output$ts_model1_pv <- renderPlot({
      if(length(input$countries)==0) return(NULL)
      
      ggplot(burden %>% 
               filter(country %in% input$countries), aes()) +
        geom_ribbon(aes(year, ymin=`Minimum total burden Pv`, ymax=`Maximum total burden Pv`), alpha=0.2) +
        geom_line(aes(year, `Median total burden Pv`)) +
        scale_y_continuous(labels = comma) +
        labs(title='Estimated clinical Pv burden, 2000 to 2015', subtitle='[Minimum, Median, Maximum]') +
        facet_wrap(~country, scale='free_y') +
        theme_bw() + theme(axis.title=element_blank())
    })
    
    
    
    # Model Phase 2 section
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # model calibration, Pf
    output$model_calibration_pf <- renderPlot({
      ggplot(burden %>% filter(country %in% input$countries), aes()) +
        geom_ribbon(aes(year, ymin=`Minimum total burden Pf`, ymax=`Maximum total burden Pf`), alpha=0.6) +
        geom_line(aes(year, `Median total burden Pf`)) +
        geom_line(data=calibration %>% filter(country %in% input$countries), aes(year, pf_med), color='red') +
        geom_ribbon(data=calibration %>% filter(country %in% input$countries),
                    aes(year, ymin=pf_min, ymax=pf_max), alpha=0.6, fill='red') +
        scale_y_continuous(labels = comma) +
        labs(title='Estimated clinical Pf burden and Model Calibration, Absolute numbers - 2000 to 2015', subtitle='[Minimum, Median, Maximum]') +
        facet_wrap(~country, scale='free_y') +
        theme_bw() + theme(axis.title=element_blank())
    })
    
    # model calibration, Pv
    output$model_calibration_pv <- renderPlot({
      ggplot(burden %>% filter(country %in% input$countries), aes()) +
        geom_ribbon(aes(year, ymin=`Minimum total burden Pv`, ymax=`Maximum total burden Pv`), alpha=0.6) +
        geom_line(aes(year, `Median total burden Pv`)) +
        geom_line(data=calibration %>% filter(country %in% input$countries), aes(year, pv_med), color='blue') +
        geom_ribbon(data=calibration %>% filter(country %in% input$countries),
                    aes(year, ymin=pv_min, ymax=pv_max), alpha=0.6, fill='blue') +
        scale_y_continuous(labels = comma) +
        labs(title='Estimated clinical Pv burden and Model Calibration, Absolute numbers - 2000 to 2015', subtitle='[Minimum, Median, Maximum]') +
        facet_wrap(~country, scale='free_y') +
        theme_bw() + theme(axis.title=element_blank())
    })
    
    
    # scenation comparison
    
    input_scenario <- reactive({c(input$scenario_reverse, input$scenario_continue, input$scenario_accelerate, input$scenario_innovate)})
    
    mda_reac <- reactive({
      switch(input$mda,
             'With MDA'='No',
             'Without MDA'='Yes',
             'Show both'='')
    })
    
    itn_reac <- reactive({
      switch(input$itn,
             'Scale-up ITN'='No',
             'Maintain ITN'='Yes',
             'Show both'='')
    })
    
    
    # message counterfactual scenarios
    output$message_counterfactual <- renderText({
      ifelse(
        any((scenario_codes %>% filter(scenario %in% input_scenario(), 
                                       resistance==input$resistance,
                                       MDA!=mda_reac(),
                                       ITN!=itn_reac()))$show == "No"),
        '<font size="3" color=" red">At least one selected scenario is counterfactual: scenario shows the predicted impact of stopping ITN activities and scaling up ITN is not allowed for this scenario.</font>',
        "")  
    })
    
    # all countries
    output$ts_model2_global <- renderPlot({
      
      # take dependancy
      input$goButton
      isolate({
        if(length(input$countries)==0 | input$goButton==0) return(NULL)
        
        
        # call to model
        df <- model %>% filter(country %in% input$countries, 
                               scenario %in% input_scenario(), 
                               prediction==input$prediction,
                               resistance==input$resistance,
                               MDA!=mda_reac(),
                               ITN!=itn_reac()) %>%
          group_by(year, prediction, scenario, estimate, resistance, MDA, ITN) %>%
          summarise(total=sum(total), total_pop_at_risk=sum(population_at_risk)) %>%
          ungroup() %>%
          arrange(scenario) %>%
          spread(estimate, total) %>%
          mutate(Maximum_Log=log10(1000*Maximum/total_pop_at_risk), Median_Log=log10(1000*Median/total_pop_at_risk), Minimum_Log=log10(1000*Minimum/total_pop_at_risk))
        
        elim_threshold_abs <- sum((country_codes %>% filter(country %in% input$countries))$`Elim Threshold (abs numbers)`)
        elim_threshold_log <- log10(0.0192)
        
        
        
        # common elements to all plots
        g <- ggplot(df, aes(x=year, color=scenario, fill=scenario, shape=MDA, linetype=ITN)) +
          scale_y_continuous(labels = comma) +
          scale_color_manual(values=scenario_colors, name='Scenario') +
          scale_fill_manual(values=scenario_colors, name='Scenario') +
          scale_linetype_manual(name='ITN', values=c('No'=1, 'Yes'=2)) +
          scale_shape_manual(name='MDA', values=c('No'=15, 'Yes'=17)) +
          labs(title=ifelse(input$resistance=="Yes", paste0(input$prediction, " with increasing resistance"), paste0(input$prediction, " with stable resistance"))) +
          theme_bw() + theme(axis.title=element_blank(), axis.text=element_text(size = 13), 
                             title=element_text(size = 14), legend.text=element_text(size=13))
        
        
        # depending on selected scale/indicator, customisation of the plot
        if(input$scale=='Absolute numbers' & input$prediction %in% c('Reported Incidence Pf + Pv + Mix Pf/Pv', 'Estimated Incidence Pf + Pv + Mix Pf/Pv')){
          return(
            g + 
              geom_line(aes(y=Median), size=1) +
              geom_point(aes(y=Median), size=5) +
              geom_ribbon(aes(ymin=Minimum, ymax=Maximum), alpha=0.1, size=1) +
              labs(subtitle=paste0('Model Prediction, ', input$scale)) +
              geom_hline(aes(yintercept=elim_threshold_abs), color='blue', lty=2) +
              annotate('label', 2015.5, elim_threshold_abs, label = 'Elimination', color='blue', size=6)
          )}
        
        if(input$scale=='Absolute numbers' & !input$prediction %in% c('Reported Incidence Pf + Pv + Mix Pf/Pv', 'Estimated Incidence Pf + Pv + Mix Pf/Pv')){
          return(
            g + 
              geom_line(aes(y=Median), size=1) +
              geom_point(aes(y=Median), size=5) +
              geom_ribbon(aes(ymin=Minimum, ymax=Maximum), alpha=0.1, size=1) +
              labs(subtitle=paste0('Model Prediction, ', input$scale))
          )}
        
        if(input$scale=='Log scale' & input$prediction %in% c('Reported Incidence Pf + Pv + Mix Pf/Pv', 'Estimated Incidence Pf + Pv + Mix Pf/Pv')){
          return(
            g + 
              geom_line(aes(y=Median_Log), size=1) +
              geom_point(aes(y=Median_Log), size=5) +
              geom_ribbon(aes(ymin=Minimum_Log, ymax=Maximum_Log), alpha=0.1, size=1) +
              labs(subtitle=paste0('Model Prediction, ', input$scale)) +
              geom_hline(aes(yintercept=elim_threshold_log), color='blue', lty=2) +
              annotate('label', 2015.5, elim_threshold_log, label = 'Elimination', color='blue', size=6)
          )}
        
        if(input$scale=='Log scale' & !input$prediction %in% c('Reported Incidence Pf + Pv + Mix Pf/Pv', 'Estimated Incidence Pf + Pv + Mix Pf/Pv')){
          return(
            g + 
              geom_line(aes(y=Median_Log), size=1) +
              geom_point(aes(y=Median_Log), size=5) +
              geom_ribbon(aes(ymin=Minimum_Log, ymax=Maximum_Log), alpha=0.1, size=1) +
              labs(subtitle=paste0('Model Prediction, ', input$scale))
          )}
        
      })
    })
    
    # grid of countries
    output$ts_model2_grid <- renderPlot({
      
      
      
      # take dependancy
      input$goButton
      
      isolate({
        
        if(length(input$countries)==0 | input$goButton==0) return(NULL)
        # call to model
        df <- model %>% filter(country %in% input$countries, 
                               scenario %in% input_scenario(), 
                               prediction==input$prediction, 
                               resistance==input$resistance,
                               MDA!=mda_reac(),
                               ITN!=itn_reac()) %>%
          arrange(scenario) %>%
          spread(estimate, total) %>%
          mutate(Maximum_Log=log10(1000*Maximum/population_at_risk), Median_Log=log10(1000*Median/population_at_risk), Minimum_Log=log10(1000*Minimum/population_at_risk))
        
        
        # common elements to all plots
        g <- ggplot(df, aes(x=year, fill=scenario, color=scenario, shape=MDA, linetype=ITN)) +
          facet_wrap(~country, scales = "free_y") +
          scale_y_continuous(labels = comma) +
          scale_fill_manual(values=scenario_colors, name='Scenario') +
          scale_color_manual(values=scenario_colors, name='Scenario') +
          scale_shape_manual(name='MDA', values=c('No'=15, 'Yes'=17)) +
          scale_linetype_manual(name='ITN', values=c('No'=1, 'Yes'=2)) +
          labs(title=ifelse(input$resistance=="Yes", paste0(input$prediction, " with increasing resistance"), paste0(input$prediction, " with stable resistance"))) +
          theme(axis.title=element_blank(), legend.position='none')
        
        
        # depending on selected scale/indicator, customisation of the plot
        if(input$scale=='Absolute numbers' & input$prediction %in% c('Reported Pf + Mix Pf/Pv Cases', 'Estimated Incidence Pf + Pv + Mix Pf/Pv')){
          return(
            g + 
              geom_line(aes(y=Median)) +
              geom_ribbon(aes(ymin=Minimum, ymax=Maximum), alpha=0.7) +
              geom_hline(data=country_codes %>% filter(country %in% input$countries), aes(yintercept=`Elim Threshold (abs numbers)`), color='blue', lty=2) +
              labs(subtitle=paste0('Model Prediction, ', input$scale))
          )}
        
        if(input$scale=='Absolute numbers' & !input$prediction %in% c('Reported Pf + Mix Pf/Pv Cases', 'Estimated Incidence Pf + Pv + Mix Pf/Pv')){
          return(
            g + 
              geom_line(aes(y=Median)) +
              geom_ribbon(aes(ymin=Minimum, ymax=Maximum), alpha=0.7) +
              labs(subtitle=paste0('Model Prediction, ', input$scale))
          )}
        
        if(input$scale=='Log scale' & input$prediction %in% c('Reported Pf + Mix Pf/Pv Cases', 'Estimated Incidence Pf + Pv + Mix Pf/Pv')){
          return(
            g +
              geom_line(aes(y=Median_Log)) +
              geom_point(aes(y=Median_Log)) +
              geom_ribbon(aes(ymin=Minimum_Log, ymax=Maximum_Log), alpha=0.7) +
              geom_hline(data=country_codes %>% filter(country %in% input$countries), aes(yintercept=`Elim Threshold (log inc/1000pop@risk)`), color='blue', lty=2) +
              labs(subtitle=paste0('Model Prediction, ', input$scale,  ' per 1,000 population at risk'))
          )}
        
        if(input$scale=='Log scale' & !input$prediction %in% c('Reported Pf + Mix Pf/Pv Cases', 'Estimated Incidence Pf + Pv + Mix Pf/Pv')){
          return(
            g +
              geom_line(aes(y=Median_Log)) +
              geom_point(aes(y=Median_Log)) +
              geom_ribbon(aes(ymin=Minimum_Log, ymax=Maximum_Log), alpha=0.7) +
              labs(subtitle=paste0('Model Prediction, ', input$scale,  ' per 1,000 population at risk'))
          )}
        
      })
    })
    
    
    
    
    # Minimum Elimination Package
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # maps
    output$elimination_minimal_scenario_2 <- output$elimination_minimal_scenario <- renderLeaflet({
      shp_APLMA_sub <- subset(shp_APLMA, shp_APLMA$country %in% input$countries)
      
      sub_labels <- data.frame(
        scenarios=c('Predicted elimination achieved by 2017', 'Single dose new Pv treatment with ITN scale-up', 'New LLINs with ITN scale-up',
                    'Effective Usage with ITN scale-up and MDA', 'New Pf drug with ITN scale-up and MDA', 'Business as usual',
                    'Universal Coverage', 'IRS', 'Effective Usage', 'Single dose new Pv treatment',
                    'Effective Usage with MDA', 'New LLINs with MDA'),
        colors=c(brewer.pal(9, 'Set1')[1:9], brewer.pal(3, 'Set2')[1:3])) %>%
        filter(scenarios %in% ((country_codes %>% filter(country %in% input$countries))$scenario_min_elim))
      
      shp_APLMA_sub@data <- left_join(shp_APLMA_sub@data, country_codes, by='country') %>%
        mutate(popup=paste0(country, ':<br>', 
                            'Minimum elimination package: ', scenario_min_elim, '<br>',
                            'Elimination range: ', Range, '<br>',
                            'National goal: ', `National goal`, '<br>',
                            'Regional goal: ', `Regional goal`))
      
      # Rima request for printable map
      # options = leafletOptions(zoomControl = FALSE))
      # in addProviderTiles(options = tileOptions(attribution = ""))
      
      leaflet(shp_APLMA_sub) %>% 
        addProviderTiles('Esri.WorldGrayCanvas', options = tileOptions(minZoom=3, maxZoom=5)) %>%
        addPolygons(fillOpacity=0.5, dashArray=1, weight=1.5, color=shp_APLMA_sub$color, popup=shp_APLMA_sub$popup) %>%
        addLegend('bottomleft', colors=sub_labels$colors, 
                  labels=sub_labels$scenarios, 
                  opacity=1, title=NULL)
    })
    
    # table
    output$table_elimination <- DT::renderDataTable({
      datatable(country_codes %>% 
                  filter(country %in% input$countries) %>%
                  transmute(Country=country, `Minimal scenario for elimination`=scenario_min_elim, Range, `National goal`, `Regional goal`), 
                rownames = FALSE, options=list(searching = TRUE, pageLength = 22))
    })
    
    # plot
    output$elim_timeline <- renderPlot({
      
      df <- elim %>% 
        mutate(elim_range=paste0('(', 
                                 ifelse(lag(elim_obs_year)==Inf, 'NO', lag(elim_obs_year)), ', ', elim_obs_year, ', ', 
                                 ifelse(lead(elim_obs_year)==Inf, 'NO', lead(elim_obs_year)), ')')) %>% # replace Inf by NO
        filter(estimate=='Median', scenario==input$scenario_unique, resistance==input$resistance_elim, MDA==input$mda_elim, ITN==input$itn_elim) %>% 
        mutate(pos=3*22:1, never=2032, lab_never='NO') %>%
        filter(country %in% input$countries)
      df$elim_obs_year[df$elim_obs_year == 'Inf'] <- NA
      df$elim_clinical_year[df$elim_clinical_year == 'Inf'] <- NA
      
      
      if(input$scenario_unique=='Reverse 3: Stop IRS, ITN, 50% treated'){return({
        plot(1, xaxt='n', yaxt='n', bty='n', pch='', ylab='', xlab='')
        text(1, 1, 'Reverse 3: Impossible to assess elimination because \n the scenario models a surveillance system collapse.', cex=2)
      })}
      
      if(input$scenario_unique=='Reverse 2: Stop IRS, ITN' & input$itn_elim == 'Yes'){return({
        plot(1, xaxt='n', yaxt='n', bty='n', pch='', ylab='', xlab='')
        text(1, 1, 'Reverse 2 shows the predicted impact of stopping ITN activities \n and scaling up ITN is not allowed for this scenario.', cex=2)
      })}
      
      
      
      if(input$elimination_type=='elim_obs_year'){
        
        return(
          ggplot(data=df, aes(x=elim_obs_year)) +
            geom_label(data=df, aes(x=elim_obs_year, y=pos, label=country), color='black', fill='grey', size=6, show.legend = FALSE) +
            geom_label(data=df, aes(x=elim_obs_year, y=pos-1.5, label=elim_range), color='black', fill='grey', alpha=0.4, size=4, show.legend = FALSE) +
            geom_label(data=df %>% filter(is.na(elim_obs_year)), aes(x=never, y=pos, label=country), color='black', fill='red', alpha=0.4, size=6, show.legend = FALSE) +
            geom_label(data=df %>% filter(is.na(elim_obs_year)), aes(x=never, y=pos-1.5, label=lab_never), color='black', fill='red', alpha=0.4, size=4, show.legend = FALSE) +
            scale_x_continuous(limits = c(2010, 2035), breaks=c(2015, 2020, 2025, 2030)) +
            geom_vline(aes(xintercept=2030), color='blue', lty=2) +
            scale_y_continuous(breaks=NULL) +
            theme_light() + theme(axis.text.y=element_blank(), axis.text.x = element_text(size=13), panel.border = element_blank()) +
            labs(x=NULL, y=NULL)
        )
      }
      
      if(input$elimination_type=='elim_clinical_year'){
        
        return(
          ggplot(data=df, aes(x=elim_clinical_year)) +
            geom_label(data=df, aes(x=elim_clinical_year, y=pos, label=country), color='black', fill='grey', size=6, show.legend = FALSE) +
            geom_label(data=df, aes(x=elim_clinical_year, y=pos-1.5, label=elim_clinical_year), color='black', fill='grey', alpha=0.4, size=4, show.legend = FALSE) +
            geom_label(data=df %>% filter(is.na(elim_clinical_year)), aes(x=never, y=pos, label=country), color='black', fill='red', alpha=0.4, size=6, show.legend = FALSE) +
            geom_label(data=df %>% filter(is.na(elim_clinical_year)), aes(x=never, y=pos-1.5, label=lab_never), color='black', fill='red', alpha=0.4, size=4, show.legend = FALSE) +
            scale_x_continuous(limits = c(2010, 2035), breaks=c(2015, 2020, 2025, 2030)) +
            geom_vline(aes(xintercept=2030), color='blue', lty=2) +
            scale_y_continuous(breaks=NULL) +
            theme_light() + theme(axis.text.y=element_blank(), axis.text.x = element_text(size=13), panel.border = element_blank()) +
            labs(x=NULL, y=NULL)
        )
      }
      
      
    })
    
    # link to the costing tab from the minimum elimination package
    observeEvent(input$link_to_tabpanel_costing, {
      updateTabItems(session, "menu", selected="model3")  # switch to cost "about"
      updateTabItems(session, "panel_costs", selected="mep_cost_tab") # switch to the "mep_cost_tab"
    })
    
    
    
    # Generate report (NOT USERD)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # output$report <- downloadHandler(
    #   # For PDF output, change this to "report.pdf"
    #   filename = "report.pdf",
    #   content = function(file) {
    #     # Copy the report file to a temporary directory before processing it, in
    #     # case we don't have write permissions to the current working dir (which
    #     # can happen when deployed).
    #     tempReport <- file.path(tempdir(), "report.Rmd")
    #     file.copy("./www/report.Rmd", tempReport, overwrite = TRUE)
    #     
    #     # Set up parameters to pass to Rmd document
    #     params <- list(scenario = input_scenario(), 
    #                    prediction=input$prediction, 
    #                    resistance=input$resistance,
    #                    mda=input$mda,
    #                    countries=input$countries,
    #                    scale=input$scale)
    #     
    #     # Knit the document, passing in the `params` list, and eval it in a
    #     # child of the global environment (this isolates the code in the document
    #     # from the code in this app).
    #     rmarkdown::render(tempReport, output_format='pdf_document', output_file = file,
    #                       params = params,
    #                       envir = new.env(parent = globalenv())
    #     )
    #   })
    
    
    # Model Phase 3: Costs
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    
    # dataframes for cost outputs
    df_costs <- reactive({
      df <- data_costs %>% filter(scenario==input$scenario_unique_cost,
                                  country %in% input$countries, 
                                  year >= input$range_cost[1],
                                  year <= input$range_cost[2],
                                  resistance==input$resistance_cost,
                                  MDA==input$mda_cost,
                                  ITN==input$itn_cost)
      
      return(left_join(df, country_codes %>% select(country_number, country),  by = c("country_number", "country")))
    })
    
    # summary with total cost for the whole area/region
    output$total_pop_risk <- renderText(
      c(
        '<h4>Total Population at Risk:</h4>',
        paste0('Total median cost for selected country(ies): ', dollar(round(sum(df_costs()$cost_med), -5)), "<br>"),
        paste0('[Min - Max]: [', dollar(round(sum(df_costs()$cost_min), -5)), ' - ', dollar(round(sum((df_costs()$cost_max), -5))), ']'),
        '<br><br>'
      ))
    
    # summary with target population at risk
    output$total_target_pop_risk <- renderText(c(
      '<h4>Targeted Population at Risk:</h4>',
      paste0('Total median cost for selected country(ies): ', dollar(round(sum(df_costs()$cost_med_focus), -5)), "<br>"),
      paste0('[Min - Max]: [', dollar(round(sum(df_costs()$cost_min_focus), -5)), ' - ', dollar(round(sum((df_costs()$cost_max_focus), -5))), ']'),
      '<br><br>'
    ))
    
    # upper limit of the y axis for the boxplot
    cost_max_country <- reactive(
      max((df_costs() %>% 
             group_by(country) %>%
             summarise(cost_max=sum(cost_max)))$cost_max)
    )
    
    # boxplot with total cost, Total Population at Risk
    output$boxplot_cost_total <- renderPlot(
      ggplot(df_costs() %>% 
               group_by(country) %>%
               summarise(cost_min=sum(cost_min), cost_med=sum(cost_med), cost_max=sum(cost_max)),
             aes(x=country, y=cost_med)) + 
        geom_point() +
        geom_errorbar(aes(ymin=cost_min, ymax=cost_max)) +
        scale_y_continuous(labels = scales::dollar, limits=c(0, cost_max_country())) +
        labs(title="Costs in USD", x=NULL, y=NULL) +
        theme_bw() + theme(axis.title=element_blank(), axis.text=element_text(size = 13), 
                           axis.text.x=element_text(angle=45, hjust=1), title=element_text(size = 14))
    )
    
    
    # boxplot with total cost, Targeted Population at Risk
    output$boxplot_cost_target <- renderPlot(
      ggplot(df_costs() %>% 
               group_by(country) %>%
               summarise(cost_min=sum(cost_min_focus), cost_med=sum(cost_med_focus), cost_max=sum(cost_max_focus)),
             aes(x=country, y=cost_med)) + 
        geom_point(col='red') +
        geom_errorbar(aes(ymin=cost_min, ymax=cost_max), col='red') +
        scale_y_continuous(labels = scales::dollar, limits=c(0, cost_max_country())) +
        labs(title="Costs in USD", x=NULL, y=NULL) +
        theme_bw() + theme(axis.title=element_blank(), axis.text=element_text(size = 13), 
                           axis.text.x=element_text(angle=45, hjust=1), title=element_text(size = 14))
    )
    
    
    # cost categories breakdown
    df_costs_comp <- reactive({
      data_costs_component %>% filter(scenario==input$scenario_unique_cost,
                                      country_name %in% input$countries,
                                      year >= input$range_cost[1]-2015,
                                      year <= input$range_cost[2]-2015,
                                      resistance==input$resistance_cost,
                                      MDA==input$mda_cost,
                                      ITN==input$itn_cost)
    })
    
    # for labels
    pct_costs_comp <- reactive({
      df_costs_comp() %>% 
        group_by(component_name_2) %>%
        summarise(cost=sum(cost)) %>%
        mutate(pct=round(100*cost/sum(cost), 2))
    })
    
    output$plot_cost_component <- renderPlot(
      ggplot(df_costs_comp() %>% 
               group_by(component_name_2) %>%
               summarise(cost=sum(cost)) %>%
               ungroup(),
             aes(x=component_name_2, y=cost)) + 
        geom_col() +
        geom_label(data=pct_costs_comp(), aes(x=component_name_2, y=0.98*cost, label=paste0(pct, "%")), size = 4, vjust=0, hjust=0.5) +
        scale_y_continuous(labels = scales::dollar) +
        labs(title="Cost Breakdown (USD)", subtitle="Total Population at Risk and Median Cost", x=NULL, y=NULL) +
        theme_bw() + theme(axis.title=element_blank(), axis.text=element_text(size = 13),
                           axis.text.x=element_text(angle=45, hjust=1), title=element_text(size = 14))
    )
    
    # Year by year cost
    output$plot_cost_year <- renderPlot(
      ggplot(df_costs() %>% 
               group_by(year) %>%
               summarise(cost_min=sum(cost_min), cost_med=sum(cost_med), cost_max=sum(cost_max),
                         cost_min_focus=sum(cost_min_focus), cost_med_focus=sum(cost_med_focus), cost_max_focus=sum(cost_max_focus)),
             aes(x=year)) + 
        geom_point(aes(y=cost_med, col="black")) +
        geom_line(aes(y=cost_med, col="black")) +
        geom_ribbon(aes(ymin=cost_min, ymax=cost_max, col="black", fill="black"), alpha=0.2) +
        
        geom_point(aes(y=cost_med_focus, col="red")) +
        geom_line(aes(y=cost_med_focus, col="red")) +
        geom_ribbon(aes(ymin=cost_min_focus, ymax=cost_max_focus, colour='red', fill='red'), alpha=0.2) +
        
        scale_y_continuous(labels = scales::dollar) +
        labs(title="Yearly Costs in USD", x=NULL, y=NULL) +
        scale_color_manual(name='leg', values=c('black', 'red'), labels=c('Total Population at Risk', 'Targeted Population at Risk')) +
        scale_fill_manual(name='leg', values=c('black', 'red'), labels=c('Total Population at Risk', 'Targeted Population at Risk')) +
        theme_bw() + theme(axis.title=element_blank(), axis.text=element_text(size = 13), 
                           axis.text.x=element_text(angle=45, hjust=1), title=element_text(size = 14),
                           legend.position='bottom',  legend.title=element_blank(),  legend.text=element_text(size=13))
    )
    
    # Minimum Elimination Package Only ------------------------------------------------------------------
    
    # dataframes for cost outputs
    df_costs_mep <- reactive({
      df <- data_costs %>% filter(country %in% input$countries, 
                                  year >= input$range_cost_2[1],
                                  year <= input$range_cost_2[2])
      
      return(left_join(df, country_codes %>% 
                         select(country_number, country, `Minimum Scenario`),  by = c("country_number", "country")) %>%
               filter(scenario_number==`Minimum Scenario`))
    })
    
    # Summary with total cost for the whole area/region
    output$total_pop_risk_mep <- renderText(c(
      '<h4>Total Population at Risk:</h4>',
      paste0('Total median cost for selected country(ies): ', dollar(round(sum(df_costs_mep()$cost_med), -5)), "<br>"),
      paste0('[Min - Max]: [', dollar(round(sum(df_costs_mep()$cost_min), -5)), ' - ', dollar(round(sum((df_costs_mep()$cost_max), -5))), ']'),
      '<br><br>'
    ))
    
    # Boxplot with total cost, Total Population at Risk
    output$boxplot_cost_total_mep <- renderPlot(
      ggplot(df_costs_mep() %>% 
               group_by(country) %>%
               summarise(cost_min=sum(cost_min), cost_med=sum(cost_med), cost_max=sum(cost_max)),
             aes(x=country, y=cost_med)) + 
        geom_point() +
        geom_errorbar(aes(ymin=cost_min, ymax=cost_max)) +
        scale_y_continuous(labels = scales::dollar) +
        labs(title="Costs in USD", x=NULL, y=NULL) +
        theme_bw() + theme(axis.title=element_blank(), axis.text=element_text(size = 13), 
                           axis.text.x=element_text(angle=45, hjust=1), title=element_text(size = 14))
    )
    
    # Cost categories breakdown
    # df_costs_comp_mep <- reactive({
    #   data_costs_component %>% filter(scenario_number==`Minimum Scenario`,
    #                                   country_name %in% input$countries,
    #                                   year >= input$range_cost_2[1]-2015,
    #                                   year <= input$range_cost_2[2]-2015,
    #                                   include==TRUE)
    # })
    
    # for labels
    # pct_costs_comp_mep <- reactive({
    #   df_costs_comp_mep() %>% 
    #     group_by(component_name_2) %>%
    #     summarise(cost=sum(cost)) %>%
    #     mutate(pct=round(100*cost/sum(cost), 2))
    # })
    
    # cost component
    # output$plot_cost_component_mep <- renderPlot(
    #   ggplot(df_costs_comp_mep() %>% 
    #            group_by(component_name_2) %>%
    #            summarise(cost=sum(cost)) %>%
    #            ungroup(),
    #          aes(x=component_name_2, y=cost)) + 
    #     geom_col() +
    #     geom_label(data=pct_costs_comp_mep(), aes(x=component_name_2, y=0.98*cost, label=paste0(pct, "%")), size = 4, vjust=0, hjust=0.5) +
    #     scale_y_continuous(labels = scales::dollar) +
    #     labs(title="Cost Breakdown (USD)", subtitle="Total Population at Risk and Median Cost", x=NULL, y=NULL) +
    #     theme_bw() + theme(axis.title=element_blank(), axis.text=element_text(size = 13), 
    #                        axis.text.x=element_text(angle=45, hjust=1), title=element_text(size = 14))
    # )
    
    # year by year cost
    output$plot_cost_year_mep <- renderPlot(
      ggplot(df_costs_mep() %>% 
               group_by(year) %>%
               summarise(cost_min=sum(cost_min), cost_med=sum(cost_med), cost_max=sum(cost_max)),
             aes(x=year)) + 
        geom_point(aes(y=cost_med)) +
        geom_line(aes(y=cost_med)) +
        geom_ribbon(aes(ymin=cost_min, ymax=cost_max), col="black", fill="black", alpha=0.2) +
        scale_y_continuous(labels = scales::dollar) +
        labs(title="Yearly Costs in USD", x=NULL, y=NULL) +
        theme_bw() + theme(axis.title=element_blank(), axis.text=element_text(size = 13), 
                           axis.text.x=element_text(angle=45, hjust=1), title=element_text(size = 14),
                           legend.position='bottom',  legend.title=element_blank(),  legend.text=element_text(size=13))
    )
    
  })