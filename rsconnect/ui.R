################################## UI ###################################################
#########################################################################################

# # navbar page with tabs
ui <- navbarPage(title       = 
                   strong(HTML(sprintf("<span style='font-size:24px; color: %s'>2020 Iraq Multicluster Needs Assessment</span>", white))), id="nav",
                 windowTitle = "Iraq MCNA 2020 Dashboard",
                 
                 tabPanel(strong("Overview"),
                          value = "panel1",
                          icon= icon("map-marker"),
                          div(class="outer",
                              
                              tags$head(
                                # custom CSS
                                includeCSS("styles.css")
                              ),
                              leafletOutput('mapOverview', width="100%", height="100%"),
                              
                              
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = F, top = 75, left = 20, right = "auto",
                                            width = 350,
                                            
                                            h4(HTML(sprintf("<span style='color: %s; border-bottom: 2px solid %s;'><strong>Methodology</span></strong>", reach_red, reach_red))),
                                            h6(HTML("At the time of data collection, the operating environment in Iraq was such that safety concerns related to COVID-19 as well as movement restrictions were only present in some districts whereas other districts were fully accessible and safety concerns related to a face-to-face data collection were considered low. Taking into consideration the variying operational and safety complexities across districts, the MCNA VIII was implemented through a nationwide household-level survey that was building on a 'hybrid' form of data collection through which face-to-face surveys were only administered in those districts where no safety concerns or movement restrictions were present at the time of data collection. In all other districts, household surveys were collected through a remote phone-based data collection. ")),
                                            h6(HTML("<strong>Face-to-face household surveys:</strong> A two-stage stratified cluster sampling approach (90% level of confidence and a 10% margin of error) was employed in all accessible districts where data was collected through face-to-face interviews. Based on the population figures from the IOM DTM Master List, sampling frames were developed for all districts with a minimum of 200 IDP or returnee househols and adjusted to align with OCHA-defined administrative boundaries. A cluster sample was drawn for each population group in each district and locations were selected with probability proportional to size. Within each location, a set of geo-points was randomly generated and provided to enumerators who would then interview an eligible household nearest to a given geo-point. In areas where multiple conflict-affected population groups are present, the precision of stratification-level findings will increase accordingly.")),
                                            h6(HTML("<strong>Remote phone-based surveys:</strong> For those districts where data collection through face-to-face interviews was inhibited by safety concerns and/or movement restrictions, a non-probability purposive quota sampling approach was employed. The minimum quotas that were established through this approach ensure that the collected data is indicative of the geographic location (district) (quota 1) and population groups (IDPs in-camp, IDPs out of camp and returnees) (quota 2). Wherever the minimum quota targets of 60 surveys per district and population group could not be reached with the available phone numbers, REACH combined the quota-based sampling with a snowball sampling approach. Through the snowball sampling, interviewees refered to other potential participants from the same quotas that can be contacted for the assessment. While most respondents for the quota sampling were found through previous REACH assessments that have employed randomized sampling methodologies, some phone numbers were also found through local networks of partner organisations. All respondents that are found through previous REACH assessments have given their consent to be contacted for potential follow-up assessments conducted by REACH."))),
                              
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = F, top = 75,  right = 20, left = "auto",
                                            h4(HTML("<span style='color: #EE5859'><strong>Coverage Map</strong></span>")),
                                            h5(HTML("<span style=' color: #58585A'><strong>2020 Iraq MCNA</strong></span>")),
                                            h6(HTML("<span style='color: #58585A'><strong>Data sources:</strong></span><br>Administrative boundaries: OCHA<br>MCNA coverage: REACH"))
                                            
                                            
                              ))
                 ),
                 
                 tabPanel(strong("Data explorer"),
                          value = "panel2",
                          icon= icon("map-marker"),
                          div(class="outer2",
                              
                              tags$head(
                                includeCSS("styles.css")
                              ),
                              
                              leafletOutput('map', width="100%", height="100%"),
                              
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = F, top = 75, right = "auto", left = 20,
                                            width = 350,
                                            
                                            h4(HTML(sprintf("<span style='color: %s'><strong>Data explorer - by district</span></strong>", reach_red))),
                                            selectInput("popgroup", "Population group:",
                                                        choices = c("Returnees"),
                                                        selected = "Returnees"),
                                            selectInput("sector", "Sector:",
                                                        choices = sort(unique(as.character(mcna_lookup$sector)), decreasing = FALSE),
                                                        selected = "Household Profile"),
                                            selectInput("indicator", "Indicator:",
                                                        choices = sort(mcna_lookup$indicator_desc[mcna_lookup$sector == "Household Profile"], decreasing = FALSE))
                                            
                              ),
                              
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = F, top = 75, left = "auto", right = 20,
                                            width = 250,
                                            
                                            p(htmlOutput("infobox")))
                              
                          )),
                 conditionalPanel("false", icon("crosshairs"))
)