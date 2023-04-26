# Thank you for browsing through here! If you don't understand parts of the code, do not hesitate to shoot me an e-mail!

library(bsplus) # tutorials
library(data.table) # faster data manipulations
library(dtplyr) # faster data manipulations, dplyr syntax with data.table speed
library(fGarch) # for rsnorm which is rnorm with skew (idea distribution)
library(shiny)
library(shinyjs) # hide/show, enable/disable, reset elements
library(tidyverse) # data manipulations
library(shinydashboard) # boxes
library(shinyWidgets) # dropdownbuttons in bias settings
library(spsComps) # popovers


#library(reactlog) # for logging reactivity while running - not needed in normal running mode

# tell shiny to log all reactivity
#reactlog_enable()



# initial/default values for the bias model coefficients (advantaged, disadvantaged; intercept, linear, squared, cubic)
coeff_mean_default <- c(0, 0, 0, 0, 0, 0, 0, 0) #mean of error term

coeff_sd_default <- c(5, 5, 0, 0, 0, 0, 0, 0) # sd of error term

coeff_skew_default <- c(1, 1, 0, 0, 0, 0, 0, 0) # skew of error term


# initial/default inputs
default_inputs <- data.frame(
  
  available_funding = 10000000,
  target_grand_volume = 500000,
  num_applications = 50,
  stages = 3,
  number_samples = 5,
  num_reviewers_per_application_1 = 2,
  num_reviewers_per_application_2 = 2,
  num_reviewers_per_application_3 = 2,
  mode_1 = "Competitive",
  mode_2 = "Competitive",
  mode_3 = "Competitive",
  cut_off_1 = 100,
  cut_off_2 = 100,
  cut_off_3 = 100,
  review_mode_1 = "mean",
  review_mode_2 = "mean",
  review_mode_3 = "mean",
  fixcosts_applications_1 = 50,
  fixcosts_applications_2 = 50,
  fixcosts_applications_3 = 50,
  hours_applicant_1 = 50,
  hours_applicant_2 = 50,
  hours_applicant_3 = 50,
  price_hour_applicant_1 = 50,
  price_hour_applicant_2 = 50,
  price_hour_applicant_3 = 50,
  num_applicants_per_application_1 = 2,
  num_applicants_per_application_2 = 2,
  num_applicants_per_application_3 = 2,
  fixcosts_review_1 = 50,
  fixcosts_review_2 = 50,
  fixcosts_review_3 = 50,
  hours_reviewer_1 = 20,
  hours_reviewer_2 = 20,
  hours_reviewer_3 = 20,
  price_hour_reviewer_1 = 50,
  price_hour_reviewer_2 = 50,
  price_hour_reviewer_3 = 50,
  name_adv = "Male",
  name_disadv = "Female",
  group_proportions = .4, 
  opportunity_factor = 0,
  sd_ideas = 15,
  skew_ideas = 1 
  
)

# tooltips/popups
affiliations_tooltip <- "1: Department of Psychiatry and Psychotherapy, University of Lübeck, Germany <br><br> 2: Department of Rheumatology and Clinical Immunology, University of Lübeck, Germany <br><br> 3: Open Science Initiative, University of Lübeck, Germany <br><br> 4: Diversity, Inclusion, and Chance Equity Commission, German Psychological Society <br><br> 5: Academy of Fine Arts, Munich, Germany <br><br> 6: Philosophy and Diversity, German Philosophical Society <br><br> 7: Behavioral Law & Economics, Max Planck Institute for Research on Collective Goods, Bonn, Germany <br><br> 8: Department of Psychology, Humboldt University, Berlin, Germany"
tutorial_basic_tooltip <- "This will open a tutorial to learn about the app's functionalities"
tutorial_bias_tooltip <- "This will open a tutorial to learn how to model accuracy and bias"
scenarios_tooltip <- "Click here to view example scenarios for different funding schemes"

reset_general_tooltip <- "Click here to reset all parameters to default"

funding_budget_tooltip <- "Total budget of the funding agency to be distributed"
target_tooltip <- "Money each successful application is supposed to receive"
number_applications_tooltip <- "Number of initial applications"
stages_tooltip <- "A stage is defined as step in the process where some of the applications get rejected. This can be any kind of review process or a lottery."
number_samples_tooltip <- "How many times the process should be simulated? Higher values yield more precise results, but take longer to compute."
box_review_parameters_tooltip <- "In this box you can change how the review process in each stage works. In a Competitive process or a Lottery, only the best X proceed. In a Normative process, all applications above a Cut-Off proceed. With the Review Mode setting you can choose which reviewer rating will be used to rank the applications (mean, min, max)"
idea_distr_plot_tooltip <- "The applications have different qualities, the distribution is shown here. This idea quality distribution is an artifical and arbitrary scale to rank the ideas/applicants by a proxy for scientific value. You can change the distribution with the parameters below. It is only the first sample shown, you can scroll through the others with the slider in the diversity results at the bottom"
box_groups_tooltip <- "Here you can split the scientific community into two groups of (un)equal proportion. In the diversity tab below, you can introduce biases favoring one of the groups."
group_proportions_tooltip <- "These are the proportions of the two groups within the pool of potential applicants."
opportunity_factor_tooltip <- "Proportion of people in the disadvantaged group not applying."
group_proportions_application_tooltip <- "Proportion of people ultimately applying."




mode_tooltip <- "How applications are selected:<br><br> <b>Competitive</b> (only the best advance, as indicated by the Number of Accepted Applications;<br><br> <b>Normative</b> (all applications better than \"Cut-Off\" advance);<br><br> <b>Lottery</b> (random choice, how many is indicated by the Number of Accepted Applications)"
review_mode_tooltip <- "How ratings of reviewers are used to determine ranking of applications:<br><br> <b>mean</b> (by average of reviewer ratings in current stage);<br><br> <b>min</b> (by minimum of reviewer ratings);<br><br> <b>max</b> (by maximum of reviewer ratings) - only makes a difference if the number of reviewers are > 1"

number_accepted_tooltip <- "How many applications are accepted in this stage and get either promoted to next stage or are finally accepted"
cut_off_tooltip <- "The Cut-Off for applications in a Normative setting. All applications below this value will be rejected"

av_accepted_tooltip <- "The number of applications who are finally accepted and receive funding. In case of a Normative setting, this value can vary from one sample to the next, which is why it is average in this case"
budget_needed_tooltip <- "Given the number of acceptances - How much funding is needed to fund all these applications given the Target Volume above? This should not exceed the Funding Budget"
total_costs_tooltip <- "This is the total cost of the process including fixed costs and costs for work hours for applicants and reviewers"
final_iq_tooltip <- "The average idea quality of the accepted applications"
final_div_tooltip <- "The average percentage of people from the disadvantaged group among the acceptances"
download_tooltip <- "Click the Download Button to download complete results data. You can add an optional tag in the text box which will appear in the results file - only numbers, characters, \"-\", and \"_\" will work."




fixcosts_applications_tooltip <- "Fixed Costs related to each application such as travel costs, office equipment, ..."
hours_applicant_tooltip <- "How many hours does each person work on the application on average?"
price_hour_applicant_tooltip <- "What is the average hourly salary for the people working on an application?"
num_applicants_per_application_tooltip <- "What is the average number of people working on an application?"
fixcosts_review_tooltip <- "Fixed Costs related to each application on the reviewer side such as travel costs, office equipment, training courses ... This scales by APPLICATION, not by REVIEWER"
hours_reviewer_tooltip <- "How many hours does each reviewer work on an application on average?"
price_hour_reviewer_tooltip <- "What is the average hourly salary for the reviewers?"
num_reviewers_per_application_tooltip <- "What is the average number of reviewers for each application?"

plot_hours_applicants_reviewer_tooltip <- "How many work hours are spent on applications in each stage? TOP: On each application; BOTTOM: On all applications; LEFT: Applicant work hours; RIGHT: Reviewer work hours."
plot_costs_applicants_reviewer_tooltip <- "How many monetary units are spent on applications in each stage? TOP: On each application; BOTTOM: On all applications; LEFT: Applicant work hours; RIGHT: Reviewer work hours."
plot_total_costs_tooltip <- "Cost of  whole process, split by stage, based on fix costs, work hours, salaries, and number of people involved (applicants and reviewer). Horizontal line indicates funding budget set in first box. If it is lower than the colored bars, the process costs more money than it ultimately provides."
plot_win_lose_cost_tooltip <- "Cost of whole process, split by win/lose status; Gold indicates costs of grant applications which end up winning, blue reflect costs of applications which get rejected."

bias_1_tooltip <- "These are predefined biases you can select.<br><br> <b>Constant Bias</b> will lead to an underestimation of ideas across the range of idea qualities for the disadvantaged group.<br><br> <b>Brilliance Bias</b> will tend to underestimate ideas from the disadvantaged group the better the ideas are. Additionnally, reviewers are overall less precise compared to the Constant Bias.<br><br> <b>Error Bias</b> will be more and more imprecise with higher idea qualities - additionally, the imprecision is overall greater in the disadvantaged group.<br><br> <b>Combined Bias</b> will underestimate ideas from the disadvantaged group and this bias increases with higher idea qualities. The imprecision of reviewers increases with higher idea qualities but increases stronger for the disadvantaged group. Reviewers overall tend to underestimate bad ideas and overestimate good ideas."
bias_2_tooltip <- bias_1_tooltip
bias_3_tooltip <- bias_1_tooltip

quick_options_mean_1_tooltip <- "Here you can customize the bias in the mean of error with a few button clicks"
quick_options_sd_1_tooltip <- "Here you can customize the bias in the sd of error with a few button clicks"
quick_options_mean_2_tooltip <- quick_options_mean_1_tooltip
quick_options_sd_2_tooltip <- quick_options_sd_1_tooltip
quick_options_mean_3_tooltip <- quick_options_mean_1_tooltip
quick_options_sd_3_tooltip <- quick_options_sd_1_tooltip

advanced_settings_1_tooltip <- "Here you can customize the bias even more precise using customizable curve fitting"
advanced_settings_2_tooltip <-advanced_settings_1_tooltip
advanced_settings_3_tooltip <- advanced_settings_1_tooltip


group_draw_tooltip <- "Click the group which bias you want to model by adding points to the plot"


plot_stage_1_tooltip <- "Distribution of reviewer error across levels of idea quality for each group. Line indicates the mean, colored area indicates first and third quantile."
plot_stage_2_tooltip <- "Distribution of reviewer error across levels of idea quality for each group. Line indicates the mean, colored area indicates first and third quantile."
plot_stage_3_tooltip <- "Distribution of reviewer error across levels of idea quality for each group. Line indicates the mean, colored area indicates first and third quantile."

plot_prediction_stage_1_tooltip <- "Scatter of true idea quality (x) against rated idea quality (y). Points close to the line indicate a precise and unbiased process. Large spread indicates imprecision, systematic deviation below or above the line indicate biases estimates (too low and high, respectively."
plot_prediction_stage_2_tooltip <- "Scatter of true idea quality (x) against rated idea quality (y). Points close to the line indicate a precise and unbiased process. Large spread indicates imprecision, systematic deviation below or above the line indicate biases estimates (too low and high, respectively."
plot_prediction_stage_3_tooltip <- "Scatter of true idea quality (x) against rated idea quality (y). Points close to the line indicate a precise and unbiased process. Large spread indicates imprecision, systematic deviation below or above the line indicate biases estimates (too low and high, respectively."


scatter_quality_by_stage_tooltip <- "This plots the individual applications still in the process for each stage against their quality by group membership. The solid line indicates the initial mean of idea qualities fot this sample which can slightly deviate from 100 due to sampling error."
mean_quality_by_stage_tooltip <- "This plots the mean of ideas for each stage with individual samples in grey, the grand mean in red and the mean for the sample you selected with the slider below in black. Error bars indicate standard deviation."
group_percentage_by_stage_tooltip <- "This plots the group proportions for each stage for the sample you selected using the slider below."
group_percentage_by_stage_all_tooltip <- "This plots the average group proportions for each stage. "
group_proportion_by_stage_tooltip <- "This plots the group proportions for each stage for the sample you selected using the slider below (left) and the average across all samples (right)."
sample_picker_tooltip <- "With this slider you can choose which of the individual samples that were simulated are displayed in the indidivual plots."

box_save_tooltip <- "Here you can save your parameter settings to compare them to different sets. You can also load recent settings to adapt them. Note that it is the settings that are saved, not the results - due to random sampling, results may differ. Closing the app will delete these settings."

plot_cost_comparison_tooltip <- "This plot will show the costs of all saved processes next to each other, split by stage"
plot_iq_comparison_tooltip <- "This plot will show the final idea qualities of all saved processes next to each other"
plot_div_comparison_tooltip <- "This plot will show the final group proportions of all saved processes next to each other"




# plot colors:
colors_stage <- c("#7C0B2B", "#E3655B", "#F2D492") 
colors_winning <- c("#0083ad", "#bf8314")
colors_groups <- c("#FBB13C", "#1F96AD") 

# function which generates titles for UI elements of applicant and reviewer costs per stage
generateTitleElement <- function(title){
  
  fluidRow( 
    textOutput(title))
  tags$b(title)
  
}

#function which generates the UI elements which control the applicant and reviewer costs per stage
generateUIElementCosts <- function(width, offset, style, IDs, titles, value, step = NA, min = NA, max = NA){
  
  fluidRow(
    column(width, offset = 0, style=style[1], div(numericInput(IDs[1],
                                                               titles[1],
                                                               step = step[1],
                                                               min = min[1],
                                                               max = max[1],
                                                               value = value[1])),class="not_bold"),
    column(width, offset = 0, style=style[2], div(numericInput(IDs[2],
                                                               titles[2],
                                                               step = step[2],
                                                               min = min[2],
                                                               max = max[2],
                                                               value = value[2])),class="not_bold"),
    column(width, offset = 0, style=style[3], div(numericInput(IDs[3],
                                                               titles[3],                                                               
                                                               step = step[3],
                                                               min = min[3],
                                                               max = max[3],
                                                               value = value[3])),class="not_bold"))
}


box_width <- '100%' # width of title boxes
nSlider <- 9 # number of slider elements which is important to set their colors

# margins for UI elements which control applicants and reviewer costs per stage
styles <- c('padding-left:14px;padding-right:2px','padding-right:4px;padding-left:4px', 'padding-right:14px;padding-left:0px')







###### UI ######  
ui <- fluidPage(
  
  useShinyjs(),
  shinyWidgets::useShinydashboard(),
  
  #slider colors
  setSliderColor(c(colors_groups[1], "#004b5a", colors_stage[1], colors_stage[1], colors_stage[2], colors_stage[2], colors_stage[3], colors_stage[3], "#004b5a"), sliderId = seq(1, nSlider)),
  
  # this is to display the "stage 1, 2, 3 headings in the cost section in non bold font
  tags$head(tags$style(HTML(".not_bold label {font-weight:normal;}"))),
  
  # this changes built-in colors for boxes to custom colors
  tags$style(
    type = 'text/css', 
    '.bg-light-blue {background-color: #004b5a!important; }'
  ),
  tags$style(
    type = 'text/css', 
    '.bg-red {background-color: #e42032!important; }'
  ),
  tags$style(
    type = 'text/css', 
    '.bg-orange {background-color: #ec7404!important; }'
  ),
  tags$style(
    type = 'text/css', 
    '.bg-yellow {background-color: #fabb00!important; }'
  ),
  tags$style(
    type = 'text/css', 
    '.bg-teal {background-color: #007E98!important; }'
  ),
  tags$style(
    type = 'text/css', 
    '.bg-olive {background-color: #ffffff!important; }'
  ),
  
  # this changes the text in the output boxes below the general settings box to white and a bigger font
  tags$head(tags$style("#acc_appl{color: white;
                                 font-size: 50px;
                                 font-style: bold;
                                 margin-top: -20px;
                       margin-left: 20px}"
  )
  ),
  
  tags$head(tags$style("#funding_spent{color: white;
                                 font-size: 50px;
                                 font-style: bold;
                                 margin-top: -20px;
                       margin-left: 1px}"
  )
  ),
  
  tags$head(tags$style("#total_costs{color: white;
                                 font-size: 50px;
                                 font-style: bold;
                                 margin-top: -20px;
                       margin-left: 1px}"
  )
  ),
  tags$head(tags$style("#resulting_iq{color: white;
                                 font-size: 50px;
                                 font-style: bold;
                                 margin-top: -20px;
                       margin-left: 20px}"
  )
  ),
  
  tags$head(tags$style("#fin_group_proportion{color: white;
                                 font-size: 50px;
                                 font-style: bold;
                                 margin-top: -20px;
                       margin-left: 20px}"
  )
  ),
  
  # styles for tabpanels - greater size
  
  tags$head(tags$style(HTML('.nav-tabs>li>a {
  font-size: 32px;
}'
  ))),

# styles for bias stage boxes. Overwrites the primary, danger and warning box properties/colors


tags$style(HTML(paste0("
                    .box.box-solid.box-primary>.box-header {
      color:#ffffff;
      background:", colors_stage[1], ";
      background-color:", colors_stage[1], "
      border:", colors_stage[1], "}",
      
      ".box.box-solid.box-primary{
          border-bottom-color:", colors_stage[1], ";
              border-left-color:", colors_stage[1], ";
              border-right-color:", colors_stage[1], ";
              border-top-color:", colors_stage[1], ";
      }
      "
))),

tags$style(HTML(paste0("
                    .box.box-solid.box-danger>.box-header {
      color:#ffffff;
      background:", colors_stage[2], ";
      background-color:", colors_stage[2]," 
      border:", colors_stage[2], "}",
      
      ".box.box-solid.box-danger{
          border-bottom-color:", colors_stage[2], ";
              border-left-color:", colors_stage[2], ";
              border-right-color:", colors_stage[2], ";
              border-top-color:", colors_stage[2], ";
      }
      
      
      "
))),

tags$style(HTML(paste0("
                    .box.box-solid.box-warning>.box-header {
      color:#ffffff;
      background:", colors_stage[3], ";
      background-color:", colors_stage[3], "
    border:", colors_stage[3], "}",
    
    ".box.box-solid.box-warning{
         border-bottom-color:", colors_stage[3], ";
         border-left-color:", colors_stage[3], ";
         border-right-color:", colors_stage[3], ";
         border-top-color:", colors_stage[3], ";
      }
      
      
      "
    
))),

# this changes button colors for the example funding scenarios and the predefined biases
tags$style(
  ".btn-success {border-color: black; background-color: white; color: #004b5a; }",
  ".btn-success.active {border-color: white; background-color: #004b5a; color: white; }",
  ".btn-success:hover {border-color: white; background-color: #004b5a; color: white; }",
  ".btn-success:focus {border-color: white; background-color: #004b5a; color: white; }",
  ".btn-success:active.focus {border-color: white; background-color: #004b5a; color: white; }",
  ".btn-success.active.focus {border-color: white; background-color: #004b5a; color: white; }",
  ".btn-success:active.hover {border-color: white; background-color: #004b5a; color: white; }",
),

tags$style(
  ".btn-primary {border-color: white; background-color: #004b5a; color: white; }",
  ".btn-primary.active {border-color: #004b5a; background-color: white; color: #004b5a; }",
  ".btn-primary:hover {border-color: #004b5a; background-color: white; color: #004b5a; }",
  ".btn-primary:focus {border-color: #004b5a; background-color: white; color: #004b5a; }",
  ".btn-primary:active.focus {border-color: #004b5a; background-color: white; color: #004b5a; }",
  ".btn-primary.active.focus {border-color: #004b5a; background-color: white; color: #004b5a; }",
  ".btn-primary:active.hover {border-color: #004b5a; background-color: white; color: #004b5a; }",
),

# when the update and download buttons get disabled - display in grey
tags$style(".btn.disabled {
    background-color: grey;
    }"),

# popover styles:
tags$head(tags$style(HTML('
.popover-title{
  color: #FFF;
    font-size: 16px;
  background-color: #004b5a;
}

.popover-header{ 
  background: #004b5a; 
} 

.popover-content{ 
  background: #FFF;
  color: #000000;
}
'))),



###### ACTUAL UI ######



titlePanel(
  fluidRow(
    column(9,
           div("GrantInq", style = 'font-size:70px; font-weight:bold'), 
    ),
    column(3,
           img(height = 80, src = "OSI_SNL_logo_white.png", align = "right"),
    ),
    
  ),
  windowTitle = "GrantInq"
),


# authors

div( helpText(style = 'font-size:18px; color: #000000', tags$span(
  HTML(paste0("Finn Luebber", tags$sup("1,2,3"), ",  
  Sören Krach",  tags$sup("1,3,4"), ", 
  Marina Martinez Mateo",  tags$sup("5,6"), ", 
  Frieder M. Paulus",  tags$sup("1,3"), ", 
  Lena Rademacher",  tags$sup("1,3"), ",   
  Rima-Maria Rahal",  tags$sup("7"), ", 
  Jule Specht",  tags$sup("8")  
              )), 
  tags$i(
  icon("fas fa-info-circle"))))) %>% 
  bsPopover("Affiliations",
            affiliations_tooltip,
            "bottom",
            "hover",
            titlecolor = "white",
            titlesize = "16px",
            contentsize = "14px",
            html = T),


###### General Settings ######
fluidRow(
  
  # general settings of the process: how much money is available, how many applications there are initially, 
  # how many stages the process has, etc
  
  div(column( 12,  
              
              div(shinydashboard::box(id = "box_general", # the big box including all general settings
                                      column(2,
                                             
                                             div(helpText(style = 'font-size:18px; color: #dee2e6;', "
                                                  With this app you can calculate how much money people invest in writing grant applications and how well
                                                  the process works in terms of funding top applications and being non-biased. \n
                                                  You can hover over elements to open tooltips with more information."
                                                  
                                             )),
                                             
                                             # this is the initial tutorial
                                             div(style = "color:#000000", bs_modal(
                                               id = "app_help", 
                                               title = "",
                                               body = bs_carousel(id = "help_instructions", use_indicators = F) %>%
                                                 bs_set_data(interval = FALSE) %>% 
                                                 
                                                 bs_append(
                                                   content = bs_carousel_image(src = "Folie1_basic.PNG")#,
                                                 ) %>%
                                                 bs_append(
                                                   content = bs_carousel_image(src = "Folie2_basic.PNG")#,
                                                 ) %>%  
                                                 bs_append(
                                                   content = bs_carousel_image(src = "Folie3_basic.PNG")#,
                                                 ) %>%  
                                                 bs_append(
                                                   content = bs_carousel_image(src = "Folie4_basic.PNG")#,
                                                 ) %>%  
                                                 bs_append(
                                                   content = bs_carousel_image(src = "Folie5_basic.PNG")#,
                                                 ) %>%  
                                                 bs_append(
                                                   content = bs_carousel_image(src = "Folie6_basic.PNG")#,
                                                 ) %>%  
                                                 bs_append(
                                                   content = bs_carousel_image(src = "Folie7_basic.PNG")#,
                                                 ) %>%  
                                                 bs_append(
                                                   content = bs_carousel_image(src = "Folie8_basic.PNG")#,
                                                 ) %>%  
                                                 bs_append(
                                                   content = bs_carousel_image(src = "Folie9_Tutorial_Div_fmp.gif")#,
                                                 ),
                                               size = "large"
                                             )),
                                             
                                             
                                             
                                             bs_button("Quick Guide Through App", button_type = "warning", button_size = "large") %>%
                                               bs_attach_modal("app_help") %>% 
                                               bsPopover("Tutorial",
                                                         tutorial_basic_tooltip,
                                                         "right",
                                                         "hover",
                                                         titlecolor = "white",
                                                         titlesize = "16px",
                                                         contentsize = "14px"),
                                             # / tutorial
                                             
                                             div( helpText(style = 'font-size:18px; font-weight:bold; color: #FFF', tags$span("Examplary Funding Scenarios", tags$i(
                                               icon("fas fa-info-circle"))))) %>% 
                                               bsPopover("Examplary Funding Scenarios",
                                                         scenarios_tooltip,
                                                         "right",
                                                         "hover",
                                                         titlecolor = "white",
                                                         titlesize = "16px",
                                                         contentsize = "14px"),
                                             
                                             
                                             
                                             radioGroupButtons("scenarios", label = NULL, choiceValues = c(1,2,3,4), choiceNames = c("One-stage Scenario (e.g., R01)", "Two-stage Scenario (e.g., ERC)", "Tiebreak Lottery (e.g., SNSF)", "Pre-lottery"), selected = character(0), direction = "vertical", size = "normal", justified = T, status = "success"),
                                             actionButton("reset_general", "Reset to default", style="font-weight: bold; color: #FFF; background-color: #e74c3c; border-color: #000000"),
                                             
                                             
                                      ),
                                      column(4, 
                                             column(5,
                                                    # column with funding budget etc
                                                    autonumericInput("available_funding", "Funding Budget", value = default_inputs$available_funding, align = "left", decimalCharacter = ".", digitGroupSeparator = ",", decimalPlaces = 0),
                                                    autonumericInput("target_grand_volume", label = "How much money should each winner receive?", value = default_inputs$target_grand_volume, align = "left", decimalCharacter = ".", digitGroupSeparator = ",", decimalPlaces = 0),
                                                    numericInput("num_applications", label = "How many applications are there?", value = default_inputs$num_applications),
                                                    numericInput("stages", label = tags$span("How many stages are there where applications can be rejected?", tags$i(
                                                      icon("fas fa-info-circle"))),
                                                      value = default_inputs$stages, min = 1, max = 3) %>% 
                                                      bsPopover("Number of Stages",
                                                                stages_tooltip,
                                                                "right",
                                                                "hover",
                                                                titlecolor = "white",
                                                                titlesize = "16px",
                                                                contentsize = "14px"), 
                                                    
                                                    bsPopover(
                                                      numericInput("number_samples", label = tags$span("How many samples do you want to simulate?", tags$i(
                                                        icon("fas fa-info-circle"))),
                                                        value =  default_inputs$number_samples, min = 1, step = 10),
                                                      "Number of samples",
                                                      number_samples_tooltip,
                                                      "right",
                                                      "hover",
                                                      titlecolor = "white",
                                                      titlesize = "16px",
                                                      contentsize = "14px"),
                                                    
                                             ),
                                             
                                             column(7,
                                                    shinydashboard::box(id = "box_idea_qualities",
                                                                        
                                                                        plotOutput("idea_distr_plot", height = "200px"),
                                                                        # how idea quality is distributed:
                                                                        generateTitleElement("Parameters of Idea Distribution"),
                                                                        generateUIElementCosts(width = 4, offset = 0, style = c('padding-left:14px;padding-right:2px','padding-right:4px;padding-left:4px', 'padding-right:14px;padding-left:0px'), 
                                                                                               IDs = c("mean_ideas", "sd_ideas", "skew_ideas"), 
                                                                                               titles = c("Mean", "SD", "skew"), 
                                                                                               value = c(100, default_inputs$sd_ideas, default_inputs$skew_ideas),
                                                                                               step = c(1, 1, .1),
                                                                                               min = c(50, 1, .1)),
                                                                        title = tags$span("Distribution of Initial Idea Qualities", style = 'color:black;', tags$i(
                                                                          icon("fas fa-info-circle"))) %>% 
                                                                          bsPopover("Idea Quality Distribution",
                                                                                    idea_distr_plot_tooltip,
                                                                                    "right",
                                                                                    "hover",
                                                                                    titlecolor = "white",
                                                                                    titlesize = "16px",
                                                                                    contentsize = "14px"), 
                                                                        background = "olive", width = box_width, style='margin-top:-10px; color:black;', 
                                                    ),
                                             ),
                                             
                                      ),
                                      
                                      column(2, 
                                             shinydashboard::box(id = "box_groups",
                                                                 div( style = 'padding-top:8px'), column(6, 
                                                                                                         div(style = paste0('color:', colors_groups[1]),textInput( "name_disadv", "Disadvantaged", default_inputs$name_disadv)) %>% 
                                                                                                           bsPopover("You can enter group names here",
                                                                                                                     "Enter the Name of the Disadvantaged Group",
                                                                                                                     "left",
                                                                                                                     "hover",
                                                                                                                     titlecolor = "white",
                                                                                                                     titlesize = "16px",
                                                                                                                     contentsize = "14px"),
                                                                 ),
                                                                 column(6,
                                                                        div(style = paste0('color:', colors_groups[2]), textInput("name_adv", "Advantaged", default_inputs$name_adv)) %>% 
                                                                          bsPopover("You can enter group names here",
                                                                                    "Enter the Name of the Advantaged Group",
                                                                                    "right",
                                                                                    "hover",
                                                                                    titlecolor = "white",
                                                                                    titlesize = "16px",
                                                                                    contentsize = "14px"),
                                                                 ),
                                                                 sliderInput("group_proportions",
                                                                             tags$span("Population Proportion of Disadv.", tags$i(
                                                                               icon("fas fa-info-circle"))),
                                                                             min = 0,
                                                                             max = 1,
                                                                             value = default_inputs$group_proportions) %>% 
                                                                   bsPopover("Proportion of (Dis)advantaged People in Population",
                                                                             group_proportions_tooltip,
                                                                             "right",
                                                                             "hover",
                                                                             titlecolor = "white",
                                                                             titlesize = "16px",
                                                                             contentsize = "14px"),
                                                                 sliderInput("opportunity_factor", tags$span("Entry Bias", tags$i(
                                                                   icon("fas fa-info-circle"))), 
                                                                   value =  default_inputs$opportunity_factor, min = 0, max = 1, step = .01) %>% 
                                                                   bsPopover("Entry Bias",
                                                                             opportunity_factor_tooltip,
                                                                             "right",
                                                                             "hover",
                                                                             titlecolor = "white",
                                                                             titlesize = "16px",
                                                                             contentsize = "14px"), 
                                                                 div(helpText(style='font-weight: bold; color: #000000; font-size: 14px;', tags$span("Initial Proportions", tags$i(
                                                                   icon("fas fa-info-circle"))))) %>% 
                                                                   bsPopover("Proportion within Applicants",
                                                                             group_proportions_application_tooltip,
                                                                             "right",
                                                                             "hover",
                                                                             titlecolor = "white",
                                                                             titlesize = "16px",
                                                                             contentsize = "14px"), 
                                                                 
                                                                 plotOutput("group_proportions_application", height = 80),
                                                                 title = span("Group Proportions", style = 'color:black;', tags$i(
                                                                   icon("fas fa-info-circle"))) %>% 
                                                                   bsPopover("Group proportions",
                                                                             box_groups_tooltip,
                                                                             "right",
                                                                             "hover",
                                                                             titlecolor = "white",
                                                                             titlesize = "16px",
                                                                             contentsize = "14px"),
                                                                 background = "olive", width = box_width, style='margin-top:-10px; color:black;'),
                                             
                                      ),
                                      
                                      column(4,
                                             # review parameters of individual stages of the process
                                             shinydashboard::box(id = "box_review_parameters",
                                                                 
                                                                 div(column(style='margin-top:-10px; color:black;', 4, 
                                                                            div(helpText(style = paste0('font-weight:bold; font-size:15px; color:', colors_stage[1] ), "Stage 1")),
                                                                            numericInput("num_reviewers_per_application_1", tags$span("Number Reviewers", tags$i(
                                                                              icon("fas fa-info-circle"))), 
                                                                              value = default_inputs$num_reviewers_per_application_1, min = 0) %>% 
                                                                              bsPopover("Number of Reviewers",
                                                                                        num_reviewers_per_application_tooltip,
                                                                                        "right",
                                                                                        "hover",
                                                                                        titlecolor = "white",
                                                                                        titlesize = "16px",
                                                                                        contentsize = "14px"),
                                                                            selectInput("mode_1", tags$span("Competition Mode", tags$i(
                                                                              icon("fas fa-info-circle"))), 
                                                                              choices = c("Competitive", "Normative", "Lottery"), selected = default_inputs$mode_1) %>% 
                                                                              bsPopover("Mode of Selection Procedure",
                                                                                        mode_tooltip,
                                                                                        "right",
                                                                                        "hover",
                                                                                        titlecolor = "white",
                                                                                        titlesize = "16px",
                                                                                        contentsize = "14px",
                                                                                        html = T),
                                                                            selectInput("review_mode_1", tags$span("Review Mode", tags$i(
                                                                              icon("fas fa-info-circle"))),
                                                                              choices = c("mean", "min", "max"), selected = default_inputs$review_mode_1) %>% 
                                                                              bsPopover("Review Mode",
                                                                                        review_mode_tooltip,
                                                                                        "right",
                                                                                        "hover",
                                                                                        titlecolor = "white",
                                                                                        titlesize = "16px",
                                                                                        contentsize = "14px",
                                                                                        html = T),
                                                                            sliderInput("num_accepted_1", tags$span("Number Accepted Applications", tags$i(
                                                                              icon("fas fa-info-circle"))), 
                                                                              value = default_inputs$num_applications/2, min = 1, max = default_inputs$num_applications) %>% 
                                                                              bsPopover("Number of Accepted Applications",
                                                                                        number_accepted_tooltip,
                                                                                        "right",
                                                                                        "hover",
                                                                                        titlecolor = "white",
                                                                                        titlesize = "16px",
                                                                                        contentsize = "14px"),
                                                                            sliderInput("cut_off_1", tags$span("Cut Off", tags$i(
                                                                              icon("fas fa-info-circle"))),
                                                                              value = default_inputs$cut_off_1, min = 50, max = 150) %>% 
                                                                              bsPopover("Cut Off for Normative",
                                                                                        cut_off_tooltip,
                                                                                        "right",
                                                                                        "hover",
                                                                                        titlecolor = "white",
                                                                                        titlesize = "16px",
                                                                                        contentsize = "14px"),
                                                                            
                                                                 )),
                                                                 div(column(style='margin-top:-10px; color:black;', 4, 
                                                                            div(helpText(style = paste0('font-weight:bold; font-size:15px; color:', colors_stage[2] ), "Stage 2")),
                                                                            numericInput("num_reviewers_per_application_2", "Number Reviewers", value = default_inputs$num_reviewers_per_application_2, min = 0),
                                                                            selectInput("mode_2", "Competition Mode", choices = c("Competitive", "Normative", "Lottery"), selected = default_inputs$mode_2),
                                                                            selectInput("review_mode_2", "Review Mode", choices = c("mean", "min", "max"), selected = default_inputs$review_mode_2),
                                                                            sliderInput("num_accepted_2", "Number Accepted Applications", value = round(default_inputs$num_applications/4, 0), min = 1, max = round(default_inputs$num_applications/2, 0)),
                                                                            sliderInput("cut_off_2", "Cut Off", value = default_inputs$cut_off_2, min = 50, max = 150),
                                                                 )),
                                                                 div(column(style='margin-top:-10px; color:black;', 4, 
                                                                            div(helpText(style = paste0('font-weight:bold; font-size:15px; color:', colors_stage[3] ), "Stage 3")),
                                                                            numericInput("num_reviewers_per_application_3", "Number Reviewers", value = default_inputs$num_reviewers_per_application_3, min = 0),
                                                                            selectInput("mode_3", "Competition Mode", choices = c("Competitive", "Normative", "Lottery"), selected = default_inputs$mode_3),
                                                                            selectInput("review_mode_3", "Review Mode", choices = c("mean", "min", "max"), selected = default_inputs$review_mode_3),
                                                                            sliderInput("num_accepted_3", "Number Accepted Applications", value = round(default_inputs$num_applications/8, 0), min = 1, max = round(default_inputs$num_applications/4, 0)),
                                                                            sliderInput("cut_off_3", "Cut Off", value = default_inputs$cut_off_3, min = 50, max = 150),
                                                                            
                                                                 )), 
                                                                 title = span(tags$span("Review Parameters", style = 'color:black;', tags$i(
                                                                   icon("fas fa-info-circle")))) %>% 
                                                                   bsPopover("Parameters for Review Procedure",
                                                                             box_review_parameters_tooltip,
                                                                             "right",
                                                                             "hover",
                                                                             titlecolor = "white",
                                                                             titlesize = "16px",
                                                                             contentsize = "14px"), 
                                                                 background = "olive", width = box_width),
                                      ),
                                      
                                      title = "General settings", background = "light-blue", width = box_width)), #, height = 785
              
  )
  )
),

# row with output/result boxes and download data
fluidRow(
  column(2,
         div(shinydashboard::box(
           textOutput("acc_appl"),  
           title = tags$span("(Average) Number of Accepted", tags$i(
             icon("fas fa-info-circle"))) %>% 
             bsPopover("Accepted Applications",
                       av_accepted_tooltip,
                       "right",
                       "hover",
                       titlecolor = "white",
                       titlesize = "16px",
                       contentsize = "14px"), 
           background = "teal", width = box_width)
         ),
  ),
  column(2,
         shinydashboard::box(
           column(2,
                  img(src="Finance.png", height = 40) 
           ),
           column(10,
                  textOutput("funding_spent"), 
           ),
           title = tags$span("Budget Needed for Scenario", tags$i(
             icon("fas fa-info-circle"))) %>% 
             bsPopover("Funding Budget Needed",
                       budget_needed_tooltip,
                       "right",
                       "hover",
                       titlecolor = "white",
                       titlesize = "16px",
                       contentsize = "14px"), 
           background = "teal", width = box_width,
           
         ),
  ),
  column(2,
         div(shinydashboard::box(
           
           column(2,
                  img(src="Finance.png", height = 40) 
           ),
           column(10,
                  textOutput("total_costs"), 
           ),
           title = tags$span("Total Costs", tags$i(
             icon("fas fa-info-circle"))) %>% 
             bsPopover("Total Cost of the Process",
                       total_costs_tooltip,
                       "right",
                       "hover",
                       titlecolor = "white",
                       titlesize = "16px",
                       contentsize = "14px"), 
           background = "teal", width = box_width
         )
         )
  ),
  column(2,
         shinydashboard::box(
           column(2,
                  img(src="quality.png", height = 40) 
           ),
           column(10,
                  textOutput("resulting_iq"),
           ),
           title = tags$span("Average Idea Quality", tags$i(
             icon("fas fa-info-circle"))) %>% 
             bsPopover("Idea Quality Accepted Applications",
                       final_iq_tooltip,
                       "right",
                       "hover",
                       titlecolor = "white",
                       titlesize = "16px",
                       contentsize = "14px"), 
           background = "teal", width = box_width
         ),
  ),
  
  column(2,
         div(shinydashboard::box(
           column(2,
                  img(src="diversity.png", height = 40) 
           ),
           column(10,
                  textOutput("fin_group_proportion"),  
           ),
           title = tags$span("Average % of Disadv. Winners", tags$i(
             icon("fas fa-info-circle"))) %>% 
             bsPopover("Group Proportion of Disadvantaged Among Accepted",
                       final_div_tooltip,
                       "right",
                       "hover",
                       titlecolor = "white",
                       titlesize = "16px",
                       contentsize = "14px"), 
           background = "teal", width = box_width)
         ),
  ),
  
  column(2,
         div(shinydashboard::box(
           textInput("text_to_save", "File prefix"),
           textOutput("download_error"),
           downloadButton("downloadData", "Download Data"),
           title = tags$span("Download Raw Data", tags$i(
             icon("fas fa-info-circle"))) %>% 
             bsPopover("Download",
                       download_tooltip,
                       "right",
                       "hover",
                       titlecolor = "white",
                       titlesize = "16px",
                       contentsize = "14px"), 
           background = "teal", width = box_width))
  ),
  
  
),

# Tabs with diversity & quality, costs, save & compare
tabsetPanel(id = "part_of_simulation",
            
            ###### Diversity & Quality ######
            tabPanel("Diversity & Quality",
                     
                     
                     # second part of the simulation, introducing idea of idea/grant quality and bias in decisions
                     fluidRow(
                       column(12,
                              shinydashboard::box(title = "Diversity & Quality - Bias Settings", background = "black", width = box_width)
                       )
                     ),
                     
                     # explanation text for second part
                     fluidRow(
                       
                       
                       column(12,
                              div(style = 'font-size:18px', helpText(
                                "Each application has a quality determined by the distribution in the top box.
                                                                  Each reviewer rates the quality of the applications but makes errors in the process,
                                                                  so that Rating = True Quality + Error. In this part, you can model the form of the error distribution and its
                                                                  dependencies with the true idea quality, thereby modeling varying degrees of precision and biases of the review process.
                                Results are at the bottom of the page"
                              )),
                              
                              # bias tutorial
                              div(style = "color:#000000", bs_modal(
                                id = "app_bias_help", 
                                title = "",
                                body = bs_carousel(id = "bias_help_instructions", use_indicators = F) %>%
                                  bs_set_data(interval = FALSE) %>% 
                                  
                                  bs_append(
                                    content = bs_carousel_image(src = "Folie1.PNG")#,
                                  ) %>%
                                  bs_append(
                                    content = bs_carousel_image(src = "Folie2.PNG")#,
                                  ) %>%  
                                  bs_append(
                                    content = bs_carousel_image(src = "Folie3.PNG")#,
                                  ) %>%  
                                  bs_append(
                                    content = bs_carousel_image(src = "Folie4.PNG")#,
                                  ) %>%  
                                  bs_append(
                                    content = bs_carousel_image(src = "Folie5.PNG")#,
                                  ) %>%  
                                  bs_append(
                                    content = bs_carousel_image(src = "Folie6.PNG")#,
                                  ) %>%  
                                  bs_append(
                                    content = bs_carousel_image(src = "Folie7.PNG")#,
                                  ) %>%  
                                  bs_append(
                                    content = bs_carousel_image(src = "Folie8.PNG")#,
                                  ) %>%  
                                  bs_append(
                                    content = bs_carousel_image(src = "Folie9.PNG")#,
                                  ) %>%  
                                  bs_append(
                                    content = bs_carousel_image(src = "Folie10.PNG")#,
                                  ) %>%  
                                  bs_append(
                                    content = bs_carousel_image(src = "Folie11.PNG")#,
                                  ) %>%  
                                  bs_append(
                                    content = bs_carousel_image(src = "Folie12.PNG")#,
                                  ) %>%  
                                  bs_append(
                                    content = bs_carousel_image(src = "Folie13.PNG")#,
                                  ),
                                size = "large"
                              )),
                              bs_button("Quick Guide Through Bias Settings", button_type = "warning", button_size = "large") %>%
                                bs_attach_modal("app_bias_help") %>% 
                                bsPopover("Bias Tutorial",
                                          tutorial_bias_tooltip,
                                          "right",
                                          "hover",
                                          titlecolor = "white",
                                          titlesize = "16px",
                                          contentsize = "14px"),
                              
                       ),
                       
                     ),
                     
                     box(id = "box_stage_1",
                         fluidRow(
                           column(7,
                                  div(style = 'font-size: 18px;', helpText("Here, you can model the bias for stage 1. You can either choose one predefined bias by clicking one of the four buttons, select from the options right next
                                      to the predefined biases, or create your own using the advanced settings below. The implications of your choices can be seen on the right.")),
                                  column(2,
                                         
                                         radioGroupButtons("bias_1", tags$span("Predefined Biases", tags$i(
                                           icon("fas fa-info-circle")))  %>%
                                             bsPopover("Predefined Biases",
                                                       bias_1_tooltip,
                                                       "right",
                                                       "hover",
                                                       titlecolor = "white",
                                                       titlesize = "16px",
                                                       contentsize = "14px",
                                                       html=T), 
                                           choiceValues = c(1,2,3,4), choiceNames = c("Constant Bias", "Brilliance Bias", "Error Bias", "Combined Bias"), selected = character(0), status = "primary", justified = TRUE, direction = "vertical"), 
                                  ),
                                  column(4,
                                         tags$style(".btn-custom {background-color: #FFF; color: #004b5a; border-color: #004b5a; height:60px; font-size:14px; font-style:bold;}"),
                                         dropdownButton(
                                           tags$h3("General"),
                                           radioButtons("sign_mean_assoc_1", "Sign of Association Idea Quality - Mean of Error", choices = c("Positive", "Negative"), selected = "Positive"),
                                           radioButtons("mean_assoc_1", label = "General Degree of the Association Idea Quality - Mean of Error", choiceNames = c("no", "small", "medium", "large"), choiceValues = c(1, 2, 3, 4),selected = 1), #, "quadratic light", "quadratic medium", "quadratic large")),
                                           
                                           tags$h3("Biases"),
                                           radioButtons("mean_magn_bias_1", label = "Bias: Underestimation of Ideas", choiceNames = c("no", "small", "medium", "large"), choiceValues = c(1, 2, 3, 4),selected = 1),
                                           radioButtons("mean_assoc_bias_1", label = "Bias in the Degree of Association of Idea Quality with Mean of Error", choiceNames = c("no", "small", "medium", "large"), choiceValues = c(1, 2, 3, 4),selected = 1),
                                           
                                           circle = F,
                                           status = "custom",
                                           icon = icon("fas fa-chevron-circle-down"),
                                           label = tags$span(HTML("Change options regarding <br/>mean of reviewer error"), tags$i(
                                             icon("fas fa-info-circle")))) %>% 
                                           bsPopover("Quick Options Mean Bias",
                                                     quick_options_mean_1_tooltip,
                                                     "top",
                                                     "hover",
                                                     titlecolor = "white",
                                                     titlesize = "16px",
                                                     contentsize = "14px"),
                                         
                                  ),
                                  
                                  column(4,
                                         
                                         dropdownButton(
                                           tags$h3("General"),
                                           radioButtons("sign_sd_assoc_1", "Sign of Association Idea Quality - SD of Error", choices = c("Positive", "Negative"), selected = "Positive"),
                                           radioButtons("sd_magn_1", label = "Magnitude of Error Spread", choiceNames = c("small", "medium", "large", "very large"), choiceValues = c(1, 2, 3, 4),selected = 1),
                                           radioButtons("sd_assoc_1", label = "General Degree of the Association Idea Quality - SD of error", choiceNames = c("no", "small", "medium", "large"), choiceValues = c(1, 2, 3, 4),selected = 1),
                                           
                                           tags$h3("Biases"),
                                           radioButtons("sd_magn_bias_1", label = "Bias: Greater Error Spread", choiceNames = c("no", "small", "medium", "large"), choiceValues = c(1, 2, 3, 4),selected = 1),
                                           radioButtons("sd_assoc_bias_1", label = "Bias in the Degree of Association of Idea Quality with Error Spread", choiceNames = c("no", "small", "medium", "large"), choiceValues = c(1, 2, 3, 4),selected = 1),
                                           
                                           circle = F,
                                           status = "custom", 
                                           icon = icon("fas fa-chevron-circle-down"),
                                           label = tags$span(HTML("Change options regarding <br/>SD of reviewer error"), tags$i(
                                             icon("fas fa-info-circle")))) %>% 
                                           bsPopover("Quick Options SD Bias",
                                                     quick_options_sd_1_tooltip,
                                                     "top",
                                                     "hover",
                                                     titlecolor = "white",
                                                     titlesize = "16px",
                                                     contentsize = "14px"),
                                         
                                  ),
                                  column(2,
                                         actionButton("reset_bias_1", div(style='margin-left:-4px;', "Reset \n Bias"),  style="font-weight: bold; color: #FFF; background-color: #e74c3c; border-color: #6a040f", width = box_width),
                                  ),
                           ),
                           column(5,
                                  column(6,
                                         plotOutput("plot_stage_1", height = 300),
                                         
                                  ),
                                  column(6,
                                         plotOutput("plot_prediction_stage_1", height = 300),
                                  ),
                           ),
                         ),
                         
                         fluidRow(
                           column(7,
                                  div(box(
                                    div(style = 'font-size: 18px;', helpText("Here, you can model your own bias. Select which group you want to model 
                                                                                 and then click somewhere in the plots to add points. Double-click on a point deletes it. A best fit line will automatically
                                                                                 be added. For higher-order terms than just the intercept, click the \"Terms for best fit\"-boxes above the plots.
                                                                                 You can also modify the best fit line with the inputs below the plots.
                                                                                 When you choose values which imply impossible values for SD or skew, the absolute value is used,
                                                                                 leading to kinks in the curves.")),
                                    
                                    column(4,
                                           checkboxGroupInput("model_terms_mean_1", "Terms for best fit mean of error distr.", c("linear", "quadratic", "cubic"), inline = T, selected = "linear"),
                                           plotOutput("plotMean1", click = "plot_click_mean_1", dblclick = "plot_dbl_click_mean_1", height = 250),
                                           
                                    ),
                                    column(4,
                                           checkboxGroupInput("model_terms_sd_1", "Terms for best fit SD of error distr.", c("linear", "quadratic", "cubic"), inline = T, selected = "linear"),
                                           plotOutput("plotsd1", click = "plot_click_sd_1", dblclick = "plot_dbl_click_sd_1", height = 250),
                                           
                                    ),
                                    column(4,
                                           checkboxGroupInput("model_terms_skew_1", "Terms for best fit skew of error distr.", c("linear", "quadratic", "cubic"), inline = T),
                                           plotOutput("plotSkew1", click = "plot_click_skew_1", dblclick = "plot_dbl_click_skew_1", height = 250),
                                           
                                    ),
                                    
                                    icon(name = "group_draw_1", class = "fas fa-info-circle", style = "font-size:20px;") %>% 
                                      bsPopover("Which group do you want to model?",
                                                group_draw_tooltip,
                                                "right",
                                                "hover",
                                                titlecolor = "white",
                                                titlesize = "16px",
                                                contentsize = "14px"),
                                    radioButtons("group_to_draw_1", label = "Which group do you want to model for stage 1?", choiceNames =  c("Adv.", "Disadv."), choiceValues = c(1,0), selected = 1, inline = T),
                                    
                                    
                                    solidHeader = T, collapsible = T,
                                    title = tags$span("Advanced settings for Stage 1", tags$i(
                                      icon("fas fa-info-circle"))) %>% 
                                      bsPopover("Advanced Settings for Bias Modeling",
                                                advanced_settings_1_tooltip,
                                                "right",
                                                "hover",
                                                titlecolor = "white",
                                                titlesize = "16px",
                                                contentsize = "14px"),
                                    status = "primary", width = box_width, style = 'color:black;', collapsed=T)),
                           ),
                           column(5,
                                  column(1),
                                  column(1,
                                         icon(name = "plot_stage_1_info", class = "fas fa-info-circle", style = "font-size:30px;")%>% 
                                           bsPopover("Visualisation of Bias by Idea Quality",
                                                     plot_stage_1_tooltip,
                                                     "right",
                                                     "hover",
                                                     titlecolor = "white",
                                                     titlesize = "16px",
                                                     contentsize = "14px")
                                  ),
                                  
                                  column(4),
                                  column(1,
                                  ),
                                  column(1,
                                         icon(name = "plot_prediction_stage_1_info", class = "fas fa-info-circle", style = "font-size:30px;")%>% 
                                           bsPopover("True Idea Quality against Observed Idea Quality",
                                                     plot_prediction_stage_1_tooltip,
                                                     "right",
                                                     "hover",
                                                     titlecolor = "white",
                                                     titlesize = "16px",
                                                     contentsize = "14px")
                                  ),
                                  column(4,
                                         actionButton("update_plot_prediction_1", "Update Plots", class = "btn btn-danger", width = box_width),
                                  ),
                           ),
                         ), solidHeader = T, title = "Stage 1", status = "primary", width = box_width, style = 'color:black;'
                         
                     ),
                     
                     
                     box(id = "box_stage_2",
                         fluidRow(
                           column(7,
                                  div(style = 'font-size: 18px;', helpText("Here, you can model the bias for stage 2. You can either choose one predefined bias by clicking one of the four buttons, select from the options right next
                                      to the predefined biases, or create your own using the advanced settings below. The implications of your choices can be seen on the right.")),
                                  column(2,
                                         radioGroupButtons("bias_2", tags$span("Predefined Biases", tags$i(
                                           icon("fas fa-info-circle"))) %>% 
                                             bsPopover("Predefined Biases",
                                                       bias_2_tooltip,
                                                       "right",
                                                       "hover",
                                                       titlecolor = "white",
                                                       titlesize = "16px",
                                                       contentsize = "14px",
                                                       html=T),
                                           choiceValues = c(1,2,3,4), choiceNames = c("Constant Bias", "Brilliance Bias", "Error Bias", "Combined Bias"), selected = character(0), status = "primary", justified = TRUE, direction = "vertical"),
                                         
                                  ),
                                  column(4,
                                         dropdownButton(
                                           tags$h3("General"),
                                           radioButtons("sign_mean_assoc_2", "Sign of Association Idea Quality - Mean of Error", choices = c("Positive", "Negative"), selected = "Positive"),
                                           radioButtons("mean_assoc_2", label = "General Degree of the Association Idea Quality - Mean of Error", choiceNames = c("no", "small", "medium", "large"), choiceValues = c(1, 2, 3, 4),selected = 1), #, "quadratic light", "quadratic medium", "quadratic large")),
                                           
                                           tags$h3("Biases"),
                                           radioButtons("mean_magn_bias_2", label = "Bias: Underestimation of Ideas", choiceNames = c("no", "small", "medium", "large"), choiceValues = c(1, 2, 3, 4),selected = 1),
                                           radioButtons("mean_assoc_bias_2", label = "Bias in the Degree of Association of Idea Quality with Mean of Error", choiceNames = c("no", "small", "medium", "large"), choiceValues = c(1, 2, 3, 4),selected = 1),
                                           
                                           circle = F,
                                           status = "custom",
                                           icon = icon("fas fa-chevron-circle-down"),
                                           label = tags$span(HTML("Change options regarding <br/>mean of reviewer error"), tags$i(
                                             icon("fas fa-info-circle")))) %>% 
                                           bsPopover("Quick Options Mean Bias",
                                                     quick_options_mean_2_tooltip,
                                                     "top",
                                                     "hover",
                                                     titlecolor = "white",
                                                     titlesize = "16px",
                                                     contentsize = "14px"),
                                         
                                  ),
                                  
                                  column(4,
                                         
                                         dropdownButton(
                                           tags$h3("General"),
                                           radioButtons("sign_sd_assoc_2", "Sign of Association Idea Quality - SD of Error", choices = c("Positive", "Negative"), selected = "Positive"),
                                           radioButtons("sd_magn_2", label = "Magnitude of Error Spread", choiceNames = c("small", "medium", "large", "very large"), choiceValues = c(1, 2, 3, 4),selected = 1),
                                           radioButtons("sd_assoc_2", label = "General Degree of the Association Idea Quality - SD of error", choiceNames = c("no", "small", "medium", "large"), choiceValues = c(1, 2, 3, 4),selected = 1),
                                           
                                           tags$h3("Biases"),
                                           radioButtons("sd_magn_bias_2", label = "Bias: Greater Error Spread", choiceNames = c("no", "small", "medium", "large"), choiceValues = c(1, 2, 3, 4),selected = 1),
                                           radioButtons("sd_assoc_bias_2", label = "Bias in the Degree of Association of Idea Quality with Error Spread", choiceNames = c("no", "small", "medium", "large"), choiceValues = c(1, 2, 3, 4),selected = 1),
                                           
                                           circle = F,
                                           status = "custom", 
                                           icon = icon("fas fa-chevron-circle-down"),
                                           label = tags$span(HTML("Change options regarding <br/>SD of reviewer error"), tags$i(
                                             icon("fas fa-info-circle")))) %>% 
                                           bsPopover("Quick Options SD Bias",
                                                     quick_options_sd_2_tooltip,
                                                     "top",
                                                     "hover",
                                                     titlecolor = "white",
                                                     titlesize = "16px",
                                                     contentsize = "14px"),
                                         
                                  ),
                                  column(2,
                                         actionButton("reset_bias_2", div(style='margin-left:-4px;', "Reset \n Bias"),  style="font-weight: bold; color: #FFF; background-color: #e74c3c; border-color: #6a040f", width = box_width),
                                  ),
                                  
                           ),
                           
                           column(5,
                                  column(6,
                                         plotOutput("plot_stage_2", height = 300),
                                         
                                  ),
                                  column(6,
                                         plotOutput("plot_prediction_stage_2", height = 300),
                                  ),
                           ),
                         ),
                         
                         fluidRow(
                           column(7,
                                  div(box(id = "box_stage_2_advanced", 
                                          div(style = 'font-size: 16px;', helpText("Here, you can model your own bias. Select which group you want to model 
                                                                                 and then click somewhere in the plots to add points. Double-click on a point deletes it. A best fit line will automatically
                                                                                 be added. For higher-order terms than just the intercept, click the \"Terms for best fit\"-boxes above the plots.
                                                                                 You can also modify the best fit line with the inputs below the plots. When you choose values which imply impossible values for SD or skew, the absolute value is used,
                                                                                 leading to kinks in the curves.")),
                                          
                                          
                                          column(4,
                                                 checkboxGroupInput("model_terms_mean_2", "Terms for best fit mean of error distr.", c("linear", "quadratic", "cubic"), inline = T, selected = "linear"),
                                                 plotOutput("plotMean2", click = "plot_click_mean_2", dblclick = "plot_dbl_click_mean_2", height = 250),
                                                 
                                          ),
                                          column(4,
                                                 checkboxGroupInput("model_terms_sd_2", "Terms for best fit SD of error distr.", c("linear", "quadratic", "cubic"), inline = T, selected = "linear"),
                                                 plotOutput("plotsd2", click = "plot_click_sd_2", dblclick = "plot_dbl_click_sd_2", height = 250),
                                                 
                                          ),
                                          column(4,
                                                 checkboxGroupInput("model_terms_skew_2", "Terms for best fit skew of error distr.", c("linear", "quadratic", "cubic"), inline = T),
                                                 plotOutput("plotSkew2", click = "plot_click_skew_2", dblclick = "plot_dbl_click_skew_2", height = 250),
                                                 
                                          ),
                                          icon(name = "group_draw_2", class = "fas fa-info-circle", style = "font-size:20px;") %>% 
                                            bsPopover("Which group do you want to model?",
                                                      group_draw_tooltip,
                                                      "right",
                                                      "hover",
                                                      titlecolor = "white",
                                                      titlesize = "16px",
                                                      contentsize = "14px"),
                                          
                                          radioButtons("group_to_draw_2", label = "Which group do you want to model for stage 2?", choiceNames =  c("Adv.", "Disadv."), inline = T, choiceValues = c(1,0), selected = 1),
                                          
                                          solidHeader = T, collapsible = T,
                                          title = tags$span("Advanced settings for Stage 2", tags$i(
                                            icon("fas fa-info-circle"))) %>% 
                                            bsPopover("Advanced Settings for Bias Modeling",
                                                      advanced_settings_2_tooltip,
                                                      "right",
                                                      "hover",
                                                      titlecolor = "white",
                                                      titlesize = "16px",
                                                      contentsize = "14px"),
                                          status = "danger", width = box_width, style = 'color:black;', collapsed = T)),
                           ),
                           column(5,
                                  column(1),
                                  column(1,
                                         icon(name = "plot_stage_2_info", class = "fas fa-info-circle", style = "font-size:30px;")%>% 
                                           bsPopover("Visualisation of Bias by Idea Quality",
                                                     plot_stage_2_tooltip,
                                                     "right",
                                                     "hover",
                                                     titlecolor = "white",
                                                     titlesize = "16px",
                                                     contentsize = "14px")
                                  ),
                                  
                                  column(4),
                                  column(1,
                                  ),
                                  column(1,
                                         icon(name = "plot_prediction_stage_2_info", class = "fas fa-info-circle", style = "font-size:30px;")%>% 
                                           bsPopover("True Idea Quality against Observed Idea Quality",
                                                     plot_prediction_stage_2_tooltip,
                                                     "right",
                                                     "hover",
                                                     titlecolor = "white",
                                                     titlesize = "16px",
                                                     contentsize = "14px")
                                  ),
                                  column(4,
                                         actionButton("update_plot_prediction_2", "Update Plots", class = "btn btn-danger", width = box_width),
                                  ),
                           ),
                         ), solidHeader = T, title = "Stage 2", status = "danger", width = box_width, style = 'color:black;'
                     ),
                     
                     box(id = "box_stage_3",
                         fluidRow(
                           column(7,
                                  div(style = 'font-size: 16px;', helpText("Here, you can model the bias for stage 3. You can either choose one predefined bias by clicking one of the four buttons, select from the options right next
                                      to the predefined biases, or create your own using the advanced settings below. The implications of your choices can be seen on the right.")),
                                  column(2,
                                         radioGroupButtons("bias_3", tags$span("Predefined Biases", tags$i(
                                           icon("fas fa-info-circle"))) %>% 
                                             bsPopover("Predefined Biases",
                                                       bias_3_tooltip,
                                                       "right",
                                                       "hover",
                                                       titlecolor = "white",
                                                       titlesize = "16px",
                                                       contentsize = "14px",
                                                       html=T),
                                           choiceValues = c(1,2,3,4), choiceNames = c("Constant Bias", "Brilliance Bias", "Error Bias", "Combined Bias"), selected = character(0), status = "primary", justified = TRUE, direction = "vertical") 
                                         
                                  ),
                                  column(4,
                                         dropdownButton(
                                           tags$h3("General"),
                                           radioButtons("sign_mean_assoc_3", "Sign of Association Idea Quality - Mean of Error", choices = c("Positive", "Negative"), selected = "Positive"),
                                           radioButtons("mean_assoc_3", label = "General Degree of the Association Idea Quality - Mean of Error", choiceNames = c("no", "small", "medium", "large"), choiceValues = c(1, 2, 3, 4),selected = 1), #, "quadratic light", "quadratic medium", "quadratic large")),
                                           
                                           tags$h3("Biases"),
                                           radioButtons("mean_magn_bias_3", label = "Bias: Underestimation of Ideas", choiceNames = c("no", "small", "medium", "large"), choiceValues = c(1, 2, 3, 4),selected = 1),
                                           radioButtons("mean_assoc_bias_3", label = "Bias in the Degree of Association of Idea Quality with Mean of Error", choiceNames = c("no", "small", "medium", "large"), choiceValues = c(1, 2, 3, 4),selected = 1),
                                           
                                           circle = F,
                                           status = "custom",
                                           icon = icon("fas fa-chevron-circle-down"),
                                           label = tags$span(HTML("Change options regarding <br/>mean of reviewer error"), tags$i(
                                             icon("fas fa-info-circle")))) %>% 
                                           bsPopover("Quick Options Mean Bias",
                                                     quick_options_mean_3_tooltip,
                                                     "top",
                                                     "hover",
                                                     titlecolor = "white",
                                                     titlesize = "16px",
                                                     contentsize = "14px"),
                                  ),
                                  
                                  column(4,
                                         
                                         dropdownButton(
                                           tags$h3("General"),
                                           radioButtons("sign_sd_assoc_3", "Sign of Association Idea Quality - SD of Error", choices = c("Positive", "Negative"), selected = "Positive"),
                                           radioButtons("sd_magn_3", label = "Magnitude of Error Spread", choiceNames = c("small", "medium", "large", "very large"), choiceValues = c(1, 2, 3, 4),selected = 1),
                                           radioButtons("sd_assoc_3", label = "General Degree of the Association Idea Quality - SD of error", choiceNames = c("no", "small", "medium", "large"), choiceValues = c(1, 2, 3, 4),selected = 1),
                                           
                                           tags$h3("Biases"),
                                           radioButtons("sd_magn_bias_3", label = "Bias: Greater Error Spread", choiceNames = c("no", "small", "medium", "large"), choiceValues = c(1, 2, 3, 4),selected = 1),
                                           radioButtons("sd_assoc_bias_3", label = "Bias in the Degree of Association of Idea Quality with Error Spread", choiceNames = c("no", "small", "medium", "large"), choiceValues = c(1, 2, 3, 4),selected = 1),
                                           
                                           circle = F,
                                           status = "custom", 
                                           icon = icon("fas fa-chevron-circle-down"),
                                           label = tags$span(HTML("Change options regarding <br/>SD of reviewer error"), tags$i(
                                             icon("fas fa-info-circle")))) %>% 
                                           bsPopover("Quick Options SD Bias",
                                                     quick_options_sd_3_tooltip,
                                                     "top",
                                                     "hover",
                                                     titlecolor = "white",
                                                     titlesize = "16px",
                                                     contentsize = "14px"),
                                         
                                  ),
                                  column(2,
                                         actionButton("reset_bias_3", div(style='margin-left:-4px;', "Reset \n Bias"),  style="font-weight: bold; color: #FFF; background-color: #e74c3c; border-color: #6a040f", width = box_width),
                                  ),
                                  
                           ),
                           column(5,
                                  column(6,
                                         plotOutput("plot_stage_3", height = 300),
                                         
                                  ),
                                  column(6,
                                         plotOutput("plot_prediction_stage_3", height = 300),
                                  ),
                           ),
                         ),
                         fluidRow(
                           column(7,
                                  div(box(id = "box_stage_3_advanced", 
                                          div(style = 'font-size: 16px;', helpText("Here, you can model your own bias. Select which group you want to model 
                                                                                 and then click somewhere in the plots to add points. Double-click on a point deletes it. A best fit line will automatically
                                                                                 be added. For higher-order terms than just the intercept, click the \"Terms for best fit\"-boxes above the plots.
                                                                                 You can also modify the best fit line with the inputs below the plots. When you choose values which imply impossible values for SD or skew, the absolute value is used,
                                                                                 leading to kinks in the curves.")),
                                          
                                          
                                          
                                          column(4,
                                                 checkboxGroupInput("model_terms_mean_3", "Terms for best fit mean of error distr.", c("linear", "quadratic", "cubic"), inline = T, selected = "linear"),
                                                 plotOutput("plotMean3", click = "plot_click_mean_3", dblclick = "plot_dbl_click_mean_3", height = 250),
                                                 
                                          ),
                                          column(4,
                                                 checkboxGroupInput("model_terms_sd_3", "Terms for best fit SD of error distr.", c("linear", "quadratic", "cubic"), inline = T, selected = "linear"),
                                                 plotOutput("plotsd3", click = "plot_click_sd_3", dblclick = "plot_dbl_click_sd_3", height = 250),
                                                 
                                          ),
                                          column(4,
                                                 checkboxGroupInput("model_terms_skew_3", "Terms for best fit skew of error distr.", c("linear", "quadratic", "cubic"), inline = T),
                                                 plotOutput("plotSkew3", click = "plot_click_skew_3", dblclick = "plot_dbl_click_skew_3", height = 250),
                                                 
                                          ),
                                          icon(name = "group_draw_3", class = "fas fa-info-circle", style = "font-size:20px;") %>% 
                                            bsPopover("Which group do you want to model?",
                                                      group_draw_tooltip,
                                                      "right",
                                                      "hover",
                                                      titlecolor = "white",
                                                      titlesize = "16px",
                                                      contentsize = "14px"),
                                          
                                          radioButtons("group_to_draw_3", label = "Which group do you want to model for stage 3?", choiceNames =  c("Adv.", "Disadv."), choiceValues = c(1,0), inline = TRUE, selected = 1),
                                          
                                          solidHeader = T, collapsible = T,
                                          title = tags$span("Advanced settings for Stage 3", tags$i(
                                            icon("fas fa-info-circle"))) %>% 
                                            bsPopover("Advanced Settings for Bias Modeling",
                                                      advanced_settings_3_tooltip,
                                                      "right",
                                                      "hover",
                                                      titlecolor = "white",
                                                      titlesize = "16px",
                                                      contentsize = "14px"),
                                          status = "warning", width = box_width, style = 'color:black;', collapsed=T)),
                           ),
                           column(5,
                                  column(1),
                                  column(1,
                                         icon(name = "plot_stage_3_info", class = "fas fa-info-circle", style = "font-size:30px;")%>% 
                                           bsPopover("Visualisation of Bias by Idea Quality",
                                                     plot_stage_3_tooltip,
                                                     "right",
                                                     "hover",
                                                     titlecolor = "white",
                                                     titlesize = "16px",
                                                     contentsize = "14px")
                                  ),
                                  
                                  column(4),
                                  column(1,
                                  ),
                                  column(1,
                                         icon(name = "plot_prediction_stage_3_info", class = "fas fa-info-circle", style = "font-size:30px;")%>% 
                                           bsPopover("True Idea Quality against Observed Idea Quality",
                                                     plot_prediction_stage_3_tooltip,
                                                     "right",
                                                     "hover",
                                                     titlecolor = "white",
                                                     titlesize = "16px",
                                                     contentsize = "14px")
                                  ),
                                  column(4,
                                         actionButton("update_plot_prediction_3", "Update Plots", class = "btn btn-danger", width = box_width),
                                  ),
                           ),
                         ), solidHeader = T, title = "Stage 3", status = "warning", width = box_width, style = 'color:black;'
                     ),
                     
                     
                     
                     
                     ###### Div and Qual - OUTPUTS ######
                     fluidRow(
                       column(12,
                              shinydashboard::box(title = "Diversity & Quality - Results", background = "black", width = box_width)
                       )
                     ),
                     # titles in boxes
                     fluidRow(
                       column(4,
                              shinydashboard::box(title = tags$span("Idea Quality and Application Status by Stage", tags$i(
                                icon("fas fa-info-circle"))) %>% 
                                  bsPopover("Quality by Stage - Scatter",
                                            scatter_quality_by_stage_tooltip,
                                            "right",
                                            "hover",
                                            titlecolor = "white",
                                            titlesize = "16px",
                                            contentsize = "14px"),
                                background = "teal", width = box_width),
                       ),
                       
                       column(4,
                              shinydashboard::box(title = tags$span("Mean Idea Qualities by Stage", tags$i(
                                icon("fas fa-info-circle"))) %>% 
                                  bsPopover("Quality by Stage - Mean",
                                            mean_quality_by_stage_tooltip,
                                            "right",
                                            "hover",
                                            titlecolor = "white",
                                            titlesize = "16px",
                                            contentsize = "14px"),
                                background = "teal", width = box_width),
                       ),
                       
                       column(4,
                              shinydashboard::box(title = tags$span("Group Proportions by Stage", tags$i(
                                icon("fas fa-info-circle"))) %>% 
                                  bsPopover("Group Proportions by Stage",
                                            group_proportion_by_stage_tooltip,
                                            "right",
                                            "hover",
                                            titlecolor = "white",
                                            titlesize = "16px",
                                            contentsize = "14px"),
                                background = "teal", width = box_width)
                       ),
                       
                     ),
                     
                     fluidRow(
                       
                       column(4,
                              actionButton("update_results_plots", "This button uses the parameters above and updates the plots below", class = "btn btn-danger") #div( style = 'margin-top:25px; padding-bottom:20px; padding-left:20px', )     #, width = 1700, height = 100
                       ),
                       
                     ),
                     
                     
                     fluidRow(
                       
                       column(4,
                              plotOutput("scatter_quality_by_stage"),
                              
                       ),
                       
                       column(4,
                              plotOutput("mean_quality_by_stage"),
                       ),
                       
                       column(4,
                              
                              column(6,
                                     plotOutput("group_percentage_by_stage"),
                              ),
                              column(6,
                                     plotOutput("group_percentage_by_stage_all"),
                              )
                       ),
                       
                     ),
                     
                     fluidRow(
                       column(12,
                              sliderInput("sample_picker", tags$span("Number of Simulated Draw to Display", tags$i(
                                icon("fas fa-info-circle"))) %>% 
                                  bsPopover("Choose a Sample to Display",
                                            sample_picker_tooltip,
                                            "top",
                                            "hover",
                                            titlecolor = "white",
                                            titlesize = "16px",
                                            contentsize = "14px"),
                                min = 1, max = default_inputs$number_samples, value = 1, step = 1),
                       )
                     ),
                     fluidRow(
                       column(1),
                       column(10,
                              
                       ),
                       column(1),
                     ),
                     
            ),
            ###### Costs ######
            tabPanel("Costs",
                     
                     fluidRow(
                       column(12,
                              div(style = 'font-size:18px', helpText(
                                "This part of the simulation is concerned with the costs of the whole grant application process, namely costs for writing and reviewing individual applications."
                              )),
                       )
                     ),
                     
                     fluidRow(
                       
                       # Elements which control the applicant costs of the process
                       
                       column(2, 
                              
                              shinydashboard::box(title = "Applicant Costs", background = "light-blue", width = box_width),
                              
                              generateTitleElement(title = tags$span("Fixed Costs per Application (in Monetary Units)", tags$i(
                                icon("fas fa-info-circle"))) %>% 
                                  bsPopover("Fixed Costs Applications",
                                            fixcosts_applications_tooltip,
                                            "right",
                                            "hover",
                                            titlecolor = "white",
                                            titlesize = "16px",
                                            contentsize = "14px")
                              ),
                              
                              generateUIElementCosts(width = 4, offset = 0, style = styles, 
                                                     IDs = c("fixcosts_applications_1", "fixcosts_applications_2", "fixcosts_applications_3"), 
                                                     titles = c("Stage 1", "2", "3"), 
                                                     value = c(default_inputs$fixcosts_applications_1, default_inputs$fixcosts_applications_2, default_inputs$fixcosts_applications_3)),
                              
                              generateTitleElement(title = tags$span("Av. Work Hours per Applicant", tags$i(
                                icon("fas fa-info-circle"))) %>% 
                                  bsPopover("Work Hours Applicants",
                                            hours_applicant_tooltip,
                                            "right",
                                            "hover",
                                            titlecolor = "white",
                                            titlesize = "16px",
                                            contentsize = "14px")
                              ),
                              generateUIElementCosts(width = 4, offset = 0, style = styles, 
                                                     IDs = c("hours_applicant_1", "hours_applicant_2", "hours_applicant_3"), 
                                                     titles = c("Stage 1", "2", "3"), 
                                                     value = c(default_inputs$hours_applicant_1, default_inputs$hours_applicant_2, default_inputs$hours_applicant_3)),
                              
                              generateTitleElement(title = tags$span("Av. Cost of Work Hour (Applicant)", tags$i(
                                icon("fas fa-info-circle"))) %>% 
                                  bsPopover("Cost per Hour Applicants",
                                            price_hour_applicant_tooltip,
                                            "right",
                                            "hover",
                                            titlecolor = "white",
                                            titlesize = "16px",
                                            contentsize = "14px")
                              ),
                              generateUIElementCosts(width = 4, offset = 0, style = styles, 
                                                     IDs = c("price_hour_applicant_1", "price_hour_applicant_2", "price_hour_applicant_3"), 
                                                     titles = c("Stage 1", "2", "3"), 
                                                     value = c(default_inputs$price_hour_applicant_1, default_inputs$price_hour_applicant_2, default_inputs$price_hour_applicant_3)),
                              
                              generateTitleElement(title = tags$span("Av. Number of People Working on Application", tags$i(
                                icon("fas fa-info-circle"))) %>% 
                                  bsPopover("Number of Applicants per Application",
                                            num_applicants_per_application_tooltip,
                                            "right",
                                            "hover",
                                            titlecolor = "white",
                                            titlesize = "16px",
                                            contentsize = "14px")
                              ),
                              generateUIElementCosts(width = 4, offset = 0, style = styles, 
                                                     IDs = c("num_applicants_per_application_1", "num_applicants_per_application_2", "num_applicants_per_application_3"), 
                                                     titles = c("Stage 1", "2", "3"), 
                                                     value = c(default_inputs$num_applicants_per_application_1, default_inputs$num_applicants_per_application_2, default_inputs$num_applicants_per_application_3)),
                       ),
                       
                       # Elements which control the reviewer costs of the process
                       
                       column(2, 
                              
                              shinydashboard::box(title = "Reviewer Costs", background = "light-blue", width = box_width),
                              
                              generateTitleElement(title = tags$span("Fixcost per Review (in Monetary Units)", tags$i(
                                icon("fas fa-info-circle"))) %>% 
                                  bsPopover("Fixed Costs Reviews",
                                            fixcosts_review_tooltip,
                                            "right",
                                            "hover",
                                            titlecolor = "white",
                                            titlesize = "16px",
                                            contentsize = "14px")
                              ),
                              generateUIElementCosts(width = 4, offset = 0, style = styles,
                                                     IDs = c("fixcosts_review_1", "fixcosts_review_2", "fixcosts_review_3"),
                                                     titles = c("Stage 1", "2", "3"),
                                                     value = c(default_inputs$fixcosts_review_1, default_inputs$fixcosts_review_2, default_inputs$fixcosts_review_3)),
                              
                              generateTitleElement(title = tags$span("Av. Work Hours per Reviewer", tags$i(
                                icon("fas fa-info-circle"))) %>% 
                                  bsPopover("Work Hours Reviewer",
                                            hours_reviewer_tooltip,
                                            "right",
                                            "hover",
                                            titlecolor = "white",
                                            titlesize = "16px",
                                            contentsize = "14px")
                              ),
                              generateUIElementCosts(width = 4, offset = 0, style = styles,
                                                     IDs = c("hours_reviewer_1", "hours_reviewer_2", "hours_reviewer_3"),
                                                     titles = c("Stage 1", "2", "3"),
                                                     value = c(default_inputs$hours_reviewer_1, default_inputs$hours_reviewer_2, default_inputs$hours_reviewer_3)),
                              
                              generateTitleElement(title = tags$span("Av. Cost of Work Hour (Reviewer)", tags$i(
                                icon("fas fa-info-circle"))) %>% 
                                  bsPopover("Cost per Hour Reviewer",
                                            price_hour_reviewer_tooltip,
                                            "right",
                                            "hover",
                                            titlecolor = "white",
                                            titlesize = "16px",
                                            contentsize = "14px")
                              ),
                              generateUIElementCosts(width = 4, offset = 0, style = styles,
                                                     IDs = c("price_hour_reviewer_1", "price_hour_reviewer_2", "price_hour_reviewer_3"),
                                                     titles = c("Stage 1", "2", "3"),
                                                     value = c(default_inputs$price_hour_reviewer_1, default_inputs$price_hour_reviewer_2, default_inputs$price_hour_reviewer_3)),
                              actionButton("update_costs_plots", "Update Plots", class = "btn btn-danger", width = '100%'),
                              
                              
                              
                       ),
                       # plot output of work hours (applicants/reviewer) per application and total by stage
                       column(2,    
                              shinydashboard::box(title = tags$span("Work Hours", tags$i(
                                icon("fas fa-info-circle"))) %>% 
                                  bsPopover("Plot Work Hours",
                                            plot_hours_applicants_reviewer_tooltip,
                                            "right",
                                            "hover",
                                            titlecolor = "white",
                                            titlesize = "16px",
                                            contentsize = "14px"),
                                
                                background = "teal", width = box_width),
                              plotOutput("plot_hours_applicants_reviewer"),
                              
                       ),
                       column(2,    
                              shinydashboard::box(title = tags$span("Costs", tags$i(
                                icon("fas fa-info-circle"))) %>% 
                                  bsPopover("Plot Costs",
                                            plot_costs_applicants_reviewer_tooltip,
                                            "right",
                                            "hover",
                                            titlecolor = "white",
                                            titlesize = "16px",
                                            contentsize = "14px"),
                                background = "teal", width = box_width),
                              plotOutput("plot_costs_applicants_reviewer"),
                              
                       ),
                       column(2,
                              shinydashboard::box(title = tags$span("Costs by Stage", tags$i(
                                icon("fas fa-info-circle"))) %>% 
                                  bsPopover("Plot Total Costs by Stage",
                                            plot_total_costs_tooltip,
                                            "right",
                                            "hover",
                                            titlecolor = "white",
                                            titlesize = "16px",
                                            contentsize = "14px"),
                                background = "teal", width = box_width),
                              
                              plotOutput("total_costs_plot"),
                       ),
                       # plots of costs by stage and costs by winning and losing applications:
                       column(2,
                              shinydashboard::box(title = tags$span("Costs by Application Status", tags$i(
                                icon("fas fa-info-circle"))) %>% 
                                  bsPopover("Plot Costs by Winning/Losing",
                                            plot_win_lose_cost_tooltip,
                                            "top",
                                            "hover",
                                            titlecolor = "white",
                                            titlesize = "16px",
                                            contentsize = "14px"),
                                background = "teal", width = box_width),
                              
                              
                              plotOutput("win_lose_cost_plot")
                       ),
                     ),
                     
                     
            ),
            ###### Save and Compare ######
            tabPanel("Save and Compare",
                     fluidRow(
                       
                       column(1,
                              shinydashboard::box(id = "box_save", title = tags$span("Save", tags$i(
                                icon("fas fa-info-circle"))) %>% 
                                  bsPopover("Saving Outputs",
                                            box_save_tooltip,
                                            "right",
                                            "hover",
                                            titlecolor = "white",
                                            titlesize = "16px",
                                            contentsize = "14px"),
                                background = "black", width = 14)
                       ),
                       column(3,
                              shinydashboard::box(title = tags$span("Cost Comparison of Saved Processes", tags$i(
                                icon("fas fa-info-circle"))) %>% 
                                  bsPopover("Comparison of Costs",
                                            plot_cost_comparison_tooltip,
                                            "right",
                                            "hover",
                                            titlecolor = "white",
                                            titlesize = "16px",
                                            contentsize = "14px"),
                                background = "teal", width = 14)
                       ),
                       column(3,
                              shinydashboard::box(title = tags$span("Success Comparison of Saved Processes", tags$i(
                                icon("fas fa-info-circle"))) %>% 
                                  bsPopover("Comparison Idea Quality",
                                            plot_iq_comparison_tooltip,
                                            "right",
                                            "hover",
                                            titlecolor = "white",
                                            titlesize = "16px",
                                            contentsize = "14px"),
                                background = "teal", width = 14)
                       ),
                       column(3,
                              shinydashboard::box(title = tags$span("Group Proportion Comparison of Saved Processes", tags$i(
                                icon("fas fa-info-circle"))) %>% 
                                  bsPopover("Comparison Group Proportion",
                                            plot_div_comparison_tooltip,
                                            "right",
                                            "hover",
                                            titlecolor = "white",
                                            titlesize = "16px",
                                            contentsize = "14px"),
                                background = "teal", width = 14)
                       ),
                       
                     ),
                     
                     fluidRow(
                       
                       column(1,
                              textInput("textsave", "Name for Current Process"),
                              actionButton("save_current", "Save"),
                              
                              selectInput(
                                inputId = "load_dropdown",
                                label = "Select",
                                choices = "You need to save something first",
                                selectize = FALSE),
                              
                              #actionButton("load_inputs", "Load"),
                              div(style="margin-bottom:50px"),
                              actionButton("delete_current", "Delete Current", style="color: #000000; background-color: #ffffff; border-color: #ffffff'", width = "100%"),
                              actionButton("reset_save_df", "Delete All", style="color: #ffffff; background-color: #000000; border-color: #000000'", width = "100%"),
                              
                       ),
                       
                       column(3,
                              plotOutput("cost_comparison")
                       ),
                       
                       column(3,
                              plotOutput("success_comparison")
                       ),
                       
                       column(3,
                              plotOutput("proportion_comparison")
                       ),
                       
                     ),   
                     
            ),
),



)

###### Server ######

# Define server logic 
server <- function(input, output, session) {
  
  
  # group names for the plots and is only changed on button click
  group_names <- reactive({
    input$update_results_plots
    input$update_plot_prediction_1
    input$update_plot_prediction_2
    input$update_plot_prediction_3
    input$update_costs_plots
    
    c(isolate(input$name_disadv), isolate(input$name_adv))
  })
  
  # this one is for display purposes on the group proportions slider and is changed immediately
  # I might have used this for the plots as well, wrapped in isolate
  group_names_display <- reactive({
    c(input$name_disadv, input$name_adv)
  })
  
  ###### Server Functions ###### 
  
  # delete point on double click in plots in advanced bias setting
  delete_point <- function(data, grouping, input_dbl_click) {
    if ( dim(data)[1] > 0) {
      points$points_combined <- rbind(data %>% select(!.data[[grouping]]), c(input_dbl_click$x, input_dbl_click$y))
      d <- as.matrix(dist(points$points_combined))
      d_only_new_point <- d[(nrow(data)+1), 1:nrow(data)]
      i <- which.min(d_only_new_point)
      if (d_only_new_point[i] < 3){
        data <- data[-i, ]
        return(data)
      }else{
        return(data)
      }
    }else{
      return(data)
    }
  }
  
  # fit the model based on the points in the plots and the requested terms
  # called by output_coefficients below
  fit_model <- function(formula_terms, data){
    formula_model <- as.formula(paste0("y ~ ", paste(formula_terms, collapse = " + ")))
    model <- lm(formula_model, data = data %>% filter(group == group))
    
    coefficients_df <- as.data.frame(model$coefficients)
    coefficients_df <- dplyr::rename(coefficients_df, values = `model$coefficients`)
    
    terms_not_in_model <- c("(Intercept)", potential_model_terms)[!c("(Intercept)", potential_model_terms) %in% rownames(coefficients_df)]
    coefficients_not_in_model <- data.frame(row.names = terms_not_in_model, coefficients = rep(0, length(terms_not_in_model)))
    coefficients_not_in_model <- dplyr::rename(coefficients_not_in_model, values = coefficients)
    
    coefficients_df <- rbind(coefficients_df, coefficients_not_in_model)
    coefficients_df <- rownames_to_column(coefficients_df, "coefficients")
    
    return(coefficients_df)
    
  }
  
  
  
  # function which will generate the coefficients of the model fit
  output_coefficients <- function(input_terms, requested_model_terms, formula_terms, data, default_intercept){
    
    if (dim(data)[1] == 0) {
      
      coefficients_temp <- data.frame(coefficients = c("(Intercept)", "x", "I(x^2)", "I(x^3)"), values = c(default_intercept, 0, 0, 0))
      return(coefficients_temp)
    }
    
    if(dim(data)[1] > max(which(requested_model_terms))){ 
      if(length(formula_terms)==0){
        formula_terms <- "1"
      }
      
      coefficients_temp <- fit_model(formula_terms, data)
      
      return(coefficients_temp)
    }else{
      requested_model_terms <- potential_terms[requested_model_terms][which(requested_model_terms) < dim(data)[1]]
      formula_terms <- potential_model_terms[potential_terms %in% requested_model_terms]
      if(length(formula_terms)==0){
        formula_terms <- "1"
      }
      
      coefficients_temp <- fit_model(formula_terms, data)
      
      return(coefficients_temp)
    }
  }
  
  
  # potential terms which users can select and their corresponding names in a formula object. Needed for calculation
  potential_terms <- c("linear", "quadratic", "cubic")
  potential_model_terms <- c("x", "I(x^2)", "I(x^3)")
  
  # combine coefficients from models in one data frame
  coefficients_combined <- function(input_terms, data, default_intercepts, parameter, stage){
    
    terms_realized <- potential_terms %in% input_terms
    
    formula_terms <- potential_model_terms[terms_realized]
    
    
    coefficients_adv <- output_coefficients(input_terms, terms_realized, formula_terms, data %>% filter(group == 1), default_intercepts[1])
    
    coefficients_disadv <- output_coefficients(input_terms, terms_realized, formula_terms, data %>% filter(group == 0), default_intercepts[2])
    
    coefficients <- rbind(coefficients_adv, coefficients_disadv)
    parameter_stage <- data.frame(parameter = rep(parameter, 8), stage = rep(stage, 8))
    
    coefficients <- cbind(coefficients, parameter_stage)
    
    return(coefficients)
  }
  
  
  # this generates the value for the error term distributions (what is mean, sd, and skew at idea quality x given the model?)
  curve_function <- function(coefficients, x){
    coefficients[1] + coefficients[2]*x + coefficients[3]*x^2 + coefficients[4]*x^3
  }
  
  # this function combines the resulting curves (error distributions) to plot in the bias overview plots
  combine_curve_data <- function(adv_mean, disadv_mean, adv_sd, disadv_sd, adv_skew, disadv_skew){
    
    adv_mean <- adv_mean %>% rename_with(~ adv_mean$parameter[1], y) %>% select(-parameter)
    disadv_mean <- disadv_mean %>% rename_with(~ disadv_mean$parameter[1], y) %>% select(-parameter)
    adv_sd <- adv_sd %>% rename_with(~ adv_sd$parameter[1], y) %>% select(-parameter)
    disadv_sd <- disadv_sd %>% rename_with(~ disadv_sd$parameter[1], y) %>% select(-parameter)
    adv_skew <- adv_skew %>% rename_with(~ adv_skew$parameter[1], y) %>% select(-parameter)
    disadv_skew <- disadv_skew %>% rename_with(~ disadv_skew$parameter[1], y) %>% select(-parameter)
    
    curves_adv <- list(adv_mean, adv_sd, adv_skew) %>% reduce(full_join, by = c("x", "group", "stage"))
    curves_disadv <- list(disadv_mean, disadv_sd, disadv_skew) %>% reduce(full_join, by = c("x", "group", "stage"))
    
    rbind(curves_adv, curves_disadv)
  }
  
  # calculating quantiles to display in the bias overview plots
  calculate_quantiles <- function(data, quantiles){
    data$q1 <- qsnorm(quantiles[1], data$mean, data$sd, data$skew)
    data$q2 <- qsnorm(quantiles[2], data$mean, data$sd, data$skew)
    return(data)
  }
  
  # function to plot the individual plots in the advanced settings
  plot_function <- function(data, mapping_total, mapping_group, curve_adv, curve_disadv, ylims){
    
    ggplot(data, mapping_total)+
      geom_point(mapping_group, size = 3)+
      scale_shape_manual(name = "Group", values = c(16, 17), breaks = c(0,1), labels = group_names())+
      scale_color_manual(name = "Group", values = colors_groups, breaks = c(0,1), labels = group_names())+   
      xlim(c(70,130))+
      ylim(ylims)+
      xlab("Idea Quality") +
      ylab("Parameter Magnitude") +
      geom_line(data = curve_adv, linetype = "solid", color = colors_groups[2], size = 1.3)+
      geom_line(data = curve_disadv, linetype = "dashed", color = colors_groups[1], size = 1.3)+
      theme_minimal()+
      theme(
        axis.title.x = element_text(face = "bold", size = 14),
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 14),
        title = element_text(face = "bold", size = 14),
        legend.position = "none")
    
  }
  
  # given the applications and the reviewer ratings, this function calculates the deciding value by which the applications are evaluated:
  # mean, min, or max
  calculate_rating <- function(data, rev_mode, which_stage) {
    if(rev_mode == "mean"){
      out <- rowMeans(across(matches(paste0("R_[0-9]+__", which_stage, "$"))))  
    } else if(rev_mode == "min"){
      out <- select(data, matches(paste0("R_[0-9]+__", which_stage, "$"))) %>%  reduce(pmin)
    }else if(rev_mode == "max"){
      out <- select(data, matches(paste0("R_[0-9]+__", which_stage, "$"))) %>%  reduce(pmax)
    }
    return(out)
  }
  
  # this function applices ratings to each application- as many as there are reviewers in a given stage
  assign_application_ratings <- function(data, stage, max_stage, coefficients_disadv, coefficients_adv, number_reviewer, mode){
    
    if(mode == "Competitive" | mode == "Normative"){
      validate(
        need(number_reviewer > 0, "Please choose a positive number of reviewers")
      )
      
      for(i in 1:number_reviewer){
        
        data <- data %>%
          lazy_dt() %>% # lazy_dt is from the dtplyr package and internally converts tibbles/dataframe to data.tables so that the higher speed can be used
          # in the following line, the "observed idea quality score" per reviewer is created:
          mutate(!!(paste0("R_", i, "__", stage)) := idea_qualities + ifelse(group == 0, 
                                                                             rsnorm(idea_qualities, 
                                                                                    curve_function(coefficients = coefficients_disadv[[1]], x = idea_qualities),
                                                                                    abs(curve_function(coefficients = coefficients_disadv[[2]], x = idea_qualities)),
                                                                                    abs(curve_function(coefficients = coefficients_disadv[[3]], x = idea_qualities))),
                                                                             rsnorm(idea_qualities, 
                                                                                    curve_function(coefficients = coefficients_adv[[1]], x = idea_qualities),
                                                                                    abs(curve_function(coefficients = coefficients_adv[[2]], x = idea_qualities)),
                                                                                    abs(curve_function(coefficients = coefficients_adv[[3]], x = idea_qualities))))) %>% 
          as.tibble() # convert back to tibble for further manipulation
      }
    }
    
    return(data)
  }
  
  # this function ranks applicants (competitive), selects the ones above are cut-off (normative) or randomly selects (lottery)
  decide_winners <- function(data, stage, number_promoted = NULL, rejection_rate = NULL, cut_off = NULL, mode, review_mode){
    
    if(mode == "Competitive" | mode == "Normative"){
      
      data <- data %>% 
        mutate(!!(paste0(review_mode, "_rating__", stage))  := calculate_rating(., rev_mode = review_mode, which_stage = stage)) %>% 
        lazy_dt() %>% 
        group_by(sample_id) %>% 
        # technically, the next line might be better placed after the following if because ranks are not needed in Normative
        mutate(!!(paste0(review_mode, "_rank__", stage)) := rank(-!!as.symbol(paste0(review_mode, "_rating__", stage)), ties.method = "random")) %>% 
        ungroup() %>% 
        as.tibble()
      
      if(mode == "Competitive"){
        data <- data %>% 
          lazy_dt() %>% 
          mutate(!!(paste0("to_next__", stage)) := if_else(!!as.symbol(paste0(review_mode, "_rank__", stage)) <= number_promoted, 1, 0)) %>% 
          as.tibble()
      }
      else if(mode == "Normative"){
        data <- data %>% 
          lazy_dt() %>% 
          group_by(sample_id) %>% 
          mutate(!!(paste0("to_next__", stage)) := if_else(!!as.symbol(paste0(review_mode, "_rating__", stage)) > cut_off, 1, 0)) %>% 
          ungroup()  %>% 
          as.tibble()
      }
      
    }
    else if(mode == "Lottery"){
      data <- data %>% 
        lazy_dt() %>% 
        group_by(sample_id) %>% 
        mutate(!!(paste0("to_next__", stage)) := sample(rep(c(1, 0), c(number_promoted, n() - number_promoted)), n())) %>% 
        ungroup() %>% 
        as.tibble()
    }
    
    return(data)
    
  }
  
  # function to transform group proportions in a space so we can calculate standard deviations for means across all samples
  arcsin_transform <- function(x){return(asin(sqrt(x)))}
  
  
  
  
  # this function calculates the position of three points for mean and standard deviation for each stage based on the quick options for biases (dropdown buttons)
  # these options work by inserting three points in the respective plot which have different heights, thus leading to different model coefficients
  quick_options_function <- function(inputs, stage, points, parameter){ 
    
    # inputs[1] is the sign of the association parameter and idea quality
    if(inputs[1] == "Positive"){
      sign_assoc <- 1
    } else if(inputs[1] == "Negative"){
      sign_assoc <- -1
    }
    
    # x coordinates        
    point1_x <- 100
    point2_x <- 85
    point3_x <- 115
    
    # some parameters need to be adjusted whether the mean or the standard deviation is to be changed
    # the initial value of the intercept:
    if(parameter == "mean"){
      
      magnitude_base <- coeff_mean_default[1]
      
      
      basic_y_diff <- 2 # the absolute difference of the y-coord of the middle point to the other y values
      
      multipliers_assoc <- c(0, 1, 1.5, 2) # multiplier for each answer "no", "small", "medium", "large"
      bias_multiplicator <- c(0, 2, 4, 8) # another multiplier for bias
      inputs_2 <- as.numeric(inputs[2]) # association of mean and idea quality 
      inputs_3 <- as.numeric(inputs[3]) # bias in magnitude aka intercept difference 
      inputs_4 <- as.numeric(inputs[4]) # bias in slope
      inputs_5 <- 0   # only needed for SD
      bias_sign <- 1 # direction of the bias - needs to be different for mean and sd
      enhancement_factor <- 1 # another factor to adjust bias size
      
      
    }else if(parameter == "sd"){
      
      magnitude_base <- coeff_sd_default[1]
      
      basic_y_diff <- 2
      
      multipliers_assoc <- c(0, .5, 1, 1.5)
      bias_multiplicator <- c(0, 4, 8, 16)
      inputs_2 <- as.numeric(inputs[8]) # association of error sd with idea quality input$sd_assoc_1, 
      inputs_3 <- as.numeric(inputs[7]) # difference of sd intercept between groups input$sd_magn_bias_1,
      inputs_4 <- as.numeric(inputs[9]) # difference in slope input$sd_assoc_bias_1
      inputs_5 <- as.numeric(inputs[6]) # general intercept input$sd_magn_1, 
      bias_sign <- -1
      enhancement_factor <- 5
      
      
    }
    
    
    points$data <- points$data[0,] # if a user chooses one option, delete what was there previously
    # terms for the advantaged group
    points$data[1,] <- c(point1_x, magnitude_base*inputs_5, 1) # the middle point is moved up and down with the magnitude term (only sd)
    points$data[2,] <- c(point2_x, magnitude_base*inputs_5  -  basic_y_diff*sign_assoc*multipliers_assoc[inputs_2], 1) # the other two are moved up and down depending on sign
    points$data[3,] <- c(point3_x, magnitude_base*inputs_5  +  basic_y_diff*sign_assoc*multipliers_assoc[inputs_2], 1)
    
    if(inputs_3 != 0 | inputs_4 != 0){ # if one of the inputs that are about bias are non zero, also update the points for the disadvantaged group
      points$data[4,] <- c(point1_x, magnitude_base*inputs_5  -  bias_multiplicator[inputs_3], 0)
      points$data[5,] <- c(point2_x, magnitude_base*inputs_5  -  basic_y_diff*sign_assoc*multipliers_assoc[inputs_2]  -  bias_multiplicator[inputs_3]*bias_sign, 0)
      points$data[6,] <- c(point3_x, magnitude_base*inputs_5  +  basic_y_diff*sign_assoc*multipliers_assoc[inputs_2]  -  bias_multiplicator[inputs_3]*bias_sign  -  multipliers_assoc[inputs_4]*bias_sign*enhancement_factor, 0)
    }
    
    # delete points when they are back to default
    if (parameter == "mean" & all(points$data$y == coeff_mean_default[1])){
      points$data <- points$data[0,]
    }
    
    if (parameter == "sd" & all(points$data$y == coeff_sd_default[1])){
      points$data <- points$data[0,]
    }
    
    return(points$data)
    
  }
  
  
  
  
  
  
  
  
  ###### Display/hide elements ######
  # Some housekeeping when to display which elements
  
  
  
  
  # the mean is fixed at an arbitrary value
  toggleState(id = "mean_ideas")
  
  
  
  # display only the inputs necessary for the respective number of stages and competition_mode
  observeEvent(c(input$stages, input$mode_1, input$mode_2, input$mode_3, input$num_reviewers_per_application_1, input$num_reviewers_per_application_2, input$num_reviewers_per_application_3), {
    toggle(id = "mode_2", condition = (input$stages > 1))
    toggle(id = "mode_3", condition = (input$stages > 2))
    
    toggle(id = "num_reviewers_per_application_2", condition = (input$stages > 1))
    toggle(id = "num_reviewers_per_application_3", condition = (input$stages > 2))
    
    toggle(id = "review_mode_1", condition = (input$mode_1 != "Lottery"))
    toggle(id = "review_mode_2", condition = (input$stages > 1 && input$mode_2 != "Lottery"))
    toggle(id = "review_mode_3", condition = (input$stages > 2 && input$mode_3 != "Lottery"))
    
    toggle(id = "num_accepted_1", condition = (input$mode_1 != "Normative"))
    toggle(id = "num_accepted_2", condition = (input$stages > 1 && input$mode_2 != "Normative"))
    toggle(id = "num_accepted_3", condition = (input$stages > 2 && input$mode_3 != "Normative"))
    
    toggle(id = "cut_off_1", condition = input$mode_1 == "Normative", )
    toggle(id = "cut_off_2", condition = input$stages > 1 && input$mode_2 == "Normative")
    toggle(id = "cut_off_3", condition = input$stages > 2 && input$mode_3 == "Normative")
    
    toggle(id = "model_terms_mean_2", condition = (input$stages > 1))
    toggle(id = "plotMean2", condition = (input$stages > 1))
    toggle(id = "model_terms_mean_3", condition = (input$stages > 2))
    toggle(id = "plotMean3", condition = (input$stages > 2))
    
    toggle(id = "model_terms_sd_2", condition = (input$stages > 1))
    toggle(id = "plotsd2", condition = (input$stages > 1))
    toggle(id = "model_terms_sd_3", condition = (input$stages > 2))
    toggle(id = "plotsd3", condition = (input$stages > 2))
    
    toggle(id = "model_terms_skew_2", condition = (input$stages > 1))
    toggle(id = "plotSkew2", condition = (input$stages > 1))
    toggle(id = "model_terms_skew_3", condition = (input$stages > 2))
    toggle(id = "plotSkew3", condition = (input$stages > 2))
    
    
    toggle(id = "mean_stage_2", condition = input$stages > 1 )
    toggle(id = "sd_stage_2", condition = input$stages > 1 )
    
    toggle(id = "bias_2_1", condition = input$stages > 1 )
    toggle(id = "bias_2_2", condition = input$stages > 1 )
    toggle(id = "bias_2_3", condition = input$stages > 1 )
    toggle(id = "bias_2_4", condition = input$stages > 1 )
    
    toggle(id = "mean_stage_3", condition = input$stages > 2 )
    toggle(id = "sd_stage_3", condition = input$stages > 2 )
    
    toggle(id = "bias_3_1", condition = input$stages > 2 )
    toggle(id = "bias_3_2", condition = input$stages > 2 )
    toggle(id = "bias_3_3", condition = input$stages > 2 )
    toggle(id = "bias_3_4", condition = input$stages > 2 )
    
    
    toggle(id = "plot_stage_2", condition = input$stages > 1 )
    toggle(id = "plot_stage_3", condition = input$stages > 2 )
    
    toggle(id = "plot_prediction_stage_2", condition = input$stages > 1 )
    toggle(id = "plot_prediction_stage_3", condition = input$stages > 2 )
    
    toggle(id = "box_stage_1", condition = input$mode_1 != "Lottery")
    
    toggle(id = "box_stage_2", condition = input$stages > 1 & input$mode_2 != "Lottery")
    toggle(id = "box_stage_3", condition = input$stages > 2 & input$mode_3 != "Lottery")
    
    
    
    # this toggles the UI elements for COSTS off so that the higher stages gets hidden when the user selects for a process with less than 3 stages
    #observe({
    
    toggle(id = "fixcosts_applications_2", condition = input$stages > 1)
    toggle(id = "fixcosts_applications_3", condition = input$stages > 2)
    
    toggle(id = "hours_applicant_2", condition = input$stages > 1)
    toggle(id = "hours_applicant_3", condition = input$stages > 2)
    
    toggle(id = "price_hour_applicant_2", condition = input$stages > 1)
    toggle(id = "price_hour_applicant_3", condition = input$stages > 2)
    
    toggle(id = "num_applicants_per_application_2", condition = input$stages > 1)
    toggle(id = "num_applicants_per_application_3", condition = input$stages > 2)
    
    toggle(id = "fixcosts_review_2", condition = input$stages > 1)
    toggle(id = "fixcosts_review_3", condition = input$stages > 2)
    
    toggle(id = "hours_reviewer_2", condition = input$stages > 1)
    toggle(id = "hours_reviewer_3", condition = input$stages > 2)
    
    toggle(id = "price_hour_reviewer_2", condition = input$stages > 1)
    toggle(id = "price_hour_reviewer_3", condition = input$stages > 2)
    
    
    toggleState(id = "review_mode_1", condition = input$num_reviewers_per_application_1 > 1)
    toggleState(id = "review_mode_2", condition = input$num_reviewers_per_application_2 > 1)
    toggleState(id = "review_mode_3", condition = input$num_reviewers_per_application_3 > 1)
  }, ignoreInit = F)
  
  
  # this will update the group names in the advanced bias settings to keep it in line with the user input
  observeEvent(group_names_display(), {
    updateRadioButtons(inputId = "group_to_draw_1", choiceNames = list(tags$span(style = paste0("color:", colors_groups[2]), input$name_adv), tags$span(style = paste0("color:", colors_groups[1]), input$name_disadv)), choiceValues = c(1,0), inline = T)
    updateRadioButtons(inputId = "group_to_draw_2", choiceNames = list(tags$span(style = paste0("color:", colors_groups[2]), input$name_adv), tags$span(style = paste0("color:", colors_groups[1]), input$name_disadv)), choiceValues = c(1,0), inline = T)
    updateRadioButtons(inputId = "group_to_draw_3", choiceNames = list(tags$span(style = paste0("color:", colors_groups[2]), input$name_adv), tags$span(style = paste0("color:", colors_groups[1]), input$name_disadv)), choiceValues = c(1,0), inline = T)
  })
  
  # make sure that the user can not advance more applications than there are applications
  observeEvent(c(input$num_applications, input$number_samples, input$group_proportions, input$opportunity_factor), {
    updateSliderInput(inputId = "num_accepted_1", max = input$num_applications)
  })
  
  # the same as above, but now to determine the upper bound of advancing applications we must calculate how many advance from the round before
  observe({
    if(input$mode_1 == "Normative"){
      upper_bound <- results_long_by_stage() %>%# this is actually calculated below but I think it makes sense to place it here anyway
        lazy_dt() %>%
        filter(stage == 1 & to_next == 1) %>% # winners of stage 1
        group_by(sample_id) %>% 
        summarize(number = n()) %>% 
        summarize(min = min(number)) %>% # this is not perfect for now - in a normative setting, the number of advancing applications depend on the initial quality
        # thus, they might be different between samples, but we need to define a value for ALL samples - for now, this is the minimum
        ungroup() %>% 
        as.tibble() %>%
        deframe() 
    } else{
      upper_bound <- input$num_accepted_1 # easier for Lottery and competitive where we can just use the upper bound from the previous round
    }
    if (input$num_accepted_1 > (input$num_accepted_2 + 1)){
      updateSliderInput(inputId = "num_accepted_2", max = round(upper_bound))
    } else {
      updateSliderInput(inputId = "num_accepted_2", value = round(upper_bound), max = round(upper_bound))
    }
  })
  
  # same but for third stage
  observe({
    if(input$mode_2 == "Normative"){
      upper_bound <- results_long_by_stage() %>%
        lazy_dt() %>%
        filter(stage == 2 & to_next == 1) %>%
        group_by(sample_id) %>% 
        summarize(number = n()) %>% 
        summarize(min = min(number)) %>% 
        ungroup() %>% 
        as.tibble() %>%
        deframe() 
    } else{
      upper_bound <- input$num_accepted_2
    }
    if (input$num_accepted_2 > (input$num_accepted_3 + 1)){
      updateSliderInput(inputId = "num_accepted_3", max = upper_bound)
    } else {
      updateSliderInput(inputId = "num_accepted_3", value = upper_bound, max = upper_bound)
    }
  })
  
  
  
  
  ####### Initial Groups ####### 
  
  
  
  # calculate and show the group proportions after factoring in opportunity cost for application:
  
  proportion_advantage_initial <- reactive(1 - input$group_proportions)
  
  
  proportion_disadvantage <- reactive(input$group_proportions*(1-input$opportunity_factor)) #reactive(input$group_proportions/input$opportunity_factor) 
  proportion_advantage <- reactive(1 - proportion_disadvantage())
  
  
  # this is the plot of the proportion how many people apply (the blue/yellow bar without axes in the group box in the general settings)
  output$group_proportions_application <- renderPlot({ 
    
    df <- data.frame("group" = factor(c(input$name_disadv, input$name_adv), levels = c(input$name_disadv, input$name_adv)), "prop" = c(proportion_disadvantage(), proportion_advantage()), "label_pos" = c(0.01, .99))
    
    ggplot(df, aes(y=as.factor(1), x = prop, fill = group))+
      geom_bar(stat="identity", orientation = "y", position=position_stack(reverse = TRUE))+
      scale_fill_manual(name="Group",  label= c("Female", "Male"), values = colors_groups)+
      geom_text(mapping = aes(y = 1, x = label_pos[1], label = group[1]), vjust = .5, hjust = 0, colour = "white", fontface = "bold", size = 5)+
      geom_text(mapping = aes(y = 1, x = label_pos[2], label = group[2]), vjust = .5, hjust = 1, colour = "white", fontface = "bold", size = 5)+
      
      scale_x_continuous(breaks = c(round(df$prop[1],2)/2, round(df$prop[1],2) + round(df$prop[2],2)/2), labels = c(paste0(round(df$prop[1],2)*100, " %"), paste0(round(df$prop[2],2)*100, " %")))+
      theme_minimal()+
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_text(face = "bold", size = 16),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.position = "none"
      )
    
  })
  
  
  
  ####### Parameters for scenarios ####### 
  
  
  # define parameters for scenarios
  observeEvent(input$scenarios, {
    #R01
    
    if(input$scenarios == 1){
      
      
      updateRadioGroupButtons(inputId = "bias_1", selected = character(0))
      
      updateRadioButtons(session = session, inputId = "sign_mean_assoc_1", selected = "Positive")
      updateRadioButtons(session = session, inputId = "mean_assoc_1", selected = 1)
      updateRadioButtons(session = session, inputId = "mean_magn_bias_1", selected = 3)
      updateRadioButtons(session = session, inputId = "mean_assoc_bias_1", selected = 1)
      updateRadioButtons(session = session, inputId = "sign_sd_assoc_1", selected = "Positive")
      updateRadioButtons(session = session, inputId = "sd_magn_1", selected = 3)
      updateRadioButtons(session = session, inputId = "sd_magn_bias_1", selected = 1)
      updateRadioButtons(session = session, inputId = "sd_assoc_1", selected = 1)
      updateRadioButtons(session = session, inputId = "sd_assoc_bias_1", selected = 1)
      
      
      updateAutonumericInput(inputId = "available_funding", value = 10000000)
      updateAutonumericInput(inputId = "target_grand_volume", value = 500000)
      updateNumericInput(inputId = "num_applications", value = 200)
      updateNumericInput(inputId = "stages", value = 1)
      updateNumericInput(inputId = "number_samples", value = 10)
      
      updateNumericInput(inputId = "num_reviewers_per_application_1", value = 2)
      updateSelectInput(inputId = "mode_1", selected = "Competitive", session = session)
      updateSliderInput(inputId = "num_accepted_1", value = 20, min = 1, max = 200)
      
      updateSelectInput(inputId = "review_mode_1", selected = "mean", session = session)
      
      updateNumericInput(inputId = "fixcosts_applications_1", value = 0)
      updateNumericInput(inputId = "hours_applicant_1", value = 80)
      updateNumericInput(inputId = "price_hour_applicant_1", value = 50)
      updateNumericInput(inputId = "num_applicants_per_application_1", value = 1)
      updateNumericInput(inputId = "fixcosts_review_1", value = 0)
      updateNumericInput(inputId = "hours_reviewer_1", value = 6)
      updateNumericInput(inputId = "price_hour_reviewer_1", value = 50)
      updateTextInput(inputId = "name_adv", label = "Advantaged", value = "Male")
      updateTextInput(inputId = "name_disadv", label = "Disadvantaged", value = "Female")
      updateSliderInput(inputId = "group_proportions", value = .36)       
      updateSliderInput(inputId = "opportunity_factor", value = .16) #1.2  old value, before entry bias got a new conceptual idea     
      updateNumericInput(inputId = "sd_ideas", value = 15)
      updateNumericInput(inputId = "skew_ideas", value = 1.5)
      
    } 
    
    
    #ERC
    
    if(input$scenarios == 2){
      updateAutonumericInput(inputId = "available_funding", value = 10000000)
      updateAutonumericInput(inputId = "target_grand_volume", value = 500000)
      
      updateNumericInput(inputId = "num_applications", value = 200)
      updateNumericInput(inputId = "stages", value = 2)
      updateNumericInput(inputId = "number_samples", value = 10)
      
      updateNumericInput(inputId = "num_reviewers_per_application_1", value = 2)
      updateNumericInput(inputId = "num_reviewers_per_application_2", value = 4)
      updateSelectInput(inputId = "mode_1", selected = "Competitive", session = session)
      updateSelectInput(inputId = "mode_2", selected = "Competitive", session = session)
      updateSelectInput(inputId = "review_mode_1", selected = "mean", session = session)
      updateSelectInput(inputId = "review_mode_2", selected = "mean", session = session)
      
      updateSliderInput(inputId = "num_accepted_1", value = 60, min = 1, max = 200)
      updateSliderInput(inputId = "num_accepted_2", value = 20, min = 1, max = 60)
      
      updateNumericInput(inputId = "fixcosts_applications_1", value = 0)
      updateNumericInput(inputId = "fixcosts_applications_2", value = 0)
      updateNumericInput(inputId = "hours_applicant_1", value = 160)
      updateNumericInput(inputId = "hours_applicant_2", value = 80)
      updateNumericInput(inputId = "price_hour_applicant_1", value = 50)
      updateNumericInput(inputId = "price_hour_applicant_2", value = 50)
      updateNumericInput(inputId = "num_applicants_per_application_1", value = 1)
      updateNumericInput(inputId = "num_applicants_per_application_2", value = 1)
      updateNumericInput(inputId = "fixcosts_review_1", value = 0)
      updateNumericInput(inputId = "fixcosts_review_2", value = 0)
      updateNumericInput(inputId = "hours_reviewer_1", value = 8)
      updateNumericInput(inputId = "hours_reviewer_2", value = 16)
      updateNumericInput(inputId = "price_hour_reviewer_1", value = 50)
      updateNumericInput(inputId = "price_hour_reviewer_2", value = 50)
      updateTextInput(inputId = "name_adv", label = "Advantaged", value = "Male")
      updateTextInput(inputId = "name_disadv", label = "Disadvantaged", value = "Female")
      updateSliderInput(inputId = "group_proportions", value = .36)
      updateSliderInput(inputId = "opportunity_factor", value = .22) #1.3       
      updateNumericInput(inputId = "sd_ideas", value = 15)
      updateNumericInput(inputId = "skew_ideas", value = 1.5)
      
      updateRadioGroupButtons(inputId = "bias_1", selected = character(0))
      
      
      updateRadioButtons(session = session, inputId = "sign_mean_assoc_1", selected = "Positive")
      updateRadioButtons(session = session, inputId = "mean_assoc_1", selected = 1)
      updateRadioButtons(session = session, inputId = "mean_magn_bias_1", selected = 3)
      updateRadioButtons(session = session, inputId = "mean_assoc_bias_1", selected = 1)
      updateRadioButtons(session = session, inputId = "sign_sd_assoc_1", selected = "Positive")
      updateRadioButtons(session = session, inputId = "sd_magn_1", selected = 4)
      updateRadioButtons(session = session, inputId = "sd_magn_bias_1", selected = 1)
      updateRadioButtons(session = session, inputId = "sd_assoc_1", selected = 1)
      updateRadioButtons(session = session, inputId = "sd_assoc_bias_1", selected = 1)
      
      
      
      updateCheckboxGroupInput("model_terms_mean_2", selected = c("linear"), session = session) # check that linear term is requested if people change any input
      updateCheckboxGroupInput(session = session, inputId = "model_terms_sd_2", selected = character(0))
      updateCheckboxGroupInput(session = session, inputId = "model_terms_skew_2", selected = character(0))
      
      updateRadioButtons(session = session, inputId = "sign_mean_assoc_2", selected = "Negative")
      updateRadioButtons(session = session, inputId = "mean_assoc_2", selected = 1)
      updateRadioButtons(session = session, inputId = "mean_magn_bias_2", selected = 2)
      updateRadioButtons(session = session, inputId = "mean_assoc_bias_2", selected = 4)
      updateRadioButtons(session = session, inputId = "sign_sd_assoc_2", selected = "Positive")
      updateRadioButtons(session = session, inputId = "sd_magn_2", selected = 2)
      updateRadioButtons(session = session, inputId = "sd_magn_bias_2", selected = 1)
      updateRadioButtons(session = session, inputId = "sd_assoc_2", selected = 1)
      updateRadioButtons(session = session, inputId = "sd_assoc_bias_2", selected = 1)
      
      
      updateRadioGroupButtons(inputId = "bias_2", selected = 2)
      
      
    }
    
    # SNSF lottery
    
    if(input$scenarios == 3){
      updateAutonumericInput(inputId = "available_funding", value = 10000000)
      updateAutonumericInput(inputId = "target_grand_volume", value = 500000)
      
      updateNumericInput(inputId = "num_applications", value = 200)
      updateNumericInput(inputId = "stages", value = 2)
      updateNumericInput(inputId = "number_samples", value = 10)
      updateNumericInput(inputId = "num_reviewers_per_application_1", value = 2)
      updateNumericInput(inputId = "num_reviewers_per_application_2", value = 0)
      
      updateSelectInput(inputId = "review_mode_1", selected = "mean", session = session)
      updateSelectInput(inputId = "mode_1", selected = "Competitive", session = session)
      updateSelectInput(inputId = "mode_2", selected = "Lottery", session = session)
      
      updateSliderInput(inputId = "num_accepted_1", value = 40, min = 1, max = 200)
      updateSliderInput(inputId = "num_accepted_2", value = 20, min = 1, max = 40)
      
      updateNumericInput(inputId = "fixcosts_applications_1", value = 0)
      updateNumericInput(inputId = "fixcosts_applications_2", value = 0)
      updateNumericInput(inputId = "hours_applicant_1", value = 80)
      updateNumericInput(inputId = "hours_applicant_2", value = 0)
      updateNumericInput(inputId = "price_hour_applicant_1", value = 50)
      updateNumericInput(inputId = "price_hour_applicant_2", value = 50)
      updateNumericInput(inputId = "num_applicants_per_application_1", value = 1)
      updateNumericInput(inputId = "num_applicants_per_application_2", value = 1)
      updateNumericInput(inputId = "fixcosts_review_1", value = 0)
      updateNumericInput(inputId = "fixcosts_review_2", value = 0)
      updateNumericInput(inputId = "hours_reviewer_1", value = 16)
      updateNumericInput(inputId = "hours_reviewer_2", value = 0)
      updateNumericInput(inputId = "price_hour_reviewer_1", value = 50)
      updateNumericInput(inputId = "price_hour_reviewer_2", value = 50)
      updateTextInput(inputId = "name_adv", label = "Advantaged", value = "Male")
      updateTextInput(inputId = "name_disadv", label = "Disadvantaged", value = "Female")
      updateSliderInput(inputId = "group_proportions", value = .36)    
      updateSliderInput(inputId = "opportunity_factor", value = .22) #1.3       
      updateNumericInput(inputId = "sd_ideas", value = 15)
      updateNumericInput(inputId = "skew_ideas", value = 1.5)
      
      updateRadioGroupButtons(inputId = "bias_1", selected = character(0))
      
      updateRadioButtons(session = session, inputId = "sign_mean_assoc_1", selected = "Positive")
      updateRadioButtons(session = session, inputId = "mean_assoc_1", selected = 1)
      updateRadioButtons(session = session, inputId = "mean_magn_bias_1", selected = 3)
      updateRadioButtons(session = session, inputId = "mean_assoc_bias_1", selected = 1)
      updateRadioButtons(session = session, inputId = "sign_sd_assoc_1", selected = "Positive")
      updateRadioButtons(session = session, inputId = "sd_magn_1", selected = 2)
      updateRadioButtons(session = session, inputId = "sd_magn_bias_1", selected = 1)
      updateRadioButtons(session = session, inputId = "sd_assoc_1", selected = 1)
      updateRadioButtons(session = session, inputId = "sd_assoc_bias_1", selected = 1)
      
    } 
    
    if(input$scenarios == 4){
      updateAutonumericInput(inputId = "available_funding", value = 10000000)
      updateAutonumericInput(inputId = "target_grand_volume", value = 500000)
      
      updateNumericInput(inputId = "num_applications", value = 200)
      updateNumericInput(inputId = "stages", value = 2)
      updateNumericInput(inputId = "number_samples", value = 10)
      
      updateNumericInput(inputId = "num_reviewers_per_application_1", value = 0)
      updateNumericInput(inputId = "num_reviewers_per_application_2", value = 3)
      
      updateSelectInput(inputId = "mode_1", selected = "Lottery", session = session)
      updateSelectInput(inputId = "mode_2", selected = "Normative", session = session)
      updateSelectInput(inputId = "review_mode_1", selected = "mean", session = session)
      
      updateSliderInput(inputId = "num_accepted_1", value = 50, min = 1, max = 200)
      
      updateSliderInput(inputId = "cut_off_2", value = 105)
      
      
      updateNumericInput(inputId = "fixcosts_applications_1", value = 0)
      updateNumericInput(inputId = "fixcosts_applications_2", value = 0)
      updateNumericInput(inputId = "hours_applicant_1", value = 0)
      updateNumericInput(inputId = "hours_applicant_2", value = 160)
      updateNumericInput(inputId = "price_hour_applicant_1", value = 50)
      updateNumericInput(inputId = "price_hour_applicant_2", value = 50)
      updateNumericInput(inputId = "num_applicants_per_application_1", value = 1)
      updateNumericInput(inputId = "num_applicants_per_application_2", value = 1)
      updateNumericInput(inputId = "fixcosts_review_1", value = 0)
      updateNumericInput(inputId = "fixcosts_review_2", value = 0)
      updateNumericInput(inputId = "hours_reviewer_1", value = 0)
      updateNumericInput(inputId = "hours_reviewer_2", value = 8)
      updateNumericInput(inputId = "price_hour_reviewer_1", value = 50)
      updateNumericInput(inputId = "price_hour_reviewer_2", value = 50)
      updateTextInput(inputId = "name_adv", label = "Advantaged", value = "Male")
      updateTextInput(inputId = "name_disadv", label = "Disadvantaged", value = "Female")
      updateSliderInput(inputId = "group_proportions", value = .36)   
      updateSliderInput(inputId = "opportunity_factor", value = .01) #1.01       
      updateNumericInput(inputId = "sd_ideas", value = 15)
      updateNumericInput(inputId = "skew_ideas", value = 1.5)
      
      updateRadioGroupButtons(inputId = "bias_2", selected = character(0))
      
      updateRadioButtons(session = session, inputId = "sign_mean_assoc_2", selected = "Positive")
      updateRadioButtons(session = session, inputId = "mean_assoc_2", selected = 1)
      updateRadioButtons(session = session, inputId = "mean_magn_bias_2", selected = 2)
      updateRadioButtons(session = session, inputId = "mean_assoc_bias_2", selected = 1)
      updateRadioButtons(session = session, inputId = "sign_sd_assoc_2", selected = "Positive")
      updateRadioButtons(session = session, inputId = "sd_magn_2", selected = 1)
      updateRadioButtons(session = session, inputId = "sd_magn_bias_2", selected = 1)
      updateRadioButtons(session = session, inputId = "sd_assoc_2", selected = 1)
      updateRadioButtons(session = session, inputId = "sd_assoc_bias_2", selected = 1)
      
    } 
    
  })
  
  
  
  # reset all parameters to default
  observeEvent(input$reset_general, {
    
    updateRadioGroupButtons(inputId = "scenarios", selected = character(0))
    
    updateAutonumericInput(inputId = "available_funding", value = default_inputs$available_funding)
    updateAutonumericInput(inputId = "target_grand_volume", value = default_inputs$target_grand_volume)
    
    updateNumericInput(inputId = "num_applications", value = default_inputs$num_applications)
    updateNumericInput(inputId = "stages", value = default_inputs$stages)
    updateNumericInput(inputId = "number_samples", value = default_inputs$number_samples)
    
    updateNumericInput(inputId = "num_reviewers_per_application_1", value = default_inputs$num_reviewers_per_application_1)
    updateNumericInput(inputId = "num_reviewers_per_application_2", value = default_inputs$num_reviewers_per_application_2)
    updateNumericInput(inputId = "num_reviewers_per_application_3", value = default_inputs$num_reviewers_per_application_3)
    
    updateSelectInput(inputId = "mode_1", selected = default_inputs$mode_1)
    updateSelectInput(inputId = "mode_2", selected = default_inputs$mode_2)
    updateSelectInput(inputId = "mode_3", selected = default_inputs$mode_3)
    
    updateSliderInput(inputId = "num_accepted_1", value = default_inputs$num_applications/2, min = 1, max = default_inputs$num_applications)
    updateSliderInput(inputId = "num_accepted_2", value = round(default_inputs$num_applications/4, 0), min = 1, max = round(default_inputs$num_applications/2, 0))
    updateSliderInput(inputId = "num_accepted_3", value = round(default_inputs$num_applications/8, 0), min = 1, max = round(default_inputs$num_applications/4, 0))
    
    
    updateNumericInput(inputId = "cut_off_1", value = default_inputs$cut_off_1)
    updateNumericInput(inputId = "cut_off_2", value = default_inputs$cut_off_2)
    updateNumericInput(inputId = "cut_off_3", value = default_inputs$cut_off_3)
    
    updateSelectInput(inputId = "review_mode_1", selected = default_inputs$review_mode_1)
    updateSelectInput(inputId = "review_mode_2", selected = default_inputs$review_mode_2)
    updateSelectInput(inputId = "review_mode_3", selected = default_inputs$review_mode_3)
    
    
    updateNumericInput(inputId = "fixcosts_applications_1", value = default_inputs$fixcosts_applications_1)
    updateNumericInput(inputId = "fixcosts_applications_2", value = default_inputs$fixcosts_applications_2)
    updateNumericInput(inputId = "fixcosts_applications_3", value = default_inputs$fixcosts_applications_3)
    updateNumericInput(inputId = "hours_applicant_1", value = default_inputs$hours_applicant_1)
    updateNumericInput(inputId = "hours_applicant_2", value = default_inputs$hours_applicant_2)
    updateNumericInput(inputId = "hours_applicant_3", value = default_inputs$hours_applicant_3)
    updateNumericInput(inputId = "price_hour_applicant_1", value = default_inputs$price_hour_applicant_1)
    updateNumericInput(inputId = "price_hour_applicant_2", value = default_inputs$price_hour_applicant_2)
    updateNumericInput(inputId = "price_hour_applicant_3", value = default_inputs$price_hour_applicant_3)
    updateNumericInput(inputId = "num_applicants_per_application_1", value = default_inputs$num_applicants_per_application_1)
    updateNumericInput(inputId = "num_applicants_per_application_2", value = default_inputs$num_applicants_per_application_2)
    updateNumericInput(inputId = "num_applicants_per_application_3", value = default_inputs$num_applicants_per_application_3)
    updateNumericInput(inputId = "fixcosts_review_1", value = default_inputs$fixcosts_review_1)
    updateNumericInput(inputId = "fixcosts_review_2", value = default_inputs$fixcosts_review_2)
    updateNumericInput(inputId = "fixcosts_review_3", value = default_inputs$fixcosts_review_3)
    updateNumericInput(inputId = "hours_reviewer_1", value = default_inputs$hours_reviewer_1)
    updateNumericInput(inputId = "hours_reviewer_2", value = default_inputs$hours_reviewer_2)
    updateNumericInput(inputId = "hours_reviewer_3", value = default_inputs$hours_reviewer_3)
    updateNumericInput(inputId = "price_hour_reviewer_1", value = default_inputs$price_hour_reviewer_1)
    updateNumericInput(inputId = "price_hour_reviewer_2", value = default_inputs$price_hour_reviewer_2)
    updateNumericInput(inputId = "price_hour_reviewer_3", value = default_inputs$price_hour_reviewer_3)
    
    
    updateTextInput(inputId = "name_adv", value = default_inputs$name_adv)
    updateTextInput(inputId = "name_disadv", value = default_inputs$name_disadv)
    updateSliderInput(inputId = "group_proportions", value = default_inputs$group_proportions) 
    updateSliderInput(inputId = "opportunity_factor", value = default_inputs$opportunity_factor)
    updateNumericInput(inputId = "sd_ideas", value = default_inputs$sd_ideas)
    updateNumericInput(inputId = "skew_ideas", value = default_inputs$skew_ideas)
    
    
    click("reset_bias_1")
    click("reset_bias_2")
    click("reset_bias_3")
    
    
    
  }, ignoreInit = T)
  
  # predefined biases
  
  observeEvent(input$bias_1, {
    
    
    if(input$bias_1 == 1){
      
      updateCheckboxGroupInput(session = session, inputId = "model_terms_mean_1", selected = character(0))
      updateCheckboxGroupInput(session = session, inputId = "model_terms_sd_1", selected = character(0))
      updateCheckboxGroupInput(session = session, inputId = "model_terms_skew_1", selected = character(0))
      
      updateRadioButtons(session = session, inputId = "sign_mean_assoc_1", selected = "Positive")
      updateRadioButtons(session = session, inputId = "mean_assoc_1", selected = 1)
      updateRadioButtons(session = session, inputId = "mean_magn_bias_1", selected = 3)
      updateRadioButtons(session = session, inputId = "mean_assoc_bias_1", selected = 1)
      updateRadioButtons(session = session, inputId = "sign_sd_assoc_1", selected = "Positive")
      updateRadioButtons(session = session, inputId = "sd_magn_1", selected = 1)
      updateRadioButtons(session = session, inputId = "sd_magn_bias_1", selected = 1)
      updateRadioButtons(session = session, inputId = "sd_assoc_1", selected = 1)
      updateRadioButtons(session = session, inputId = "sd_assoc_bias_1", selected = 1)
    }
    if(input$bias_1 == 2){
      
      
      updateCheckboxGroupInput("model_terms_mean_1", selected = c("linear"), session = session) # check that linear term is requested if people change any input
      updateCheckboxGroupInput(session = session, inputId = "model_terms_sd_1", selected = character(0))
      updateCheckboxGroupInput(session = session, inputId = "model_terms_skew_1", selected = character(0))
      
      
      updateRadioButtons(session = session, inputId = "sign_mean_assoc_1", selected = "Negative")
      updateRadioButtons(session = session, inputId = "mean_assoc_1", selected = 1)
      updateRadioButtons(session = session, inputId = "mean_magn_bias_1", selected = 2)
      updateRadioButtons(session = session, inputId = "mean_assoc_bias_1", selected = 4)
      updateRadioButtons(session = session, inputId = "sign_sd_assoc_1", selected = "Positive")
      updateRadioButtons(session = session, inputId = "sd_magn_1", selected = 2)
      updateRadioButtons(session = session, inputId = "sd_magn_bias_1", selected = 1)
      updateRadioButtons(session = session, inputId = "sd_assoc_1", selected = 1)
      updateRadioButtons(session = session, inputId = "sd_assoc_bias_1", selected = 1)
      
    }  
    if(input$bias_1 == 3){  
      
      
      updateCheckboxGroupInput(session = session, inputId = "model_terms_mean_1", selected = character(0))
      updateCheckboxGroupInput("model_terms_sd_1", selected = c("linear"), session = session)
      updateCheckboxGroupInput(session = session, inputId = "model_terms_skew_1", selected = character(0))
      
      updateRadioButtons(session = session, inputId = "sign_mean_assoc_1", selected = "Positive")
      updateRadioButtons(session = session, inputId = "mean_assoc_1", selected = 1)
      updateRadioButtons(session = session, inputId = "mean_magn_bias_1", selected = 1)
      updateRadioButtons(session = session, inputId = "mean_assoc_bias_1", selected = 1)
      updateRadioButtons(session = session, inputId = "sign_sd_assoc_1", selected = "Positive")
      updateRadioButtons(session = session, inputId = "sd_magn_1", selected = 1)
      updateRadioButtons(session = session, inputId = "sd_magn_bias_1", selected = 4)
      updateRadioButtons(session = session, inputId = "sd_assoc_1", selected = 2)
      updateRadioButtons(session = session, inputId = "sd_assoc_bias_1", selected = 1)
    }
    if(input$bias_1 == 4){
      
      updateCheckboxGroupInput("model_terms_mean_1", selected = c("linear"), session = session) # check that linear term is requested if people change any input
      updateCheckboxGroupInput("model_terms_sd_1", selected = c("linear"), session = session)
      updateCheckboxGroupInput(session = session, inputId = "model_terms_skew_1", selected = character(0))
      
      updateRadioButtons(session = session, inputId = "sign_mean_assoc_1", selected = "Positive")
      updateRadioButtons(session = session, inputId = "mean_assoc_1", selected = 2)
      updateRadioButtons(session = session, inputId = "mean_magn_bias_1", selected = 2)
      updateRadioButtons(session = session, inputId = "mean_assoc_bias_1", selected = 2)
      updateRadioButtons(session = session, inputId = "sign_sd_assoc_1", selected = "Positive")
      updateRadioButtons(session = session, inputId = "sd_magn_1", selected = 3)
      updateRadioButtons(session = session, inputId = "sd_magn_bias_1", selected = 1)
      updateRadioButtons(session = session, inputId = "sd_assoc_1", selected = 2)
      updateRadioButtons(session = session, inputId = "sd_assoc_bias_1", selected = 2)
      
    }
  }, ignoreInit = T)
  
  
  
  observeEvent(input$reset_bias_1, {
    
    updateRadioGroupButtons(inputId = "bias_1", selected = character(0))
    
    updateRadioButtons(inputId = "bias_1", selected = character(0))
    
    shinyjs::reset("sign_mean_assoc_1")
    shinyjs::reset("mean_assoc_1")
    shinyjs::reset("mean_magn_bias_1")
    shinyjs::reset("mean_assoc_bias_1")
    shinyjs::reset("sign_sd_assoc_1")
    shinyjs::reset("sd_magn_1")
    shinyjs::reset("sd_magn_bias_1")
    shinyjs::reset("sd_assoc_1")
    shinyjs::reset("sd_assoc_bias_1")
    
    shinyjs::reset("model_terms_mean_1")
    shinyjs::reset("model_terms_sd_1")
    shinyjs::reset("model_terms_skew_1")
    
    points$points_mean_1$data <- points$points_mean_1$data[0,]
    points$points_sd_1$data <- points$points_sd_1$data[0,]
    points$points_skew_1$data <- points$points_skew_1$data[0,]
    
    
  }, ignoreInit = T)
  
  
  
  
  observeEvent(input$bias_2, {
    
    if(input$bias_2 == 1){
      
      updateCheckboxGroupInput(session = session, inputId = "model_terms_mean_2", selected = character(0))
      updateCheckboxGroupInput(session = session, inputId = "model_terms_sd_2", selected = character(0))
      updateCheckboxGroupInput(session = session, inputId = "model_terms_skew_2", selected = character(0))
      
      
      updateRadioButtons(session = session, inputId = "sign_mean_assoc_2", selected = "Positive")
      updateRadioButtons(session = session, inputId = "mean_assoc_2", selected = 1)
      updateRadioButtons(session = session, inputId = "mean_magn_bias_2", selected = 3)
      updateRadioButtons(session = session, inputId = "mean_assoc_bias_2", selected = 1)
      updateRadioButtons(session = session, inputId = "sign_sd_assoc_2", selected = "Positive")
      updateRadioButtons(session = session, inputId = "sd_magn_2", selected = 1)
      updateRadioButtons(session = session, inputId = "sd_magn_bias_2", selected = 1)
      updateRadioButtons(session = session, inputId = "sd_assoc_2", selected = 1)
      updateRadioButtons(session = session, inputId = "sd_assoc_bias_2", selected = 1)
    }
    
    if(input$bias_2 == 2){
      
      
      updateCheckboxGroupInput("model_terms_mean_2", selected = c("linear"), session = session) # check that linear term is requested if people change any input
      updateCheckboxGroupInput(session = session, inputId = "model_terms_sd_2", selected = character(0))
      updateCheckboxGroupInput(session = session, inputId = "model_terms_skew_2", selected = character(0))
      
      updateRadioButtons(session = session, inputId = "sign_mean_assoc_2", selected = "Negative")
      updateRadioButtons(session = session, inputId = "mean_assoc_2", selected = 1)
      updateRadioButtons(session = session, inputId = "mean_magn_bias_2", selected = 2)
      updateRadioButtons(session = session, inputId = "mean_assoc_bias_2", selected = 4)
      updateRadioButtons(session = session, inputId = "sign_sd_assoc_2", selected = "Positive")
      updateRadioButtons(session = session, inputId = "sd_magn_2", selected = 2)
      updateRadioButtons(session = session, inputId = "sd_magn_bias_2", selected = 1)
      updateRadioButtons(session = session, inputId = "sd_assoc_2", selected = 1)
      updateRadioButtons(session = session, inputId = "sd_assoc_bias_2", selected = 1)
    }
    
    if(input$bias_2 == 3){
      
      updateCheckboxGroupInput(session = session, inputId = "model_terms_mean_2", selected = character(0))
      updateCheckboxGroupInput("model_terms_sd_2", selected = c("linear"), session = session)
      updateCheckboxGroupInput(session = session, inputId = "model_terms_skew_2", selected = character(0))
      
      
      updateRadioButtons(session = session, inputId = "sign_mean_assoc_2", selected = "Positive")
      updateRadioButtons(session = session, inputId = "mean_assoc_2", selected = 1)
      updateRadioButtons(session = session, inputId = "mean_magn_bias_2", selected = 1)
      updateRadioButtons(session = session, inputId = "mean_assoc_bias_2", selected = 1)
      updateRadioButtons(session = session, inputId = "sign_sd_assoc_2", selected = "Positive")
      updateRadioButtons(session = session, inputId = "sd_magn_2", selected = 1)
      updateRadioButtons(session = session, inputId = "sd_magn_bias_2", selected = 4)
      updateRadioButtons(session = session, inputId = "sd_assoc_2", selected = 2)
      updateRadioButtons(session = session, inputId = "sd_assoc_bias_2", selected = 1)
    }
    
    if(input$bias_2 == 4){
      
      updateCheckboxGroupInput("model_terms_mean_2", selected = c("linear"), session = session) # check that linear term is requested if people change any input
      updateCheckboxGroupInput("model_terms_sd_2", selected = c("linear"), session = session)
      updateCheckboxGroupInput(session = session, inputId = "model_terms_skew_2", selected = character(0))
      
      updateRadioButtons(session = session, inputId = "sign_mean_assoc_2", selected = "Positive")
      updateRadioButtons(session = session, inputId = "mean_assoc_2", selected = 2)
      updateRadioButtons(session = session, inputId = "mean_magn_bias_2", selected = 2)
      updateRadioButtons(session = session, inputId = "mean_assoc_bias_2", selected = 2)
      updateRadioButtons(session = session, inputId = "sign_sd_assoc_2", selected = "Positive")
      updateRadioButtons(session = session, inputId = "sd_magn_2", selected = 3)
      updateRadioButtons(session = session, inputId = "sd_magn_bias_2", selected = 1)
      updateRadioButtons(session = session, inputId = "sd_assoc_2", selected = 2)
      updateRadioButtons(session = session, inputId = "sd_assoc_bias_2", selected = 2)
    }
  }, ignoreInit = T)
  
  
  
  stage_2_resetted <- reactiveVal(FALSE)
  
  observeEvent(input$reset_bias_2, {
    
    updateRadioButtons(inputId = "bias_2", selected = character(0))
    
    shinyjs::reset("sign_mean_assoc_2")
    shinyjs::reset("mean_assoc_2")
    shinyjs::reset("mean_magn_bias_2")
    shinyjs::reset("mean_assoc_bias_2")
    shinyjs::reset("sign_sd_assoc_2")
    shinyjs::reset("sd_magn_2")
    shinyjs::reset("sd_magn_bias_2")
    shinyjs::reset("sd_assoc_2")
    shinyjs::reset("sd_assoc_bias_2")
    
    
    shinyjs::reset("model_terms_mean_2")
    shinyjs::reset("model_terms_sd_2")
    shinyjs::reset("model_terms_skew_2")
    
    points$points_mean_2$data <- points$points_mean_2$data[0,]
    points$points_sd_2$data <- points$points_sd_2$data[0,]
    points$points_skew_2$data <- points$points_skew_2$data[0,]
    
  }, ignoreInit = T)
  
  observeEvent(input$bias_3, {
    
    if(input$bias_3 == 1){
      
      updateCheckboxGroupInput(session = session, inputId = "model_terms_mean_3", selected = character(0))
      updateCheckboxGroupInput(session = session, inputId = "model_terms_sd_3", selected = character(0))
      updateCheckboxGroupInput(session = session, inputId = "model_terms_skew_3", selected = character(0))
      
      updateRadioButtons(session = session, inputId = "sign_mean_assoc_3", selected = "Positive")
      updateRadioButtons(session = session, inputId = "mean_assoc_3", selected = 1)
      updateRadioButtons(session = session, inputId = "mean_magn_bias_3", selected = 3)
      updateRadioButtons(session = session, inputId = "mean_assoc_bias_3", selected = 1)
      updateRadioButtons(session = session, inputId = "sign_sd_assoc_3", selected = "Positive")
      updateRadioButtons(session = session, inputId = "sd_magn_3", selected = 1)
      updateRadioButtons(session = session, inputId = "sd_magn_bias_3", selected = 1)
      updateRadioButtons(session = session, inputId = "sd_assoc_3", selected = 1)
      updateRadioButtons(session = session, inputId = "sd_assoc_bias_3", selected = 1)
    }
    if(input$bias_3 == 2){
      
      updateCheckboxGroupInput("model_terms_mean_3", selected = c("linear"), session = session) # check that linear term is requested if people change any input
      updateCheckboxGroupInput(session = session, inputId = "model_terms_sd_3", selected = character(0))
      updateCheckboxGroupInput(session = session, inputId = "model_terms_skew_3", selected = character(0))
      
      
      updateRadioButtons(session = session, inputId = "sign_mean_assoc_3", selected = "Negative")
      updateRadioButtons(session = session, inputId = "mean_assoc_3", selected = 1)
      updateRadioButtons(session = session, inputId = "mean_magn_bias_3", selected = 2)
      updateRadioButtons(session = session, inputId = "mean_assoc_bias_3", selected = 4)
      updateRadioButtons(session = session, inputId = "sign_sd_assoc_3", selected = "Positive")
      updateRadioButtons(session = session, inputId = "sd_magn_3", selected = 2)
      updateRadioButtons(session = session, inputId = "sd_magn_bias_3", selected = 1)
      updateRadioButtons(session = session, inputId = "sd_assoc_3", selected = 1)
      updateRadioButtons(session = session, inputId = "sd_assoc_bias_3", selected = 1)
    }  
    if(input$bias_3 == 3){
      
      updateCheckboxGroupInput(session = session, inputId = "model_terms_mean_3", selected = character(0))
      updateCheckboxGroupInput("model_terms_sd_3", selected = c("linear"), session = session)
      updateCheckboxGroupInput(session = session, inputId = "model_terms_skew_3", selected = character(0))
      
      updateRadioButtons(session = session, inputId = "sign_mean_assoc_3", selected = "Positive")
      updateRadioButtons(session = session, inputId = "mean_assoc_3", selected = 1)
      updateRadioButtons(session = session, inputId = "mean_magn_bias_3", selected = 1)
      updateRadioButtons(session = session, inputId = "mean_assoc_bias_3", selected = 1)
      updateRadioButtons(session = session, inputId = "sign_sd_assoc_3", selected = "Positive")
      updateRadioButtons(session = session, inputId = "sd_magn_3", selected = 1)
      updateRadioButtons(session = session, inputId = "sd_magn_bias_3", selected = 4)
      updateRadioButtons(session = session, inputId = "sd_assoc_3", selected = 2)
      updateRadioButtons(session = session, inputId = "sd_assoc_bias_3", selected = 1)
    }
    if(input$bias_3 == 4){
      
      updateCheckboxGroupInput("model_terms_mean_3", selected = c("linear"), session = session) # check that linear term is requested if people change any input
      updateCheckboxGroupInput("model_terms_sd_3", selected = c("linear"), session = session)
      updateCheckboxGroupInput(session = session, inputId = "model_terms_skew_3", selected = character(0))
      
      updateRadioButtons(session = session, inputId = "sign_mean_assoc_3", selected = "Positive")
      updateRadioButtons(session = session, inputId = "mean_assoc_3", selected = 2)
      updateRadioButtons(session = session, inputId = "mean_magn_bias_3", selected = 2)
      updateRadioButtons(session = session, inputId = "mean_assoc_bias_3", selected = 2)
      updateRadioButtons(session = session, inputId = "sign_sd_assoc_3", selected = "Positive")
      updateRadioButtons(session = session, inputId = "sd_magn_3", selected = 3)
      updateRadioButtons(session = session, inputId = "sd_magn_bias_3", selected = 1)
      updateRadioButtons(session = session, inputId = "sd_assoc_3", selected = 2)
      updateRadioButtons(session = session, inputId = "sd_assoc_bias_3", selected = 2)
    }
  }, ignoreInit = T)
  
  
  stage_3_resetted <- reactiveVal(FALSE)
  
  observeEvent(input$reset_bias_3, {
    
    updateRadioButtons(inputId = "bias_3", selected = character(0))
    
    shinyjs::reset("sign_mean_assoc_3")
    shinyjs::reset("mean_assoc_3")
    shinyjs::reset("mean_magn_bias_3")
    shinyjs::reset("mean_assoc_bias_3")
    shinyjs::reset("sign_sd_assoc_3")
    shinyjs::reset("sd_magn_3")
    shinyjs::reset("sd_magn_bias_3")
    shinyjs::reset("sd_assoc_3")
    shinyjs::reset("sd_assoc_bias_3")
    
    shinyjs::reset("model_terms_mean_3")
    shinyjs::reset("model_terms_sd_3")
    shinyjs::reset("model_terms_skew_3")
    
    points$points_mean_3$data <- points$points_mean_3$data[0,]
    points$points_sd_3$data <- points$points_sd_3$data[0,]
    points$points_skew_3$data <- points$points_skew_3$data[0,]
    
  }, ignoreInit = T)
  
  # this counter gets increades when any "update" button is clicked. This counter is the only reactive in the plot outputs which is not isolated
  # so whenever any update button is clicked, the counter increases and the plots get updated
  simulate <- reactiveValues(counter = 0)
  observeEvent(c(input$update_plot_prediction_1, input$update_plot_prediction_2, input$update_plot_prediction_3, input$update_results_plots, input$update_costs_plots),{
    simulate$counter <- simulate$counter + 1
  })
  
  
  
  # when simulation is clicked, reset action button
  observeEvent(c(input$update_results_plots, input$update_plot_prediction_1, input$update_plot_prediction_2, input$update_plot_prediction_3, input$update_costs_plots), {
    
    updateActionButton(inputId = "update_results_plots", label = "Plots are up to date")
    updateActionButton(inputId = "update_plot_prediction_1", label = "Plots are up to date")
    updateActionButton(inputId = "update_plot_prediction_2", label = "Plots are up to date")
    updateActionButton(inputId = "update_plot_prediction_3", label = "Plots are up to date")
    updateActionButton(inputId = "update_costs_plots", label = "Plots are up to date")
    
    disable("update_results_plots")
    disable("update_plot_prediction_1")
    disable("update_plot_prediction_2")
    disable("update_plot_prediction_3")
    disable("update_costs_plots")
    
    updateSliderInput(session = session, inputId = "sample_picker", max = input$number_samples)
    
    
  })
  
  
  
  
  
  all_inputs <- reactive({
    list(input$available_funding,
         input$target_grand_volume,
         input$num_applications,
         input$stages,
         input$number_samples,
         
         input$num_reviewers_per_application_1,
         input$num_reviewers_per_application_2,
         input$num_reviewers_per_application_3,
         
         input$mode_1,
         input$mode_2,
         input$mode_3,
         
         input$num_accepted_1,
         input$num_accepted_2,
         input$num_accepted_3,
         
         
         input$cut_off_1,
         input$cut_off_2,
         input$cut_off_3,
         
         input$review_mode_1,
         input$review_mode_2,
         input$review_mode_3,
         
         input$fixcosts_applications_1,
         input$fixcosts_applications_2,
         input$fixcosts_applications_3,
         input$hours_applicant_1,
         input$hours_applicant_2,
         input$hours_applicant_3,
         input$price_hour_applicant_1,
         input$price_hour_applicant_2,
         input$price_hour_applicant_3,
         input$num_applicants_per_application_1,
         input$num_applicants_per_application_2,
         input$num_applicants_per_application_3,
         input$fixcosts_review_1,
         input$fixcosts_review_2,
         input$fixcosts_review_3,
         input$hours_reviewer_1,
         input$hours_reviewer_2,
         input$hours_reviewer_3,
         input$price_hour_reviewer_1,
         input$price_hour_reviewer_2,
         input$price_hour_reviewer_3,
         input$name_adv,
         input$name_disadv,
         input$group_proportions, 
         input$opportunity_factor,
         input$sd_ideas,
         input$skew_ideas,
         coeffs$coefficients_mean_1,
         coeffs$coefficients_mean_2,
         coeffs$coefficients_mean_3,
         coeffs$coefficients_sd_1,
         coeffs$coefficients_sd_2,
         coeffs$coefficients_sd_3,
         coeffs$coefficients_skew_1,
         coeffs$coefficients_skew_2,
         coeffs$coefficients_skew_3
    )
  })
  
  # when any input is changed, change color and text on update buttons
  observeEvent(all_inputs(), {
    
    updateActionButton(inputId = "update_results_plots", label = "Parameters changed, UPDATE")
    updateActionButton(inputId = "update_plot_prediction_1", label = "Parameters changed, UPDATE")
    updateActionButton(inputId = "update_plot_prediction_2", label = "Parameters changed, UPDATE")
    updateActionButton(inputId = "update_plot_prediction_3", label = "Parameters changed, UPDATE")
    updateActionButton(inputId = "update_costs_plots", label = "Parameters changed, UPDATE")
    
    enable("update_results_plots")
    enable("update_plot_prediction_1")
    enable("update_plot_prediction_2")
    enable("update_plot_prediction_3")
    enable("update_costs_plots")
    
    updateRadioGroupButtons(inputId = "scenarios", selected = character(0))
  }, ignoreInit = TRUE)
  
  
  
  
  # initialize the data where we store the points when user clicks a plot in the advanced bias settings
  points <- reactiveValues()
  points$points_mean_1 <- reactiveValues(data = data.frame(x = numeric(), y = numeric(), group = numeric()))
  points$points_sd_1 <- reactiveValues(data = data.frame(x = numeric(), y = numeric(), group = numeric()))
  points$points_skew_1 <- reactiveValues(data = data.frame(x = numeric(), y = numeric(), group = numeric()))
  
  points$points_mean_2 <- reactiveValues(data = data.frame(x = numeric(), y = numeric(), group = numeric()))
  points$points_sd_2 <- reactiveValues(data = data.frame(x = numeric(), y = numeric(), group = numeric()))
  points$points_skew_2 <- reactiveValues(data = data.frame(x = numeric(), y = numeric(), group = numeric()))
  
  points$points_mean_3 <- reactiveValues(data = data.frame(x = numeric(), y = numeric(), group = numeric()))
  points$points_sd_3 <- reactiveValues(data = data.frame(x = numeric(), y = numeric(), group = numeric()))
  points$points_skew_3 <- reactiveValues(data = data.frame(x = numeric(), y = numeric(), group = numeric()))
  
  
  # when user clicks on a plot, add the new point to the data
  observeEvent(input$plot_click_mean_1, {
    points$points_mean_1$data <- rbind(points$points_mean_1$data, data.frame(x = round(input$plot_click_mean_1$x, 0), y = round(input$plot_click_mean_1$y, 1), group = as.numeric(input$group_to_draw_1)))
  })
  
  observeEvent(input$plot_click_sd_1, {
    points$points_sd_1$data <- rbind(points$points_sd_1$data, data.frame(x = round(input$plot_click_sd_1$x, 0), y = round(input$plot_click_sd_1$y, 1), group = as.numeric(input$group_to_draw_1)))
  })
  
  observeEvent(input$plot_click_skew_1, {
    points$points_skew_1$data <- rbind(points$points_skew_1$data, data.frame(x = round(input$plot_click_skew_1$x, 0), y = round(input$plot_click_skew_1$y, 1), group = as.numeric(input$group_to_draw_1)))
  })
  
  
  
  
  observeEvent(input$plot_click_mean_2, {
    points$points_mean_2$data <- rbind(points$points_mean_2$data, data.frame(x = round(input$plot_click_mean_2$x, 0), y = round(input$plot_click_mean_2$y, 1), group = as.numeric(input$group_to_draw_2)))
  })
  
  observeEvent(input$plot_click_sd_2, {
    points$points_sd_2$data <- rbind(points$points_sd_2$data, data.frame(x = round(input$plot_click_sd_2$x, 0), y = round(input$plot_click_sd_2$y, 1), group = as.numeric(input$group_to_draw_2)))
  })
  
  observeEvent(input$plot_click_skew_2, {
    points$points_skew_2$data <- rbind(points$points_skew_2$data, data.frame(x = round(input$plot_click_skew_2$x, 0), y = round(input$plot_click_skew_2$y, 1), group = as.numeric(input$group_to_draw_2)))
  })
  
  
  
  
  observeEvent(input$plot_click_mean_3, {
    points$points_mean_3$data <- rbind(points$points_mean_3$data, data.frame(x = round(input$plot_click_mean_3$x, 0), y = round(input$plot_click_mean_3$y, 1), group = as.numeric(input$group_to_draw_3)))
  })
  
  observeEvent(input$plot_click_sd_3, {
    points$points_sd_3$data <- rbind(points$points_sd_3$data, data.frame(x = round(input$plot_click_sd_3$x, 0), y = round(input$plot_click_sd_3$y, 1), group = as.numeric(input$group_to_draw_3)))
  })
  
  observeEvent(input$plot_click_skew_3, {
    points$points_skew_3$data <- rbind(points$points_skew_3$data, data.frame(x = round(input$plot_click_skew_3$x, 0), y = round(input$plot_click_skew_3$y, 1), group = as.numeric(input$group_to_draw_3)))
  })
  
  
  
  
  
  
  # if there is a double click in a plot, delete the point nearby
  
  observeEvent(input$plot_dbl_click_mean_1, {
    points$points_mean_1$data <- delete_point(points$points_mean_1$data, "group", input$plot_dbl_click_mean_1)
  })
  
  observeEvent(input$plot_dbl_click_sd_1, {
    points$points_sd_1$data <- delete_point(points$points_sd_1$data, "group", input$plot_dbl_click_sd_1)
  })
  
  observeEvent(input$plot_dbl_click_skew_1, {
    points$points_skew_1$data <- delete_point(points$points_skew_1$data, "group", input$plot_dbl_click_skew_1)
  })
  
  observeEvent(input$plot_dbl_click_mean_2, {
    points$points_mean_2$data <- delete_point(points$points_mean_2$data, "group", input$plot_dbl_click_mean_2)
  })
  
  observeEvent(input$plot_dbl_click_sd_2, {
    points$points_sd_2$data <- delete_point(points$points_sd_2$data, "group", input$plot_dbl_click_sd_2)
  })
  
  observeEvent(input$plot_dbl_click_skew_2, {
    points$points_skew_2$data <- delete_point(points$points_skew_2$data, "group", input$plot_dbl_click_skew_2)
  })
  
  observeEvent(input$plot_dbl_click_mean_3, {
    points$points_mean_3$data <- delete_point(points$points_mean_3$data, "group", input$plot_dbl_click_mean_3)
  })
  
  observeEvent(input$plot_dbl_click_sd_3, {
    points$points_sd_3$data <- delete_point(points$points_sd_3$data, "group", input$plot_dbl_click_sd_3)
  })
  
  observeEvent(input$plot_dbl_click_skew_3, {
    points$points_skew_3$data <- delete_point(points$points_skew_3$data, "group", input$plot_dbl_click_skew_3)
  })
  
  
  
  
  
  # here, the "model terms" are saved (linear, squard, cubic)
  model_terms <- reactiveValues()
  
  observeEvent(input$model_terms_mean_1, {
    model_terms$mean_1 <- input$model_terms_mean_1
    
  })
  
  observeEvent(input$model_terms_sd_1, {
    model_terms$sd_1 <- input$model_terms_sd_1
    
  })
  
  
  observeEvent(input$model_terms_skew_1, {
    model_terms$skew_1 <- input$model_terms_skew_1
    
  })
  
  observeEvent(input$model_terms_mean_2, {
    model_terms$mean_2 <- input$model_terms_mean_2
    
  })
  
  observeEvent(input$model_terms_sd_2, {
    model_terms$sd_2 <- input$model_terms_sd_2
    
  })
  
  
  observeEvent(input$model_terms_skew_2, {
    model_terms$skew_2 <- input$model_terms_skew_2
    
  })
  
  observeEvent(input$model_terms_mean_3, {
    model_terms$mean_3 <- input$model_terms_mean_3
    
  })
  
  observeEvent(input$model_terms_sd_3, {
    model_terms$sd_3 <- input$model_terms_sd_3
    
  })
  
  
  observeEvent(input$model_terms_skew_3, {
    model_terms$skew_3 <- input$model_terms_skew_3
    
  })
  
  
  
  
  
  
  
  #these save the inputs from the quick construction of biases (drop down buttons) into reactive expressions
  quick_options_1 <- reactiveValues()
  observeEvent(c(input$sign_mean_assoc_1, input$mean_assoc_1, input$mean_magn_bias_1, input$mean_assoc_bias_1, input$sign_sd_assoc_1, input$sd_magn_1, input$sd_magn_bias_1, input$sd_assoc_1, input$sd_assoc_bias_1),{
    quick_options_1$inputs <- c(input$sign_mean_assoc_1, input$mean_assoc_1, input$mean_magn_bias_1, input$mean_assoc_bias_1, input$sign_sd_assoc_1, input$sd_magn_1, input$sd_magn_bias_1, input$sd_assoc_1, input$sd_assoc_bias_1)
  })
  
  quick_options_2 <- reactiveValues()
  observeEvent(c(input$sign_mean_assoc_2, input$mean_assoc_2, input$mean_magn_bias_2, input$mean_assoc_bias_2, input$sign_sd_assoc_2, input$sd_magn_2, input$sd_magn_bias_2, input$sd_assoc_2, input$sd_assoc_bias_2),{
    quick_options_2$inputs <- c(input$sign_mean_assoc_2, input$mean_assoc_2, input$mean_magn_bias_2, input$mean_assoc_bias_2, input$sign_sd_assoc_2, input$sd_magn_2, input$sd_magn_bias_2, input$sd_assoc_2, input$sd_assoc_bias_2)
  })
  
  quick_options_3 <- reactiveValues()
  observeEvent(c(input$sign_mean_assoc_3, input$mean_assoc_3, input$mean_magn_bias_3, input$mean_assoc_bias_3, input$sign_sd_assoc_3, input$sd_magn_3, input$sd_magn_bias_3, input$sd_assoc_3, input$sd_assoc_bias_3),{
    quick_options_3$inputs <- c(input$sign_mean_assoc_3, input$mean_assoc_3, input$mean_magn_bias_3, input$mean_assoc_bias_3, input$sign_sd_assoc_3, input$sd_magn_3, input$sd_magn_bias_3, input$sd_assoc_3, input$sd_assoc_bias_3)
  })
  
  # if there is a change in one of the quick options by stage, update the model terms, the respective input of the model terms, and mean and sd points
  observeEvent(quick_options_1$inputs, {
    
    if(quick_options_1$inputs[2] > 1 | quick_options_1$inputs[4] > 1){
      model_terms$mean_1 <- "linear"
      
      updateCheckboxGroupInput(paste0("model_terms_mean_1"), selected = c("linear"), session = session) # check that linear term is requested if people change any input
    }
    else if(quick_options_1$inputs[2] == 0 | quick_options_1$inputs[4] == 0){
      updateCheckboxGroupInput(paste0("model_terms_mean_1"), selected = NA, session = session) # check that linear term is requested if people change any input
      model_terms$mean_1 <- NULL
    }
    
    if(quick_options_1$inputs[8] > 1 | quick_options_1$inputs[9] > 1){
      updateCheckboxGroupInput(paste0("model_terms_sd_1"), selected = c("linear"), session = session) # check that linear term is requested if people change any input
      
      model_terms$sd_1 <- "linear"
    }
    else if(quick_options_1$inputs[8] == 0 | quick_options_1$inputs[9] == 0){
      updateCheckboxGroupInput(paste0("model_terms_sd_1"), selected = NA, session = session) # check that linear term is requested if people change any input
      model_terms$sd_1 <- NULL
    }
    
    points$points_mean_1$data <- quick_options_function(quick_options_1$inputs, 1, points$points_mean_1, "mean")  
    points$points_sd_1$data <- quick_options_function(quick_options_1$inputs, 1,  points$points_sd_1, "sd") 
    
  }, ignoreInit = T)
  
  observeEvent(quick_options_2$inputs, {
    
    if(quick_options_2$inputs[2] > 1 | quick_options_2$inputs[4] > 1){
      updateCheckboxGroupInput(paste0("model_terms_mean_2"), selected = c("linear"), session = session) # check that linear term is requested if people change any input
      model_terms$mean_2 <- "linear"
    }
    else if(quick_options_2$inputs[2] == 0 | quick_options_2$inputs[4] == 0){
      updateCheckboxGroupInput(paste0("model_terms_mean_2"), selected = NA, session = session) # check that linear term is requested if people change any input
      model_terms$mean_2 <- NULL
    }
    
    if(quick_options_2$inputs[8] > 1 | quick_options_2$inputs[9] > 1){
      updateCheckboxGroupInput(paste0("model_terms_sd_2"), selected = c("linear"), session = session) # check that linear term is requested if people change any input
      model_terms$sd_2 <- "linear"
    }
    else if(quick_options_2$inputs[8] == 0 | quick_options_2$inputs[9] == 0){
      updateCheckboxGroupInput(paste0("model_terms_sd_2"), selected = NA, session = session) # check that linear term is requested if people change any input
      model_terms$sd_2 <- NULL
      
    }
    
    points$points_mean_2$data <- quick_options_function(quick_options_2$inputs, 2, points$points_mean_2, "mean") #sign_mean_assoc_2(), 
    points$points_sd_2$data <- quick_options_function(quick_options_2$inputs, 2, points$points_sd_2, "sd") #sign_sd_assoc_2(), 
    
  }, ignoreInit = T)
  
  observeEvent(quick_options_3$inputs, {
    
    if(quick_options_3$inputs[2] > 1 | quick_options_3$inputs[4] > 1){
      updateCheckboxGroupInput(paste0("model_terms_mean_3"), selected = c("linear"), session = session) # check that linear term is requested if people change any input
      model_terms$mean_3 <- "linear"
    }
    else if(quick_options_3$inputs[2] == 0 | quick_options_3$inputs[4] == 0){
      updateCheckboxGroupInput(paste0("model_terms_mean_3"), selected = NA, session = session) # check that linear term is requested if people change any input
      model_terms$mean_3 <- NULL
      
    }
    
    if(quick_options_3$inputs[8] > 1 | quick_options_3$inputs[9] > 1){
      updateCheckboxGroupInput(paste0("model_terms_sd_3"), selected = c("linear"), session = session) # check that linear term is requested if people change any input
      model_terms$sd_3 <- "linear"
    }
    else if(quick_options_3$inputs[8] == 0 | quick_options_3$inputs[9] == 0){
      updateCheckboxGroupInput(paste0("model_terms_sd_3"), selected = NA, session = session) # check that linear term is requested if people change any input
      model_terms$sd_3 <- NULL
      
    }
    
    points$points_mean_3$data <- quick_options_function(quick_options_3$inputs, 3,  points$points_mean_3, "mean") #sign_mean_assoc_3(),
    points$points_sd_3$data <- quick_options_function(quick_options_3$inputs, 3, points$points_sd_3, "sd") #sign_sd_assoc_3(), 
    
  }, ignoreInit = T)
  
  
  # inputs here are points in the plots and the chechboxes with requested model terms (linear, squared, cubic)
  mean_1_inputs <- reactive(list(input$model_terms_mean_1, points$points_mean_1$data))
  mean_2_inputs <- reactive(list(input$model_terms_mean_2, points$points_mean_2$data))
  mean_3_inputs <- reactive(list(input$model_terms_mean_3, points$points_mean_3$data))
  
  sd_1_inputs <- reactive(list(input$model_terms_sd_1, points$points_sd_1$data))
  sd_2_inputs <- reactive(list(input$model_terms_sd_2, points$points_sd_2$data))
  sd_3_inputs <- reactive(list(input$model_terms_sd_3, points$points_sd_3$data))
  
  skew_1_inputs <- reactive(list(input$model_terms_skew_1, points$points_skew_1$data))
  skew_2_inputs <- reactive(list(input$model_terms_skew_2, points$points_skew_2$data))
  skew_3_inputs <- reactive(list(input$model_terms_skew_3, points$points_skew_3$data))
  
  coeffs <- reactiveValues()
  
  # if there is a change in inputs, recalculate coefficients, which will change the lines, the bias, and ultimately the selected applications
  observeEvent(mean_1_inputs(), {
    coeffs$coefficients_mean_1 <- coefficients_combined(input$model_terms_mean_1, points$points_mean_1$data, coeff_mean_default[1:2], "mean", 1)
  })
  
  observeEvent(sd_1_inputs(), {
    coeffs$coefficients_sd_1 <- coefficients_combined(input$model_terms_sd_1, points$points_sd_1$data, coeff_sd_default[1:2], "sd", 1)
  })
  
  observeEvent(skew_1_inputs(), {
    coeffs$coefficients_skew_1 <- coefficients_combined(input$model_terms_skew_1, points$points_skew_1$data, coeff_skew_default[1:2], "Skew", 1)
  })
  
  observeEvent(mean_2_inputs(), {
    coeffs$coefficients_mean_2 <- coefficients_combined(input$model_terms_mean_2, points$points_mean_2$data, coeff_mean_default[1:2], "mean", 2)
  })
  
  observeEvent(sd_2_inputs(), {
    coeffs$coefficients_sd_2 <- coefficients_combined(input$model_terms_sd_2, points$points_sd_2$data, coeff_sd_default[1:2], "sd", 2)
  })
  
  observeEvent(skew_2_inputs(), {
    coeffs$coefficients_skew_2 <- coefficients_combined(input$model_terms_skew_2, points$points_skew_2$data, coeff_skew_default[1:2], "Skew", 2)
  })
  
  observeEvent(mean_3_inputs(), {
    coeffs$coefficients_mean_3 <- coefficients_combined(input$model_terms_mean_3, points$points_mean_3$data, coeff_mean_default[1:2], "mean", 3)
  })
  
  observeEvent(sd_3_inputs(), {
    coeffs$coefficients_sd_3 <- coefficients_combined(input$model_terms_sd_3, points$points_sd_3$data, coeff_sd_default[1:2], "sd", 3)
  })
  
  observeEvent(skew_3_inputs(), {
    coeffs$coefficients_skew_3 <- coefficients_combined(input$model_terms_skew_3, points$points_skew_3$data, coeff_skew_default[1:2], "Skew", 3)
  })
  
  # these are the x axes limits for the plots
  curve_data_from_to_1 <- reactive(seq(min(results_stage_1()$idea_qualities), max(results_stage_1()$idea_qualities), 1))
  curve_data_from_to_2 <- reactive(seq(min(results_stage_2()$idea_qualities), max(results_stage_2()$idea_qualities), 1))
  curve_data_from_to_3 <- reactive(seq(min(results_stage_3()$idea_qualities), max(results_stage_3()$idea_qualities), 1))
  
  # this creates data frames with x and y values for each parameter, stage, and group. This will be used for plotting
  curve_data_adv_mean_1 <- reactive(data.frame(x = curve_data_from_to_1(), y = curve_function(coeffs$coefficients_mean_1$values[1:4], curve_data_from_to_1()), parameter = "mean", group = 1, stage = coeffs$coefficients_mean_1$stage[1]))
  curve_data_disadv_mean_1 <- reactive(data.frame(x = curve_data_from_to_1(), y = curve_function(coeffs$coefficients_mean_1$values[5:8], curve_data_from_to_1()), parameter = "mean", group = 0, stage = coeffs$coefficients_mean_1$stage[1]))
  
  curve_data_adv_sd_1 <- reactive(data.frame(x = curve_data_from_to_1(), y = abs(curve_function(coeffs$coefficients_sd_1$values[1:4], curve_data_from_to_1())), parameter = "sd", group = 1, stage = coeffs$coefficients_sd_1$stage[1]))
  curve_data_disadv_sd_1 <- reactive(data.frame(x = curve_data_from_to_1(), y = abs(curve_function(coeffs$coefficients_sd_1$values[5:8], curve_data_from_to_1())), parameter = "sd", group = 0, stage = coeffs$coefficients_sd_1$stage[1]))
  
  curve_data_adv_skew_1 <- reactive(data.frame(x = curve_data_from_to_1(), y = abs(curve_function(coeffs$coefficients_skew_1$values[1:4], curve_data_from_to_1())), parameter = "skew", group = 1, stage = coeffs$coefficients_skew_1$stage[1]))
  curve_data_disadv_skew_1 <- reactive(data.frame(x = curve_data_from_to_1(), y = abs(curve_function(coeffs$coefficients_skew_1$values[5:8], curve_data_from_to_1())), parameter = "skew", group = 0, stage = coeffs$coefficients_skew_1$stage[1]))
  
  curve_data_adv_mean_2 <- reactive(data.frame(x = curve_data_from_to_2(), y = curve_function(coeffs$coefficients_mean_2$values[1:4], curve_data_from_to_2()), parameter = "mean", group = 1, stage = coeffs$coefficients_mean_2$stage[1]))
  curve_data_disadv_mean_2 <- reactive(data.frame(x = curve_data_from_to_2(), y = curve_function(coeffs$coefficients_mean_2$values[5:8], curve_data_from_to_2()), parameter = "mean", group = 0, stage = coeffs$coefficients_mean_2$stage[1]))
  
  curve_data_adv_sd_2 <- reactive(data.frame(x = curve_data_from_to_2(), y = abs(curve_function(coeffs$coefficients_sd_2$values[1:4], curve_data_from_to_2())), parameter = "sd", group = 1, stage = coeffs$coefficients_sd_2$stage[1]))
  curve_data_disadv_sd_2 <- reactive(data.frame(x = curve_data_from_to_2(), y = abs(curve_function(coeffs$coefficients_sd_2$values[5:8], curve_data_from_to_2())), parameter = "sd", group = 0, stage = coeffs$coefficients_sd_2$stage[1]))
  
  curve_data_adv_skew_2 <- reactive(data.frame(x = curve_data_from_to_2(), y = abs(curve_function(coeffs$coefficients_skew_2$values[1:4], curve_data_from_to_2())), parameter = "skew", group = 1, stage = coeffs$coefficients_skew_2$stage[1]))
  curve_data_disadv_skew_2 <- reactive(data.frame(x = curve_data_from_to_2(), y = abs(curve_function(coeffs$coefficients_skew_2$values[5:8], curve_data_from_to_2())), parameter = "skew", group = 0, stage = coeffs$coefficients_skew_2$stage[1]))
  
  curve_data_adv_mean_3 <- reactive(data.frame(x = curve_data_from_to_3(), y = curve_function(coeffs$coefficients_mean_3$values[1:4], curve_data_from_to_3()), parameter = "mean", group = 1, stage = coeffs$coefficients_mean_3$stage[1]))
  curve_data_disadv_mean_3 <- reactive(data.frame(x = curve_data_from_to_3(), y = curve_function(coeffs$coefficients_mean_3$values[5:8], curve_data_from_to_3()), parameter = "mean", group = 0, stage = coeffs$coefficients_mean_3$stage[1]))
  
  curve_data_adv_sd_3 <- reactive(data.frame(x = curve_data_from_to_3(), y = abs(curve_function(coeffs$coefficients_sd_3$values[1:4], curve_data_from_to_3())), parameter = "sd", group = 1, stage = coeffs$coefficients_sd_3$stage[1]))
  curve_data_disadv_sd_3 <- reactive(data.frame(x = curve_data_from_to_3(), y = abs(curve_function(coeffs$coefficients_sd_3$values[5:8], curve_data_from_to_3())), parameter = "sd", group = 0, stage = coeffs$coefficients_sd_3$stage[1]))
  
  curve_data_adv_skew_3 <- reactive(data.frame(x = curve_data_from_to_3(), y = abs(curve_function(coeffs$coefficients_skew_3$values[1:4], curve_data_from_to_3())), parameter = "skew", group = 1, stage = coeffs$coefficients_skew_3$stage[1]))
  curve_data_disadv_skew_3 <- reactive(data.frame(x = curve_data_from_to_3(), y = abs(curve_function(coeffs$coefficients_skew_3$values[5:8], curve_data_from_to_3())), parameter = "skew", group = 0, stage = coeffs$coefficients_skew_3$stage[1]))
  
  
  
  curve_data_combined_1 <- reactive(combine_curve_data(curve_data_adv_mean_1(), 
                                                       curve_data_disadv_mean_1(), 
                                                       curve_data_adv_sd_1(), 
                                                       curve_data_disadv_sd_1(),
                                                       curve_data_adv_skew_1(),
                                                       curve_data_disadv_skew_1()))
  
  curve_data_combined_2 <- reactive(combine_curve_data(curve_data_adv_mean_2(), 
                                                       curve_data_disadv_mean_2(), 
                                                       curve_data_adv_sd_2(), 
                                                       curve_data_disadv_sd_2(),
                                                       curve_data_adv_skew_2(),
                                                       curve_data_disadv_skew_2()))
  
  curve_data_combined_3 <- reactive(combine_curve_data(curve_data_adv_mean_3(), 
                                                       curve_data_disadv_mean_3(), 
                                                       curve_data_adv_sd_3(), 
                                                       curve_data_disadv_sd_3(),
                                                       curve_data_adv_skew_3(),
                                                       curve_data_disadv_skew_3()))
  
  
  
  # quantiles for the bias plots
  curve_data_combined_1_quantiles <- reactive(calculate_quantiles(curve_data_combined_1(), c(.25, .75)))
  curve_data_combined_2_quantiles <- reactive(calculate_quantiles(curve_data_combined_2(), c(.25, .75)))
  curve_data_combined_3_quantiles <- reactive(calculate_quantiles(curve_data_combined_3(), c(.25, .75)))
  
  
  ###### Bias parameter plots ######
  
  #ylims <- reactiveValues(mean_1 = c(-20, 20), sd_1 = c(-1, 20), skew_1 = c(-1, 4), mean_2 = c(-20, 20), sd_2 = c(-1, 20), skew_2 = c(-1, 4), mean_3 = c(-20, 20), sd_3 = c(-1, 20), skew_3 = c(-1, 4) )
  
  
  
  # those are the individual plots in the advances bias settings
  output$plotMean1 <- renderPlot({
    plot_function(points$points_mean_1$data, aes(x, y), aes(shape = as.factor(group), color = as.factor(group)), curve_data_adv_mean_1(), isolate(curve_data_disadv_mean_1()), ylims = c(-30, 30))
  })
  
  output$plotsd1 <- renderPlot({
    plot_function(points$points_sd_1$data, aes(x, y), aes(shape = as.factor(group), color = as.factor(group)), curve_data_adv_sd_1(), isolate(curve_data_disadv_sd_1()), ylims = c(-1,20))
  })
  
  output$plotSkew1 <- renderPlot({
    plot_function(points$points_skew_1$data, aes(x, y), aes(shape = as.factor(group), color = as.factor(group)), curve_data_adv_skew_1(), isolate(curve_data_disadv_skew_1()), ylims = c(-1,4))
  })
  
  
  output$plotMean2 <- renderPlot({
    plot_function(points$points_mean_2$data, aes(x, y), aes(shape = as.factor(group), color = as.factor(group)), curve_data_adv_mean_2(), isolate(curve_data_disadv_mean_2()), ylims = c(-30, 30))
  })
  
  output$plotsd2 <- renderPlot({
    plot_function(points$points_sd_2$data, aes(x, y), aes(shape = as.factor(group), color = as.factor(group)), curve_data_adv_sd_2(), isolate(curve_data_disadv_sd_2()), ylims = c(-1,20))
  })
  
  output$plotSkew2 <- renderPlot({
    plot_function(points$points_skew_2$data, aes(x, y), aes(shape = as.factor(group), color = as.factor(group)), curve_data_adv_skew_2(), isolate(curve_data_disadv_skew_2()), ylims = c(-1,4))
  })
  
  
  output$plotMean3 <- renderPlot({
    plot_function(points$points_mean_3$data, aes(x, y), aes(shape = as.factor(group), color = as.factor(group)), curve_data_adv_mean_3(), isolate(curve_data_disadv_mean_3()), ylims = c(-30, 30))
  })
  
  output$plotsd3 <- renderPlot({
    plot_function(points$points_sd_3$data, aes(x, y), aes(shape = as.factor(group), color = as.factor(group)), curve_data_adv_sd_3(), isolate(curve_data_disadv_sd_3()), ylims = c(-1,20))
  })
  
  output$plotSkew3 <- renderPlot({
    plot_function(points$points_skew_3$data, aes(x, y), aes(shape = as.factor(group), color = as.factor(group)), curve_data_adv_skew_3(), isolate(curve_data_disadv_skew_3()), ylims = c(-1,4))
  })
  
  ###### Bias distribution plots ######
  
  # these are the bias plots without the individual points (the left ones in the app)
  output$plot_stage_1 <- renderPlot({
    data <- curve_data_combined_1_quantiles()
    
    ggplot(data = data, aes(x = x, color = as.factor(group)))+
      geom_vline(xintercept = input$mean_ideas, color = "darkgrey")+
      geom_ribbon(aes(ymin = q1, ymax = q2, fill = as.factor(group), linetype = as.factor(group)), alpha = .2)+
      geom_line(aes(y = mean, linetype = as.factor(group)))+
      coord_cartesian(ylim = c(-30,30), xlim = c(min(isolate(results_stage_1()$idea_qualities)), max(isolate(results_stage_1()$idea_qualities))))+#  c(isolate(input$mean_ideas) - 3*isolate(input$sd_ideas), isolate(input$mean_ideas) + 3*isolate(input$sd_ideas)))+
      scale_linetype_manual(name = "Group", values = c("dashed", "solid"), breaks = c(0,1), labels = group_names_display())+
      scale_fill_manual(name = "Group", values = colors_groups, breaks = c(0,1), labels = group_names_display())+
      scale_color_manual(name = "Group", values = colors_groups, breaks = c(0,1), labels = group_names_display())+
      xlab("Idea Quality")+
      ylab("Reviewer Error Distribution")+
      theme_minimal()+
      theme(
        axis.title.x = element_text(face = "bold", size = 14),
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 14),
        title = element_text(face = "bold", size = 14),
        legend.title = element_text(face="bold", size=12),
        legend.text = element_text(face="bold", size=12),
        legend.position = "top")
    
  })
  
  output$plot_stage_2 <- renderPlot({
    
    req(results_stage_2())
    
    data <- curve_data_combined_2_quantiles()
    
    
    ggplot(data = data, aes(x=x, color = as.factor(group)))+
      geom_vline(xintercept = input$mean_ideas, color = "darkgrey")+
      geom_ribbon(aes(ymin=q1, ymax=q2, fill = as.factor(group), linetype = as.factor(group)), alpha = .2)+
      geom_line(aes(y=mean, linetype = as.factor(group)))+
      coord_cartesian(ylim = c(-30,30), xlim = c(min(isolate(results_stage_2()$idea_qualities)), max(isolate(results_stage_2()$idea_qualities))))+#  c(isolate(input$mean_ideas) - 3*isolate(input$sd_ideas), isolate(input$mean_ideas) + 3*isolate(input$sd_ideas)))+
      scale_linetype_manual(name = "Group", values = c("dashed", "solid"), breaks = c(0,1), labels = group_names_display())+
      scale_fill_manual(name = "Group", values = colors_groups, breaks = c(0,1), labels = group_names_display())+
      scale_color_manual(name = "Group", values = colors_groups, breaks = c(0,1), labels = group_names_display())+
      xlab("Idea Quality")+
      ylab("Reviewer Error Distribution")+
      theme_minimal()+
      theme(
        axis.title.x = element_text(face = "bold", size = 14),
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 14),
        title = element_text(face = "bold", size = 14),
        legend.title = element_text(face="bold", size=12),
        legend.text = element_text(face="bold", size=12),
        legend.position = "top")
    
  })
  
  output$plot_stage_3 <- renderPlot({
    
    req(results_stage_3())
    data <- curve_data_combined_3_quantiles()
    
    
    
    ggplot(data = data, aes(x=x, color = as.factor(group)))+
      geom_vline(xintercept = input$mean_ideas, color = "darkgrey")+
      geom_ribbon(aes(ymin=q1, ymax=q2, fill = as.factor(group), linetype = as.factor(group)), alpha = .2)+
      geom_line(aes(y=mean, linetype = as.factor(group)))+
      coord_cartesian(ylim = c(-30,30), xlim = c(min(isolate(results_stage_3()$idea_qualities)), max(isolate(results_stage_3()$idea_qualities))))+#  c(isolate(input$mean_ideas) - 3*isolate(input$sd_ideas), isolate(input$mean_ideas) + 3*isolate(input$sd_ideas)))+
      scale_linetype_manual(name = "Group", values = c("dashed", "solid"), breaks = c(0,1), labels = group_names_display())+
      scale_fill_manual(name = "Group", values = colors_groups, breaks = c(0,1), labels = group_names_display())+
      scale_color_manual(name = "Group", values = colors_groups, breaks = c(0,1), labels = group_names_display())+
      xlab("Idea Quality")+
      ylab("Reviewer Error Distribution")+
      theme_minimal()+
      theme(
        axis.title.x = element_text(face = "bold", size = 14),
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 14),
        title = element_text(face = "bold", size = 14),
        legend.title = element_text(face="bold", size=12),
        legend.text = element_text(face="bold", size=12),
        legend.position = "top")
    
  })
  
  
  
  
  ###### Sampling of idea qualities ######
  
  # First, only idea qualities are sampled, together with an id
  df_idea_qualities <- reactive({
    data.frame(
      sample_id = rep(seq(1, input$number_samples, 1), each = input$num_applications),
      idea_qualities = (round(rsnorm(input$num_applications * input$number_samples, input$mean_ideas, input$sd_ideas, input$skew_ideas), digits = 0))
    )
  })
  
  # next, each idea quality gets a group indicator. The reason for separating is that the idea distribution does not change when the groups are changed
  df_idea_qualities_with_group <- reactive(
    data.frame(
      sample_id = df_idea_qualities()$sample_id,
      within_sample_id = rep(1:isolate(input$num_applications), isolate(input$number_samples)),
      overall_id = paste(c(rep(seq(1, isolate(input$number_samples), 1), each = isolate(input$num_applications))), rep(1:isolate(input$num_applications), isolate(input$number_samples)), sep = "_"), #concatenate sample_id and within_sample_id
      idea_qualities = df_idea_qualities()$idea_qualities,
      group = as.factor(sample(c(0,1), size = isolate(input$num_applications) * isolate(input$number_samples), replace = T, prob = c(isolate(proportion_disadvantage()), proportion_advantage())))
    )
  ) 
  
  
  
  
  # calculate binwidth of idea quality histogram according to a standard formula
  binwidth_hist = reactive(2 * IQR(df_idea_qualities()[df_idea_qualities()$sample_id == input$sample_picker,]$idea_qualities) / input$num_applications^(1/3))
  
  #plot histogram of idea qualities
  output$idea_distr_plot <- renderPlot({
    ggplot(data = data.frame(df_idea_qualities()[df_idea_qualities()$sample_id == input$sample_picker,]$idea_qualities) ,aes(df_idea_qualities()[df_idea_qualities()$sample_id == input$sample_picker,]$idea_qualities))+
      geom_histogram(aes(y = ..density..),  fill = "#004b5a", color = "white", binwidth = binwidth_hist())+
      geom_function(fun = dsnorm, args = list(mean = isolate(input$mean_ideas), sd = isolate(input$sd_ideas), xi = isolate(input$skew_ideas)), size = 1.3)+
      scale_y_continuous("Density", sec.axis = sec_axis(
        trans = ~ . * binwidth_hist() * input$num_applications, name = "Counts"))+
      theme_minimal()+
      labs(title=paste0("Sample Nr. ", input$sample_picker),
           x ="Idea Quality", y = "Count")+
      theme(plot.title = element_text(face = "bold", size = 16))+
      theme(axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            panel.grid.minor=element_blank())
  })
  
  
  
  ###### Main calculation of winning applications ######
  
  # here, each application gets a rating from reviewers (except for lottery, see corresponding function)
  ratings_stage_1 <- reactive({
    
    results_temp <- assign_application_ratings(data = df_idea_qualities_with_group(), 
                                               stage = 1, 
                                               max_stage = input$stages,
                                               coefficients_disadv = list(coeffs$coefficients_mean_1$values[5:8],
                                                                          coeffs$coefficients_sd_1$values[5:8],
                                                                          coeffs$coefficients_skew_1$values[5:8]), 
                                               coefficients_adv = list(coeffs$coefficients_mean_1$values[1:4],
                                                                       coeffs$coefficients_sd_1$values[1:4],
                                                                       coeffs$coefficients_skew_1$values[1:4]), 
                                               number_reviewer = input$num_reviewers_per_application_1, 
                                               mode = input$mode_1)
    
    return(results_temp)
  })
  
  # here, the ratings are used to decide the winners. Separated from the ratings so that they will not change if the selection procedure is changed
  results_stage_1 <- reactive({
    results_temp <- decide_winners(ratings_stage_1(),
                                   stage = 1,
                                   number_promoted = input$num_accepted_1,
                                   cut_off = input$cut_off_1, 
                                   mode = input$mode_1,
                                   review_mode = input$review_mode_1)
    
    return(results_temp)
    
  })
  
  
  ratings_stage_2 <- reactive({
    
    if(input$stages > 1){
      
      results_temp <- assign_application_ratings(data = results_stage_1() %>% filter(to_next__1 == 1), # take only the ones who advanced from previous stage
                                                 stage = 2, 
                                                 max_stage = input$stages,
                                                 coefficients_disadv = list(coeffs$coefficients_mean_2$values[5:8],
                                                                            coeffs$coefficients_sd_2$values[5:8],
                                                                            coeffs$coefficients_skew_2$values[5:8]), 
                                                 coefficients_adv = list(coeffs$coefficients_mean_2$values[1:4],
                                                                         coeffs$coefficients_sd_2$values[1:4],
                                                                         coeffs$coefficients_skew_2$values[1:4]), 
                                                 number_reviewer = input$num_reviewers_per_application_2,
                                                 mode = input$mode_2)
      return(results_temp)
    } 
  })
  
  results_stage_2 <- reactive({
    if(input$stages > 1){
      
      results_temp <- decide_winners(ratings_stage_2(), 
                                     stage = 2,
                                     number_promoted = input$num_accepted_2,
                                     cut_off = input$cut_off_2, 
                                     mode = input$mode_2,
                                     review_mode = input$review_mode_2)
      
      return(results_temp)
    }
    
  })
  
  
  ratings_stage_3 <- reactive({
    
    
    if(input$stages > 2){
      results_temp <- assign_application_ratings(data = results_stage_2() %>% filter(to_next__2 == 1), 
                                                 stage = 3, 
                                                 max_stage = input$stages,
                                                 coefficients_disadv = list(coeffs$coefficients_mean_3$values[5:8],
                                                                            coeffs$coefficients_sd_3$values[5:8],
                                                                            coeffs$coefficients_skew_3$values[5:8]), 
                                                 coefficients_adv = list(coeffs$coefficients_mean_3$values[1:4],
                                                                         coeffs$coefficients_sd_3$values[1:4],
                                                                         coeffs$coefficients_skew_3$values[1:4]), 
                                                 number_reviewer = input$num_reviewers_per_application_3,
                                                 mode = input$mode_3)
      
      return(results_temp)
    }
  })
  
  
  
  results_stage_3 <- reactive({
    
    if(input$stages > 2){
      results_temp <- decide_winners(ratings_stage_3(),
                                     stage = 3,
                                     number_promoted = input$num_accepted_3,
                                     cut_off = input$cut_off_3, 
                                     mode = input$mode_3, 
                                     review_mode = input$review_mode_3)
      
      
      return(results_temp)
    }
    
  })
  
  
  # take the individual results from before and merge them:
  results <- reactive({
    if(isolate(input$stages) == 1){
      results_temp <- results_stage_1()
    }
    else if(isolate(input$stages) == 2){
      results_list <- list(results_stage_1() %>% lazy_dt(), results_stage_2() %>% lazy_dt() %>% select(-ends_with("__1")))
      results_temp <- results_list %>% 
        reduce(full_join, by = c("sample_id", "within_sample_id", "overall_id", "idea_qualities", "group")) %>% 
        as.tibble()
    }
    else if(isolate(input$stages) == 3){
      results_list <- list(results_stage_1() %>% lazy_dt(), results_stage_2() %>% lazy_dt() %>% select(-ends_with("__1")), results_stage_3() %>% lazy_dt() %>% select(-ends_with("__1"), -ends_with("__2")))
      results_temp <- results_list %>% 
        reduce(full_join, by = c("sample_id", "within_sample_id", "overall_id", "idea_qualities", "group")) %>% 
        as.tibble()
    }
    
    
    
    
    results_temp <- results_temp %>% 
      lazy_dt() %>% 
      mutate("won" := if_else(!!(as.symbol(paste0("to_next__", input$stages))) ==  0 | is.na(!!(as.symbol(paste0("to_next__", input$stages)))), 0, 1)) %>% 
      # This is a work around solution: To plot group proportions and idea qualities of the winning applications, I needed an indicator which applications receive funding.
      # I coded this by calling this variable "won__" followed by the number of stages + 1. When pivoting to long, this creates an additional artificial "stage" which
      # I used for plotting of the qualities and group proportions of winning applications by stage. However later this turned out to be quite bad and slow, so I needed to change
      # the logic and make "won" a variable on application level, not on stage level. I did not have time to redo everything, so I needed to introduce a new variable
      # which takes the role of the old "won__" that everything worked as before.
      mutate(!!(paste0("hack__",  input$stages+1)) := if_else(!!(as.symbol(paste0("to_next__", input$stages))) ==  1, TRUE, NA)) %>% 
      as.tibble()
    
    
    return(results_temp)
  })
  
  ###### Results transformations ######
  
  results_long_by_stage <- reactive({
    results() %>%
      lazy_dt() %>% 
      pivot_longer(c(contains("__")), names_sep = "__", names_to = c("names", "stage"), values_to = "values", values_drop_na = T) %>% 
      pivot_wider(names_from = "names", values_from = "values") %>% 
      select(-hack) %>% 
      mutate(stage = as.factor(stage)) %>% 
      as.tibble()
  })
  
  
  
  # mean idea quality by stage and sample
  results_by_stage_and_sample <- reactive({
    
    df_summarized <- results_long_by_stage() %>% 
      lazy_dt() %>% 
      group_by(sample_id, stage) %>% 
      summarise(mean_idea = mean(idea_qualities, na.rm = T)) %>%  
      arrange(sample_id)  %>% 
      as.tibble()
    
    df_summarized
  })
  
  
  # calculation of total mean
  results_by_stage_across_samples <- reactive({
    if(isolate(input$number_samples) > 1){
      summary_df <- results_by_stage_and_sample() %>%  
        lazy_dt() %>% 
        group_by(stage) %>% 
        summarise(mean_within = mean(mean_idea), sd_within = sd(mean_idea)) %>% 
        as.tibble()#%>%
      
    }
    else{
      summary_df <- results_by_stage_and_sample() %>%  
        lazy_dt() %>% 
        group_by(stage) %>% 
        summarise(mean_within = mean(mean_idea), sd_within = 0) %>% 
        as.tibble() 
      
    }
    summary_df <- as.data.frame(summary_df)
    summary_df
  })
  
  
  
  
  
  ###### Scatterplots observed - true Idea Quality ######
  #(the rightmost plot in each stage in the diversity and quality tab)
  # how idea-quality and reviewed idea-quality relate
  
  output$plot_prediction_stage_1 <- renderPlot({
    
    req(isolate(input$mode_1 != "Lottery")) # so this only when no Lottery
    simulate$counter # when this is increased by clicking any update button, recalculate plot. Everything else is isolated
    
    data <- isolate(results_stage_1()) %>%
      lazy_dt() %>% 
      pivot_longer(c(contains("__")), names_sep = "__", names_to = c("names", "stage"), values_to = "values", values_drop_na = T) %>% 
      pivot_wider(names_from = "names", values_from = "values") %>% 
      mutate(stage = as.factor(stage)) %>% 
      as.tibble()
    
    
    
    ggplot(data = data, aes(y=!!as.symbol(paste0(isolate(input$review_mode_1), "_rating")), x = idea_qualities, color = as.factor(group), shape = as.factor(group)))+
      geom_hline(yintercept = input$mean_ideas, color = "darkgrey")+
      geom_vline(xintercept = input$mean_ideas, color = "darkgrey")+
      geom_point(size = 2)+          
      geom_abline(intercept = 0, slope = 1)+
      ylab("Rated Idea Quality")+
      xlab("Idea Quality")+
      coord_cartesian(ylim = c(min(data[[paste0(isolate(input$review_mode_1), "_rating")]]), max(data[[paste0(isolate(input$review_mode_1), "_rating")]])),  
                      xlim = c(min(data$idea_qualities), max(data$idea_qualities)))+ 
      scale_shape_manual(name = "Group", values = c(16, 17), breaks = c(0,1), labels = group_names_display())+
      scale_color_manual(name = "Group", values = colors_groups, breaks = c(0,1), labels = group_names_display())+            
      theme_minimal()+
      theme(
        axis.title.x = element_text(face = "bold", size = 14),
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 14),
        title = element_text(face = "bold", size = 14),
        legend.title = element_text(face="bold", size=12),
        legend.text = element_text(face="bold", size=12),
        legend.position = "top")
    
  })
  
  output$plot_prediction_stage_2 <- renderPlot({
    
    req(isolate(input$mode_2) != "Lottery")
    simulate$counter
    
    data <- isolate(results_stage_2()) %>%
      lazy_dt() %>% 
      pivot_longer(c(contains("__2")), names_sep = "__", names_to = c("names", "stage"), values_to = "values", values_drop_na = T) %>% 
      pivot_wider(names_from = "names", values_from = "values") %>% 
      mutate(stage = as.factor(stage)) %>% 
      as.tibble()
    
    
    ggplot(data = data, aes(y=!!as.symbol(paste0(isolate(input$review_mode_2), "_rating")), x = idea_qualities, color = as.factor(group), shape = as.factor(group)))+
      geom_hline(yintercept = input$mean_ideas, color = "darkgrey")+
      geom_vline(xintercept = input$mean_ideas, color = "darkgrey")+
      geom_point(size = 2)+          
      geom_abline(intercept = 0, slope = 1)+
      ylab("Rated Idea Quality")+
      xlab("Idea Quality")+
      coord_cartesian(ylim = c(min(data[[paste0(isolate(input$review_mode_2), "_rating")]]), max(data[[paste0(isolate(input$review_mode_2), "_rating")]])),  
                      xlim = c(min(data$idea_qualities), max(data$idea_qualities)))+
      scale_shape_manual(name = "Group", values = c(16, 17), breaks = c(0,1), labels = group_names_display())+
      scale_color_manual(name = "Group", values = colors_groups, breaks = c(0,1), labels = group_names_display())+   
      theme_minimal()+
      theme(
        axis.title.x = element_text(face = "bold", size = 14),
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 14),
        title = element_text(face = "bold", size = 14),
        legend.title = element_text(face="bold", size=12),
        legend.text = element_text(face="bold", size=12),
        legend.position = "top")
    
  })
  
  output$plot_prediction_stage_3 <- renderPlot({
    
    
    req(isolate(input$mode_3) != "Lottery")
    simulate$counter
    
    data <- isolate(results_stage_3()) %>%
      lazy_dt() %>% 
      pivot_longer(c(contains("__3")), names_sep = "__", names_to = c("names", "stage"), values_to = "values", values_drop_na = T) %>% 
      pivot_wider(names_from = "names", values_from = "values") %>% 
      mutate(stage = as.factor(stage)) %>% 
      as.tibble()
    
    
    ggplot(data = data, aes(y=!!as.symbol(paste0(isolate(input$review_mode_3), "_rating")), x = idea_qualities, color = as.factor(group), shape = as.factor(group)))+
      geom_hline(yintercept = input$mean_ideas, color = "darkgrey")+
      geom_vline(xintercept = input$mean_ideas, color = "darkgrey")+
      geom_point(size = 2)+          
      geom_abline(intercept = 0, slope = 1)+
      ylab("Rated Idea Quality")+
      xlab("Idea Quality")+
      coord_cartesian(ylim = c(min(data[[paste0(isolate(input$review_mode_3), "_rating")]]), max(data[[paste0(isolate(input$review_mode_3), "_rating")]])),  
                      xlim = c(min(data$idea_qualities), max(data$idea_qualities)))+
      scale_shape_manual(name = "Group", values = c(16, 17), breaks = c(0,1), labels = group_names_display())+
      scale_color_manual(name = "Group", values = colors_groups, breaks = c(0,1), labels = group_names_display())+   
      theme_minimal()+
      theme(
        axis.title.x = element_text(face = "bold", size = 14),
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 14),
        title = element_text(face = "bold", size = 14),
        legend.title = element_text(face="bold", size=12),
        legend.text = element_text(face="bold", size=12),
        legend.position = "top")
    
  })
  
  
  
  
  
  
  ###### Diversity result plots ######
  # (the bottom most plots)
  output$scatter_quality_by_stage <- renderPlot({
    simulate$counter
    
    current_sample <- isolate(results_long_by_stage()) %>%
      lazy_dt() %>% 
      filter(sample_id == input$sample_picker ) %>% # show only sample indicated by the bottom slider
      as.tibble()
    
    ggplot()+
      geom_point(data = current_sample, mapping = aes(x=as.factor(stage), y=idea_qualities, color=as.factor(group)), size = 2, alpha = .8, position = position_jitter(height = 0, width = 0.3))+
      geom_hline(mapping = aes(yintercept = mean(current_sample[which(current_sample$stage == 1),]$idea_qualities), linetype = "Initial Mean"), color = "black", size = 1)+
      labs(x = "Stage", y = "Idea Quality")+
      scale_x_discrete(labels = c(as.character(seq(1, isolate(input$stages), 1)), 'Won'))+
      scale_color_manual(name="Group", breaks = c(0,1), label= isolate(group_names()), values = colors_groups)+
      ylim(range(isolate(results_long_by_stage()$idea_qualities)))+
      theme_minimal()+
      theme(
        axis.title.x = element_text(face = "bold", size = 14),
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 14),
        legend.title = element_text(face="bold", size=12),
        legend.text = element_text(face="bold", size=12)
      )
    
  })
  
  
  
  output$mean_quality_by_stage <- renderPlot({
    
    
    simulate$counter    
    # This is already sorted in a way that allows to select the current sample by indexing instead of filter
    # it does not look as nice but is faster
    current_sample <- isolate(results_by_stage_and_sample())[(input$sample_picker + (input$sample_picker-1)*(isolate(input$stages))):((input$sample_picker + (input$sample_picker-1)*(isolate(input$stages))) + isolate(input$stages)),]
    
    ggplot(mapping = aes(x = stage))+ 
      geom_line(data = isolate(results_by_stage_and_sample()), aes(y=mean_idea, group = sample_id, color = "Individual simulations"))+
      geom_line(data = current_sample, aes(y=mean_idea, color = "Selected simulation", group = 1), size = 1)+
      geom_line(data = isolate(results_by_stage_across_samples()), mapping = aes(y = mean_within, color = "Grand Mean", group = 1), linetype = 1, size = 2)+
      geom_errorbar(data = isolate(results_by_stage_across_samples()), mapping = aes(y = mean_within, ymin=mean_within-sd_within, ymax=mean_within+sd_within), width=.2)+
      labs(x = "Stage", y = "Idea Quality")+
      scale_x_discrete(labels = c(as.character(seq(1, isolate(input$stages), 1)), 'Won'))+
      scale_color_manual(name="",  label= c("Grand Mean", "Individual simulations", "Selected simulation"), values = c("red", "grey50", "black"))+
      theme_minimal()+
      theme(
        axis.title.x = element_text(face = "bold", size = 14),
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 14),
        legend.title = element_text(face="bold", size=12),
        legend.text = element_text(face="bold", size=12),
        legend.position="top"
      )
    
    
  })
  
  
  
  df_proportions_by_stage_and_sample <- reactive({
    # pipeline to get the relative proportions of disadvantaged group:
    results_long_by_stage() %>% 
      lazy_dt() %>% 
      group_by(sample_id, stage, group) %>% 
      summarize(n = n()) %>% 
      as.data.frame() %>% 
      tidyr::complete(sample_id, stage, group = factor(group, levels = c("0", "1")), fill = list(n = 0)) %>% 
      pivot_wider(names_from = "group", values_from = "n", names_prefix = "group_") %>%
      mutate(n_total = group_0 + group_1) %>%
      pivot_longer(cols = c("group_0", "group_1"), names_to = "group", values_to = "n", names_prefix = "group_") %>%
      filter(group == 0) %>% 
      mutate(proportion = n/n_total) %>% 
      as.tibble()
  })
  
  # proportion mean and sd by stage. Transformation because proportions are bounded by 0 and 1
  df_proportions_by_stage_summary <- reactive({
    df_proportions_by_stage_and_sample() %>% 
      lazy_dt() %>% 
      group_by(stage) %>% 
      summarise(
        Mean = sin(mean(arcsin_transform(proportion)))^2,
        lower_sd = sin(mean(arcsin_transform(proportion)) - sd(arcsin_transform(proportion)))^2,
        upper_sd = sin(mean(arcsin_transform(proportion)) + sd(arcsin_transform(proportion)))^2) %>% 
      as.tibble()
  })
  
  
  output$group_percentage_by_stage <- renderPlot({
    
    simulate$counter
    
    current <- isolate(df_proportions_by_stage_and_sample()) %>% 
      lazy_dt() %>% 
      filter(sample_id == input$sample_picker) %>% 
      as.tibble()
    
    
    ggplot(data = current, aes(x=as.factor(stage), y=proportion, fill = as.factor(group)))+
      geom_bar(stat="identity")+
      scale_fill_manual(name="Group", breaks = 0, label= isolate(group_names()[1]), values = colors_groups[1])+
      scale_x_discrete(labels = c(as.character(seq(1, isolate(input$stages), 1)), 'Won'))+
      lims(y = c(0,1)) +
      labs(title = "Current sample", x = "Stage", y = paste0("Proportion ", isolate(group_names()[1])))+
      theme_minimal()+
      theme(
        axis.title.x = element_text(face = "bold", size = 14),
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 14),
        title = element_text(face = "bold", size = 14),
        legend.position = "none")
    
  })
  
  
  
  
  
  
  output$group_percentage_by_stage_all <- renderPlot({
    
    simulate$counter
    
    isolate(df_proportions_by_stage_summary()) %>% 
      ggplot()+
      geom_bar(aes(x=as.factor(stage), y=Mean, fill = as.factor(0)),  stat="identity")+
      #geom_errorbar(aes(x=as.factor(stage), y= Mean, ymin = lower_sd, ymax = upper_sd))+
      scale_fill_manual(name="Group", breaks = 0, label= isolate(group_names()[1]), values = colors_groups[1])+
      scale_x_discrete(labels = c(as.character(seq(1, isolate(input$stages), 1)), 'Won'))+
      lims(y = c(0,1)) +
      labs(title = "Average All Samples", x = "Stage", y = paste0("Proportion ", isolate(group_names()[1])))+
      theme_minimal()+
      theme(
        axis.title.x = element_text(face = "bold", size = 14),
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 14),
        title = element_text(face = "bold", size = 14),
        legend.position = "none"
        # legend.title = element_text(face="bold", size=12),
        # legend.text = element_text(face="bold", size=12)
      )
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ######## COSTS ##########
  
  
  ###### Cost and work hour calculations ######
  
  parameters_costs <- reactive({
    if(input$stages == 1){
      df <- data.frame(stage = as.factor(input$stages),
                       fixcosts_applications = input$fixcosts_applications_1,
                       hours_applicant = input$hours_applicant_1,
                       price_hour_applicant = input$price_hour_applicant_1,
                       num_applicants_per_application = input$num_applicants_per_application_1,
                       fixcosts_review = input$fixcosts_review_1,
                       hours_reviewer = input$hours_reviewer_1,
                       price_hour_reviewer = input$price_hour_reviewer_1,
                       num_reviewers_per_application = input$num_reviewers_per_application_1)
    }
    if(input$stages == 2){
      df <- data.frame(stage = as.factor(seq(1, input$stages)),
                       fixcosts_applications = c(input$fixcosts_applications_1, input$fixcosts_applications_2),
                       hours_applicant = c(input$hours_applicant_1, input$hours_applicant_2),
                       price_hour_applicant = c(input$price_hour_applicant_1, input$price_hour_applicant_2),
                       num_applicants_per_application = c(input$num_applicants_per_application_1,input$num_applicants_per_application_2),
                       fixcosts_review = c(input$fixcosts_review_1, input$fixcosts_review_2),
                       hours_reviewer = c(input$hours_reviewer_1, input$hours_reviewer_2),
                       price_hour_reviewer = c(input$price_hour_reviewer_1, input$price_hour_reviewer_2),
                       num_reviewers_per_application = c(input$num_reviewers_per_application_1, input$num_reviewers_per_application_2))
      
    }
    if(input$stages == 3){
      df <- data.frame(stage = as.factor(seq(1, input$stages)),
                       fixcosts_applications = c(input$fixcosts_applications_1, input$fixcosts_applications_2, input$fixcosts_applications_3),
                       hours_applicant = c(input$hours_applicant_1, input$hours_applicant_2, input$hours_applicant_3),
                       price_hour_applicant = c(input$price_hour_applicant_1, input$price_hour_applicant_2, input$price_hour_applicant_3),
                       num_applicants_per_application = c(input$num_applicants_per_application_1, input$num_applicants_per_application_2, input$num_applicants_per_application_3),
                       fixcosts_review = c(input$fixcosts_review_1, input$fixcosts_review_2, input$fixcosts_review_3),
                       hours_reviewer = c(input$hours_reviewer_1, input$hours_reviewer_2, input$hours_reviewer_3),
                       price_hour_reviewer = c(input$price_hour_reviewer_1, input$price_hour_reviewer_2, input$price_hour_reviewer_3),
                       num_reviewers_per_application = c(input$num_reviewers_per_application_1, input$num_reviewers_per_application_2, input$num_reviewers_per_application_3))
      
      
    }
    df
  })
  
  # "only won" means that there are no other indicators about the applications in this df. It does not mean that only winning applications are in here
  # naming things is hard
  results_long_by_stage_only_won <- reactive({
    results_long_by_stage() %>% 
      lazy_dt() %>% 
      select("sample_id", "within_sample_id", "overall_id", "group", "stage", "won") %>% 
      filter(as.numeric(stage) <= isolate(input$stages)) %>% 
      as.tibble()
  })
  
  # merge cost to previously defined df
  results_long_by_stage_with_costs <- reactive({
    results_long_by_stage_only_won() %>% 
      lazy_dt() %>% 
      full_join(parameters_costs(), by = "stage") %>% 
      mutate(applicant_hours_per_application = hours_applicant * num_applicants_per_application) %>% 
      mutate(reviewer_hours_per_application = hours_reviewer * num_reviewers_per_application) %>% 
      mutate(applicant_costs = applicant_hours_per_application * price_hour_applicant + fixcosts_applications) %>% 
      mutate(reviewer_costs = reviewer_hours_per_application * price_hour_reviewer + fixcosts_review) %>% 
      mutate(total_costs = applicant_costs + reviewer_costs) %>% 
      as.tibble()
    
  }) 
  
  # summarize the costs and work hours
  results_long_by_stage_with_costs_summarized <- reactive({
    
    df_overall <- results_long_by_stage_with_costs() %>% 
      lazy_dt() %>% 
      group_by(sample_id, stage) %>% 
      summarize(sum_total_costs = sum(total_costs), 
                sum_applicant_hours_per_application = sum(applicant_hours_per_application), 
                sum_reviewer_hours_per_application = sum(reviewer_hours_per_application),
                sum_applicant_costs = sum(applicant_costs),
                sum_reviewer_costs = sum(reviewer_costs)) %>% 
      group_by(stage) %>% 
      summarize(overall__total_costs = mean(sum_total_costs),
                overall__applicant_hours_per_application = mean(sum_applicant_hours_per_application), 
                overall__reviewer_hours_per_application = mean(sum_reviewer_hours_per_application),
                overall__applicant_costs = mean(sum_applicant_costs),
                overall__reviewer_costs = mean(sum_reviewer_costs)) %>% 
      as.tibble()
    
    
    df_per_appl <- results_long_by_stage_with_costs() %>% 
      lazy_dt() %>% 
      group_by(stage) %>% 
      summarize(individual__total_costs = mean(total_costs), 
                individual__applicant_hours_per_application = mean(applicant_hours_per_application), 
                individual__reviewer_hours_per_application = mean(reviewer_hours_per_application),
                individual__applicant_costs = mean(applicant_costs),
                individual__reviewer_costs = mean(reviewer_costs)) %>% 
      as.tibble()
    
    df_combined <- full_join(df_per_appl, df_overall, by = "stage") %>% 
      pivot_longer(cols = -stage, names_to = c("cost_per", "parameter"), values_to = "value", names_sep = "__")
    
    
  })
  
  
  # how many applications are accepted total?
  nr_accepted_applications <- reactive({
    
    results_long_by_stage_only_won() %>%
      lazy_dt() %>%
      filter(won == 1) %>%
      group_by(overall_id) %>%
      summarize(n = n()) %>%
      as.tibble() %>%
      nrow()/isolate(input$number_samples)
  })
  
  output$acc_appl <- renderText({
    nr_accepted_applications()
  })
  
  # how much money is needed in total to fund the winning applications with the requested volume per grand
  output$funding_spent <- renderText({
    scales::comma(nr_accepted_applications()*input$target_grand_volume)
  })
  
  output$total_costs <- renderText({
    costs <- results_long_by_stage_with_costs_summarized() %>% 
      filter(parameter == "total_costs" & cost_per == "overall") %>% 
      summarise(total_costs = sum (value)) %>% 
      deframe()
    
    scales::comma(costs)
    
    # This would be the relative costs for winning applications. Leave that here in case it is of interest
    # number <- results_long_by_stage_with_costs() %>% 
    #   lazy_dt() %>% 
    #   group_by(won, sample_id) %>% 
    #   summarize(sum_total_costs = sum(total_costs)) %>% 
    #   summarize(mean_total_costs = mean(sum_total_costs)) %>% 
    #   mutate(rel_win_lose_costs = mean_total_costs/sum(mean_total_costs))  %>% 
    #   mutate(won = as.factor(won)) %>% 
    #   slice(-1) %>% 
    #   select(rel_win_lose_costs) %>% 
    #   mutate(rel_win_lose_costs = round(rel_win_lose_costs * 100, 1)) %>% 
    #   as.tibble() %>% 
    #   deframe()
    # 
    # paste0(number, " %")
    # scales::comma(nr_accepted_applications()*input$target_grand_volume)
  })
  
  
  # the average idea quality for winning applications
  output$resulting_iq <- renderText({
    scales::comma(results_by_stage_and_sample() %>% 
                    ungroup() %>% 
                    filter(stage == max(as.numeric(stage))) %>% 
                    summarize(mean = mean(mean_idea)) %>% 
                    deframe())
  })
  
  
  
  # final group proportion within winning application
  output$fin_group_proportion <- renderText({
    
    scales::percent(deframe(tail(df_proportions_by_stage_summary()[2], n=1)), accuracy = .1)
    
  })
  
  ###### Cost and Work hour plots ######
  
  # plot work hours split by applicant/reviewer and individual application/all applications
  output$plot_hours_applicants_reviewer <- renderPlot({
    
    input$update_costs_plots
    
    parameter_labels <- c("Applicant", "Reviewer")
    names(parameter_labels) <- c("applicant_hours_per_application", "reviewer_hours_per_application")
    
    isolate(results_long_by_stage_with_costs_summarized()) %>% 
      lazy_dt() %>% 
      filter(grepl("hours", parameter)) %>% 
      as.tibble() %>% 
      
      ggplot(data = ., aes(x=as.factor(1), y = value, fill=as.factor(stage) ))+
      geom_bar(position=position_stack(reverse = TRUE), stat="identity")+ 
      scale_y_continuous(labels = scales::comma) +
      scale_fill_manual(values = colors_stage, name = "Stage")+
      theme_minimal()+
      theme(axis.title.x = element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y = element_text(face="bold", size = 14),
            axis.text.y = element_text(size=12),
            strip.text.x = element_text(size = 12, face = "bold"),
            strip.text.y = element_text(size = 12, face = "bold"))+
      ylab("Work hours")+
      theme(legend.position="bottom")+
      facet_grid(cost_per ~ parameter, scales = "free", 
                 labeller = labeller(parameter = parameter_labels))
    
  })
  
  # the costs split by applicant/reviewer and individual application/all applications
  output$plot_costs_applicants_reviewer <- renderPlot({
    
    input$update_costs_plots
    
    parameter_labels <- c("Applicant", "Reviewer")
    names(parameter_labels) <- c("applicant_costs", "reviewer_costs")
    
    isolate(results_long_by_stage_with_costs_summarized()) %>% 
      lazy_dt() %>% 
      filter(parameter == "applicant_costs" | parameter == "reviewer_costs") %>% 
      as.tibble() %>% 
      
      ggplot(data = ., aes(x=as.factor(1), y = value, fill=as.factor(stage) ))+
      geom_bar(position=position_stack(reverse = TRUE), stat="identity")+ 
      scale_y_continuous(labels = scales::comma) +
      scale_fill_manual(values = colors_stage, name = "Stage")+
      theme_minimal()+
      theme(axis.title.x = element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y = element_text(face="bold", size = 14),
            axis.text.y = element_text(size=12),
            strip.text.x = element_text(size = 12, face = "bold"),
            strip.text.y = element_text(size = 12, face = "bold"))+
      ylab("Costs")+
      theme(legend.position="bottom")+
      facet_grid(cost_per ~ parameter, scales = "free", 
                 labeller = labeller(parameter = parameter_labels))
    
  })
  
  
  
  
  # this plots the bar plot showing costs of the process by stage
  output$total_costs_plot <- renderPlot({
    
    input$update_costs_plots
    
    isolate(results_long_by_stage_with_costs_summarized()) %>% 
      lazy_dt() %>% 
      filter(parameter == "total_costs") %>% 
      filter(cost_per == "overall") %>% 
      as.tibble() %>% 
      
      ggplot(aes(x=as.factor(1), y=value, fill = stage))+
      geom_bar(position=position_stack(reverse = TRUE), stat="identity")+ 
      geom_hline(yintercept = isolate(input$available_funding))+
      geom_text(x=1, y = isolate(input$available_funding) * 1.03, label = "Funding Budget")+
      geom_hline(yintercept = isolate(nr_accepted_applications())*isolate(input$target_grand_volume))+
      geom_text(x=1, y = isolate(nr_accepted_applications())*isolate(input$target_grand_volume) * .93, label = "Budget Needed")+
      scale_y_continuous(labels = scales::comma) +
      scale_fill_manual(values = colors_stage, name = "Stage")+
      theme_minimal()+
      theme(axis.title.x = element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y = element_text(face="bold", size = 14),
            axis.text.y = element_text(size=11))+
      ylab("Cost of process by stage")+
      theme(legend.position="bottom")
    
    
  })
  
  # bar plot showing how much money the winners spent and how much the losers - relative costs 
  output$win_lose_cost_plot <- renderPlot({
    
    input$update_costs_plots
    
    isolate(results_long_by_stage_with_costs()) %>% 
      lazy_dt() %>% 
      group_by(won, sample_id) %>% 
      summarize(sum_total_costs = sum(total_costs)) %>% 
      summarize(mean_total_costs = mean(sum_total_costs)) %>% 
      mutate(rel_win_lose_costs = mean_total_costs/sum(mean_total_costs))  %>% 
      mutate(won = as.factor(won)) %>% 
      as.tibble() %>% 
      
      ggplot(aes(x=as.factor(1), y=rel_win_lose_costs, fill = won))+
      geom_bar(position=position_stack(reverse = FALSE), stat="identity")+ 
      scale_fill_manual(values = colors_winning, name = "", breaks = c(0,1), labels = c("Rejected", "Won"))+
      theme_minimal()+
      theme(axis.title.x = element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y = element_text(face="bold", size = 14),
            axis.text.y = element_text(size=11))+
      ylab("% cost of process by winning/losing")+
      theme(legend.position="bottom")
    
    
  })
  
  
  
  
  
  
  
  
  
  ######## SAVING of parameters for later comparison ##################
  
  input_IDs <- c("scenarios",
                 "available_funding",
                 "target_grand_volume",
                 "num_applications",
                 "stages",
                 "number_samples",
                 "num_reviewers_per_application_1", "num_reviewers_per_application_2", "num_reviewers_per_application_3",
                 "mode_1", "mode_2", "mode_3",
                 "num_accepted_1", "num_accepted_2", "num_accepted_3",
                 
                 "cut_off_1", "cut_off_2", "cut_off_3",
                 "review_mode_1", "review_mode_2", "review_mode_3",
                 "mean_ideas", "sd_ideas", "skew_ideas",
                 "name_disadv", "name_adv",
                 "group_proportions",
                 "opportunity_factor",
                 
                 "fixcosts_applications_1", "fixcosts_applications_2", "fixcosts_applications_3",
                 "hours_applicant_1", "hours_applicant_2", "hours_applicant_3",
                 "price_hour_applicant_1", "price_hour_applicant_2", "price_hour_applicant_3",
                 "num_applicants_per_application_1", "num_applicants_per_application_2", "num_applicants_per_application_3",
                 "fixcosts_review_1", "fixcosts_review_2", "fixcosts_review_3",
                 "hours_reviewer_1", "hours_reviewer_2", "hours_reviewer_3",
                 "price_hour_reviewer_1", "price_hour_reviewer_2", "price_hour_reviewer_3",
                 
                 "bias_1",
                 "sign_mean_assoc_1", "mean_assoc_1", "mean_magn_bias_1", "mean_assoc_bias_1",
                 "sign_sd_assoc_1", "sd_magn_1", "sd_magn_bias_1", "sd_assoc_1", "sd_assoc_bias_1",
                 "model_terms_mean_1", "model_terms_sd_1", "model_terms_skew_1", 
                 
                 
                 "bias_2",
                 "sign_mean_assoc_2", "mean_assoc_2", "mean_magn_bias_2", "mean_assoc_bias_2",
                 "sign_sd_assoc_2", "sd_magn_2", "sd_magn_bias_2", "sd_assoc_2", "sd_assoc_bias_2",
                 "model_terms_mean_2", "model_terms_sd_2", "model_terms_skew_2", 
                 
                 
                 "bias_3",
                 "sign_mean_assoc_3", "mean_assoc_3", "mean_magn_bias_3", "mean_assoc_bias_3",
                 "sign_sd_assoc_3", "sd_magn_3", "sd_magn_bias_3", "sd_assoc_3", "sd_assoc_bias_3",
                 "model_terms_mean_3", "model_terms_sd_3", "model_terms_skew_3")#, #checkboxGroupInput
  
  
  input_types <- c("radioGroup",
                   "autonumeric",
                   "autonumeric",
                   "numeric",
                   "numeric",
                   "numeric",
                   "numeric", "numeric", "numeric",
                   "dropdown", "dropdown", "dropdown",
                   "slider", "slider", "slider",
                   "slider", "slider", "slider",
                   "dropdown", "dropdown", "dropdown",
                   "numeric", "numeric", "numeric",
                   "text", "text",
                   "slider", 
                   "slider",
                   
                   "numeric", "numeric", "numeric",
                   "numeric", "numeric", "numeric",
                   "numeric", "numeric", "numeric",
                   "numeric", "numeric", "numeric",
                   "numeric", "numeric", "numeric",
                   "numeric", "numeric", "numeric",
                   "numeric", "numeric", "numeric",
                   
                   "radioGroup",
                   "radio", "radio", "radio", "radio",   
                   "radio", "radio", "radio", "radio", "radio",
                   "checkboxGroup", "checkboxGroup", "checkboxGroup",
                   
                   "radioGroup",
                   "radio", "radio", "radio", "radio",   
                   "radio", "radio", "radio", "radio", "radio",
                   "checkboxGroup", "checkboxGroup", "checkboxGroup",
                   
                   "radioGroup",
                   "radio", "radio", "radio", "radio",   
                   "radio", "radio", "radio", "radio", "radio",
                   "checkboxGroup", "checkboxGroup", "checkboxGroup"#,
                   
  )
  
  
  # a counter how many saved settings there are and an indicator how many stages the current setting has
  saves <- reactiveValues(number = 1, stages = 0, name = "NONE")
  
  # multiple saving data frames: _costs, _successes, _proportions save the summarized data from all saved scenarios to plot in the comparison plots. 
  # _temp is the current result which will be bound to the other data.
  # saved_parameters, points and coeffs save the settings for the current scenario so that they can be loaded and changed later
  saves$df_saves_costs <- data.frame(ID = factor(), stage = factor(), costs = numeric(), mean_won = numeric(), text_ID = character(), color = character(), stringsAsFactors = FALSE)
  saves$df_saves_costs_temp <- data.frame(ID = factor(), stage = factor(), costs = numeric(), text_ID = character(), color = character(), stringsAsFactors = FALSE)
  saves$mean_sd_successes <- data.frame(ID = factor(), stage = factor(), Mean = numeric(), lower_sd = numeric(), upper_sd = numeric(), text_ID = character())
  saves$mean_sd_successes_temp <- data.frame(ID = factor(), stage = factor(), Mean = numeric(), lower_sd = numeric(), upper_sd = numeric(), text_ID = character())
  saves$proportions <- data.frame(ID = factor(), group = factor(), stage = factor(), estimate = numeric(), lower_sd = numeric(), upper_sd = numeric(), text_ID = character())
  saves$proportions_temp <- data.frame(ID = factor(), group = factor(), stage = factor(), estimate = numeric(), lower_sd = numeric(), upper_sd = numeric(), text_ID = character())
  saves$saved_parameters <- data.frame(IDs = input_IDs, types = input_types, stringsAsFactors = FALSE)
  # saves$points <- list()
  # saves$coeffs <- list()
  
  observeEvent(input$save_current, {
    saves$stages <- isolate(input$stages)
    # name of to saved process. Will be concatenated with the number of already saved processes to avoid duplicate names
    if(isolate(input$textsave) == ""){
      saves$name <- as.character(saves$number)
    } else{
      saves$name <- paste(as.character(saves$number), isolate(input$textsave), sep =": ")
    }
    #COSTS
    # empty the temporary data frame (in case something was saved already)
    saves$df_saves_costs_temp <- saves$df_saves_costs_temp[0,]
    
    total_costs <- results_long_by_stage_with_costs_summarized() %>% 
      filter(parameter == "total_costs" & cost_per == "overall") %>% 
      select("value") %>% 
      pull()
    
    saves$df_saves_costs_temp <- data.frame(ID = as.factor(rep(saves$number, saves$stages)),
                                            stage = as.factor(seq(1, saves$stages, 1)),
                                            costs = total_costs,
                                            text_ID = rep(saves$name, saves$stages),
                                            color = colors_stage[1:saves$stages]
    )
    
    # bind the current values to the ones already saved
    saves$df_saves_costs <- rbind(saves$df_saves_costs, saves$df_saves_costs_temp)
    
    # IDEAS/ Effectiveness of process
    # save mean, SD,
    saves$mean_sd_successes_temp <- saves$mean_sd_successes_temp[0,]
    
    saves$mean_sd_successes_temp <- data.frame(ID = as.factor(rep(saves$number, saves$stages+1)),
                                               stage = isolate(results_by_stage_across_samples()$stage),
                                               Mean = isolate(results_by_stage_across_samples()$mean_within),
                                               lower_sd = isolate(results_by_stage_across_samples()$mean_within) - isolate(results_by_stage_across_samples()$sd_within),
                                               upper_sd = isolate(results_by_stage_across_samples()$mean_within) + isolate(results_by_stage_across_samples()$sd_within),
                                               text_ID = rep(saves$name, saves$stages+1)
    )
    
    saves$mean_sd_successes <- rbind(saves$mean_sd_successes, saves$mean_sd_successes_temp)
    
    
    # GROUP PROPORTIONS
    
    saves$proportions_temp <- saves$proportions_temp[0,]
    
    saves$proportions_temp <- data.frame(ID = as.factor(rep(saves$number, saves$stages+1)),
                                         group = rep(0, saves$stages+1),
                                         stage = isolate(df_proportions_by_stage_summary()$stage),
                                         estimate = isolate(df_proportions_by_stage_summary()$Mean),
                                         lower_sd = isolate(df_proportions_by_stage_summary()$lower_sd),
                                         upper_sd = isolate(df_proportions_by_stage_summary()$upper_sd),
                                         text_ID = rep(saves$name, saves$stages+1)
    )
    
    saves$proportions <- rbind(saves$proportions, saves$proportions_temp)
    
    
    # save current parameter settings in a list
    inputs_vec <- c(mode = list())
    for (i in 1:length(input_IDs)){
      if(!is.null(isolate(input[[saves$saved_parameters$IDs[i]]]))){
        inputs_vec[[i]] <- isolate(input[[saves$saved_parameters$IDs[i]]])
      } else{
        inputs_vec[[i]] <- "character(0)" # necessary because in the skew plots, the model terms are empty, so they will not be saved. 
      }
    }
    #  DISABLED - only needed when also loading settings possible which does not work yet
    # #save drawn points
    # #saves$points[[saves$number]] <- isolate(points) # first try: problem: whenever points get updated, the saved settings get updated as well
    # saves$points[[saves$number]]$points_mean_1$data <- points$points_mean_1$data # this is 
    # saves$points[[saves$number]]$points_mean_2$data <- points$points_mean_2$data
    # saves$points[[saves$number]]$points_mean_3$data <- points$points_mean_3$data
    # saves$points[[saves$number]]$points_sd_1$data <- points$points_sd_1$data
    # saves$points[[saves$number]]$points_sd_2$data <- points$points_sd_2$data
    # saves$points[[saves$number]]$points_sd_3$data <- points$points_sd_3$data
    # saves$points[[saves$number]]$points_skew_1$data <- points$points_skew_1$data
    # saves$points[[saves$number]]$points_skew_2$data <- points$points_skew_2$data
    # saves$points[[saves$number]]$points_skew_3$data <- points$points_skew_3$data
    # 
    # #save coefficients
    # saves$coeffs[[saves$number]] <- coeffs
    
    # save inputs in parameter data frame and name column according to user entry. This creates a data frame with input IDs and input types in first two columns
    # and settings for saved settings in the following columns
    saves$saved_parameters[[saves$number+2]] <- inputs_vec
    colnames(saves$saved_parameters)[saves$number+2] <- saves$name
    
    # update load-dropdown menu with option that was saved
    updateSelectInput(inputId = "load_dropdown", choices = subset(saves$df_saves_costs,!duplicated(saves$df_saves_costs$ID))$text_ID) #unique(saves$df_saves_costs$text_ID)) #subset(saves$df_saves_costs,!duplicated(saves$df_saves_costs$ID))$textID) # saves$df_saves_costs[unique(saves$df_saves_costs$ID)]$text_ID)
    
    # empty the name for current process field
    shinyjs::reset("textsave")
    saves$number <- saves$number + 1
    
  })
  
  
  # DISABLED for now since it does not work properly
  # # Load saved process. Logic is to loop through all inputs and update each input with the saved setting
  # observeEvent(input$load_inputs, {
  #   
  #   req(input$load_dropdown != "You need to save something first")
  #   
  #   for (i in 1:length(input_IDs)){
  #     
  #     if (saves$saved_parameters$types[i] == "numeric"){
  #       updateNumericInput(inputId = saves$saved_parameters$IDs[i], value = as.numeric(saves$saved_parameters[[input$load_dropdown]][i]), session = session)
  #     }
  #     else if (saves$saved_parameters$types[i] == "autonumeric"){
  #       updateAutonumericInput(inputId = saves$saved_parameters$IDs[i], value = as.numeric(saves$saved_parameters[[input$load_dropdown]][i]), session = session)
  #     }
  #     else if(saves$saved_parameters$types[i] == "slider"){
  #       updateSliderInput(inputId = saves$saved_parameters$IDs[i], value = as.numeric(saves$saved_parameters[[input$load_dropdown]][i]), session = session)
  #     }
  #     
  #     else if(saves$saved_parameters$types[i] == "dropdown"){
  #       updateSelectInput(inputId = saves$saved_parameters$IDs[i], selected = as.character(saves$saved_parameters[[input$load_dropdown]][i]), session = session)
  #     }
  #     
  #     else if(saves$saved_parameters$types[i] == "text"){
  #       updateTextInput(inputId = saves$saved_parameters$IDs[i], value = as.character(saves$saved_parameters[[input$load_dropdown]][i]), session = session)
  #     }
  #     
  #     else if(saves$saved_parameters$types[i] == "radio"){
  #       updateRadioButtons(inputId = saves$saved_parameters$IDs[i], selected = as.character(saves$saved_parameters[[input$load_dropdown]][i]), session = session)
  #     }
  #     
  #     else if(saves$saved_parameters$types[i] == "radioGroup"){
  #       updateRadioGroupButtons(inputId = saves$saved_parameters$IDs[i], selected = as.character(saves$saved_parameters[[input$load_dropdown]][i]), session = session)
  #     }
  #     else if(saves$saved_parameters$types[i] == "checkboxGroup"){
  #       updateCheckboxGroupInput(inputId = saves$saved_parameters$IDs[i], selected = as.character(saves$saved_parameters[[input$load_dropdown]][i]), session = session)
  #     }
  #     
  #   }
  #   
  #   #here update the points$points_data and the model coefficients - they need to be updated separately from the inputs above in case the scenario was created using advanced bias settings
  #   
  #   points <- saves$points[[as.numeric(str_extract(input$load_dropdown, "^[0-9]"))]] # only extract the digits from the text_IDs
  #   
  #   coeffs <- saves$coeffs[[as.numeric(str_extract(input$load_dropdown, "^[0-9]"))]]
  # })
  # 
  
  # Delete selected setting
  observeEvent(input$delete_current, {
    
    saves$df_saves_costs <- saves$df_saves_costs %>% filter(text_ID != input$load_dropdown)
    saves$saved_parameters <- saves$saved_parameters %>% select(- input$load_dropdown)
    saves$mean_sd_successes <- saves$mean_sd_successes %>% filter(text_ID != input$load_dropdown)
    saves$proportions <- saves$proportions %>% filter(text_ID != input$load_dropdown)
    
    updateSelectInput(inputId = "load_dropdown", choices = subset(saves$df_saves_costs,!duplicated(saves$df_saves_costs$ID))$text_ID, selected = NULL) 
    
  })
  
  # delete all saved processes and reset
  observeEvent(input$reset_save_df, {
    
    saves$df_saves_costs <- saves$df_saves_costs[0,]
    saves$df_saves_costs_temp <- saves$df_saves_costs_temp[0,]
    saves$number <- 1
    saves$saved_parameters <- saves$saved_parameters[,1:2]
    saves$mean_sd_successes <- saves$mean_sd_successes[0,]
    saves$mean_sd_successes_temp <- saves$mean_sd_successes_temp[0,]
    saves$proportions <- saves$proportions[0,]
    saves$proportions_temp <- saves$proportions_temp[0,]
    
    updateSelectInput(session = session, inputId = "load_dropdown", choices = "You need to save something first", selected = NULL)
    
  })
  
  
  
  # Plot for costs for saved processes
  output$cost_comparison <- renderPlot({
    req(nrow(saves$df_saves_costs)>0)
    ggplot(data = saves$df_saves_costs, aes(x=as.factor(ID), y = costs, fill = as.factor(stage)))+
      geom_bar(position=position_stack(reverse = TRUE), stat="identity")+
      scale_y_continuous(labels = scales::comma) +
      scale_fill_manual(values = saves$df_saves_costs$color, breaks = saves$df_saves_costs$stage, name = "Stage")+
      scale_x_discrete(breaks = subset(saves$df_saves_costs,!duplicated(saves$df_saves_costs$ID))$ID, 
                       labels = subset(saves$df_saves_costs,!duplicated(saves$df_saves_costs$ID))$text_ID)+
      theme_minimal()+
      theme(axis.title.x = element_text(face="bold", size = 14),
            axis.text.x=element_text(size = 12),
            axis.title.y = element_text(face="bold", size = 14),
            axis.text.y = element_text(size=11))+
      ylab("Cost of process by stage")+
      xlab("Name of process")+
      theme(legend.position="bottom")
    
  })
  
  # plot for effectiveness of all saved processes
  output$success_comparison <- renderPlot({
    req(nrow(saves$mean_sd_successes)>0)
    
    saves$mean_sd_successes %>% 
      group_by(ID) %>% 
      filter(stage == max(as.numeric(stage))) %>%
      ggplot(aes(x=as.factor(ID), y = Mean, fill = as.factor("Accepted")))+
      geom_errorbar(aes(ymin = lower_sd, ymax = upper_sd))+
      geom_point()+
      scale_y_continuous(labels = scales::comma) +
      scale_fill_manual(breaks = "Accepted", values = c(isolate(results()$colors), "#000000"), name = "Stage")+
      scale_x_discrete(breaks = subset(saves$mean_sd_successes,!duplicated(saves$mean_sd_successes$ID))$ID, 
                       labels = subset(saves$mean_sd_successes,!duplicated(saves$mean_sd_successes$ID))$text_ID)+
      theme_minimal()+
      theme(axis.title.x = element_text(face="bold", size = 14),
            axis.text.x=element_text(size = 12),
            axis.title.y = element_text(face="bold", size = 14),
            axis.text.y = element_text(size=11))+
      ylab("Mean quality of selected ideas")+
      xlab("Name of process")+
      theme(legend.position="bottom")
    
  })
  
  
  # plot for saved group proportions
  output$proportion_comparison <- renderPlot({
    req(nrow(saves$proportions)>0)
    
    saves$proportions %>% 
      group_by(ID) %>% 
      filter(stage == max(as.numeric(stage))) %>% 
      ggplot(aes(x = as.factor(ID), y = estimate, fill = as.factor(group)))+ #[saves$mean_sd_successes$stage == 3,]
      geom_bar(aes(),  stat="identity")+
      geom_point(aes(shape = "Accepted"))+
      scale_fill_manual(name="Group", breaks = 0, label= isolate(group_names()[1]), values = colors_groups[1])+
      scale_shape_manual(breaks = "Accepted", values = 16, name = "Stage")+
      scale_y_continuous(labels = scales::comma) +
      scale_x_discrete(breaks = subset(saves$proportions,!duplicated(saves$proportions$ID))$ID, 
                       labels = subset(saves$proportions,!duplicated(saves$proportions$ID))$text_ID)+
      theme_minimal()+
      theme(axis.title.x = element_text(face="bold", size = 14),
            axis.text.x=element_text(size = 12),
            axis.title.y = element_text(face="bold", size = 14),
            axis.text.y = element_text(size=11))+
      ylab("Mean group proportion across winners")+
      xlab("Name of process")+
      theme(legend.position="bottom")
    
  })
  
  
  
  
  
  
  
  
  
  
  ####### Downloading Raw Data ######
  
  download_name <- reactiveVal()
  
  observeEvent(input$text_to_save, {
    if (grepl("^[a-zA-Z0-9\\-_ ]*$", input$text_to_save)) {
      file_name <- paste0(input$text_to_save, "_GrantInq_results", ".zip")
      download_name(file_name)
    } else {
      download_name("GrantInq_results.zip")
    }
    
    
    
  })
  
  output$download_error <- renderText(
    if (download_name() == "GrantInq_results.zip"){
      disable("downloadData")
      return("No special characters please!")
    }
    else {
      enable("downloadData")
      return("")
    }
  )
  
  
  output$downloadData <- downloadHandler(
    
    
    filename =  function() { download_name()},
    content = function(fname) {
      tmpdir <- tempdir()
      setwd(tempdir())
      print(tempdir())
      all_filenames <- c("individual_results.csv", "group_proportions.csv", "costs_summarized.csv", "individual_costs.csv", "relative_costs.csv", "mean_quality.csv")
      
      fs <- all_filenames
      write.csv(results() %>% 
                  select(-starts_with("hack")), 
                file = all_filenames[1], sep =",")
      write.csv(df_proportions_by_stage_summary() %>% 
                  select(c("stage", "Mean")), 
                file = all_filenames[2], sep =",")
      write.csv(results_long_by_stage_with_costs_summarized() , file = all_filenames[3], sep =",")
      write.csv(results_long_by_stage_with_costs() , file = all_filenames[4], sep =",")
      write.csv(results_long_by_stage_with_costs() %>% 
                  group_by(won, sample_id) %>% 
                  summarize(sum_total_costs = sum(total_costs)) %>% 
                  summarize(mean_total_costs = mean(sum_total_costs)) %>% 
                  mutate(rel_win_lose_costs = mean_total_costs/sum(mean_total_costs))  %>% 
                  mutate(won = as.factor(won)), 
                file = all_filenames[5], sep =",")
      write.csv(results_by_stage_across_samples() , file = all_filenames[6], sep =",")
      
      print (fs)
      
      zip(zipfile=fname, files=fs)
    },
    contentType = "application/zip"
  )
  
  # 
  # # here I want to implement downloading and uploading of parameter settings
  # output$download_settings <- downloadHandler(
  #   filename = function() {
  #     paste("data-", Sys.Date(), ".csv", sep="")
  #   },
  #   content = function(file) {
  #     write.csv(data, file)
  #   }
  # )
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
