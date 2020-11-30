###################
# sidebar.R
# 
# Create the sidebar menu options for the ui.
###################
sidebar <- dashboardSidebar(
  sidebarMenu(

    menuItem("Search", tabName = "search", icon = icon("search")),
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
    
  )
)
