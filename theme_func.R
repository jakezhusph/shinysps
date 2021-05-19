## DIY the theme using the functions from library(dashboardthemes)
theme_jqz <- shinyDashboardThemeDIY(
  ### general
  appFontFamily = "Arial"
  ,appFontColor = "rgb(45,45,45)"
  ,primaryFontColor = "rgb(15,15,15)"
  ,infoFontColor = "rgb(15,15,15)"
  # ,successFontColor = "rgb(15,15,15)"
   ,successFontColor = "rgb(255,255,255)"
  ,warningFontColor = "rgb(255,255,255)"
  # ,warningFontColor = "rgb(45,45,45)"
  ,dangerFontColor = "rgb(15,15,15)"
  ,bodyBackColor = "rgb(255,255,255)" ## background of the output

  ### header
  ,logoBackColor = "rgb(255,203,5)"
  ,headerButtonBackColor = "rgb(255,203,5)"
  ,headerButtonIconColor = "rgb(220,220,220)"
  ,headerButtonBackColorHover = "rgb(255,203,5)"
  ,headerButtonIconColorHover = "rgb(255,203,5)"
  ,headerBackColor ="rgb(255,203,5)"
  ,headerBoxShadowColor = "rgb(255,203,5)"
  ,headerBoxShadowSize = "0px 0px 0px"

  ### sidebar
  ,sidebarBackColor = "rgb(240,240,240)"
  # ,sidebarBackColor = "rgb(255,240,240)"
  ,sidebarPadding = 10

  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 0
  ,sidebarMenuBorderRadius = 0

  ,sidebarShadowRadius = "0px 0px 0px"
  ,sidebarShadowColor = "#dfdfdf"
  ,sidebarUserTextColor = "rgb(115,115,115)"
  ,sidebarSearchBackColor = "rgb(240,240,240)"
  ,sidebarSearchIconColor = "rgb(100,100,100)"
  ,sidebarSearchBorderColor = "rgb(220,220,220)"

  ,sidebarTabTextColor = "rgb(100,100,100)"
  ,sidebarTabTextSize = 14
  ,sidebarTabBorderStyle = "none"
  ,sidebarTabBorderColor = "none"
  ,sidebarTabBorderWidth = 0

  ,sidebarTabBackColorSelected = "rgb(230,230,230)"
  ,sidebarTabTextColorSelected = "rgb(0,0,0)"
  ,sidebarTabRadiusSelected = "0px"

  ,sidebarTabBackColorHover = "rgb(245,245,245)"
  ,sidebarTabTextColorHover = "rgb(0,0,0)"
  ,sidebarTabBorderStyleHover = "none solid none none"
  ,sidebarTabBorderColorHover = "rgb(200,200,200)"
  ,sidebarTabBorderWidthHover = 4
  ,sidebarTabRadiusHover = "0px"


  # ,boxBackColor = "rgb(248,248,248)"
  ,boxBackColor = "rgb(255,255,255)"
  ,boxBorderRadius = 5
  ,boxShadowSize = "none"
  ,boxShadowColor = ""
  ,boxTitleSize = 18
  ,boxDefaultColor = "rgb(225,225,225)"
  ,boxPrimaryColor = "rgb(95,155,213)"
  ,boxInfoColor = "rgb(180,180,180)"
  ,boxSuccessColor = "rgb(112,173,71)"
  # ,boxWarningColor = "rgb(237,125,49)"
  ,boxWarningColor = "rgb(0,39,76)"
  ,boxDangerColor = "rgb(232,76,34)"

  ,tabBoxTabColor = "rgb(248,248,248)"
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "rgb(100,100,100)"
  ,tabBoxTabTextColorSelected = "rgb(45,45,45)"
  ,tabBoxBackColor = "rgb(248,248,248)"
  ,tabBoxHighlightColor = "rgb(200,200,200)"
  ,tabBoxBorderRadius = 1

  ### inputs
  ,buttonBackColor = "rgb(215,215,215)"
  ,buttonTextColor = "rgb(45,45,45)"
  ,buttonBorderColor = "rgb(150,150,150)"
  ,buttonBorderRadius = 5

  ,buttonBackColorHover = "rgb(190,190,190)"
  ,buttonTextColorHover = "rgb(0,0,0)"
  ,buttonBorderColorHover = "rgb(150,150,150)"

  ,textboxBackColor = "rgb(255,255,255)"
  ,textboxBorderColor = "rgb(118,118,118)"
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "rgb(245,245,245)"
  ,textboxBorderColorSelect = "rgb(108,108,108)"

  ### tables
  # ,tableBackColor = "rgb(248,248,248)"
  ,tableBackColor = "rgb(255,255,255)"
  ,tableBorderColor = "rgb(238,238,238)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
)


## DIY the logos
customLogo <- shinyDashboardLogoDIY(
  boldText = "shinysps",
  mainText = "",
  textSize = 24,
  badgeText = "v1.0",
  badgeTextColor = "white",
  badgeTextSize = 3,
  badgeBackColor = "#40E0D0",
  badgeBorderRadius = 4
)
