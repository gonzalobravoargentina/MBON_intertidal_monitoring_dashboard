#install.packages("rsconnect")

rsconnect::setAccountInfo(name='mbon-poletopole',
                          token='EF7EADADAA94DFFA45288B90EA006B21',
                          secret='ZJdMtH9hpoWpO8KKHZuB9eqKcY2X7E+41FSfz5wb')

rsconnect::deployApp(
  appDir = ".",
  appPrimaryDoc = "shiny_mbon_con_indice.rmd",
  appName = "monitoreo-intermareal"
)
