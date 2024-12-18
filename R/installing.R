
# دالة لتثبيت الحزم اذا كانت غير المثبتة
install_packages <- function(packages) {

  # الحزم غير المثبتة
  new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]

  # اذا كان هناك حزم غير مثبتة
  if (length(new_packages)) {

    print("Installing...")

    # تثبيت الحزم واعتمادياتها
    install.packages(new_packages, dependencies = TRUE)
  }

}

# تثبيت الحزم المطلوبة وغير المثبتة
install_packages(

  # قائمة الحزم المطلوبة
  c("shiny","bslib","ggplot2","dplyr","scales","plotly","scales", "arules", "arulesViz")


)
