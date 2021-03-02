#' LOMU AR ggplot2 Theme
#' @author Jeremy Holden and Ron Green
#' @param base_size Default font size value of 17
#' @param base_family Default font family is sans
#' @description This theme provides an figure layout that is
#' consistent with the style guidelines for the LOMU Annual Report.
#' @examples
#' require(ggplot2)
#' qplot(1:10, 1:10) + theme_gfs()
#' @export

theme_gfs<-function (base_size=17, base_family='sans') {
  theme_bw(base_size=base_size, base_family=base_family)+
    theme(axis.text.x = element_text(face='bold'))+
    theme(axis.text.y = element_text(face = "bold"))+
    theme(axis.title  = element_text(color="Black", face="bold", size=17))+
    theme(line        = element_line(size=1))+
    theme(legend.text = element_text(size = rel(0.8), face='bold'))+
    theme(legend.title = element_text(size = rel(0.8), face = "bold", hjust = 0))+
    theme(strip.text.x = element_text(size = 14, face='bold'))+
    theme(panel.grid.major =   element_blank())+
    theme(panel.grid.minor =   element_blank())+
    theme(axis.line.y = element_line(colour = "black"),
          axis.line.x = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank())
}

#' LOMU AR ggplot2 Acoustic Map Theme
#' @author Jeremy Holden
#' @description This theme provides an figure layout that is
#' consistent with the style guidelines for the LOMU Annual Report
#' specifically for acoustic density plots on the lake.
#' @param base_size Default font size value of 17
#' @param base_family Default font family is sans
#' @examples
#' require(ggplot2)
#' ggplot(LOschaner, aes(lon, lat)) + geom_path()+ theme_hacmap()
#' @export

theme_hacmap <- function(base_size=17, base_family='sans') {
  theme_bw(base_size=base_size, base_family=base_family) %+replace%
    theme(axis.ticks = element_blank(), axis.text.x = element_blank(),
          axis.text.y = element_blank(), axis.title.x = element_blank(),
          axis.title.y = element_blank(), panel.grid.major =   element_blank(),
          panel.grid.minor =   element_blank()
    )
}

#' Another LOMU AR ggplot2 Theme
#' @author Jeremy Holden and Ron Green
#' @description This theme provides an figure layout that is
#' consistent with the style guidelines for the LOMU Annual Report
#' that slight, but possibly important changes.
#' @param base_size Default font size value of 17
#' @param base_family Default font family is sans
#' @examples
#' require(ggplot2)
#' qplot(1:10, 1:10, geom = "point") + theme_gfs2()
#' @export

theme_gfs2<-function (base_size=17, base_family='sans') {
  theme_bw(base_size=base_size, base_family=base_family)+
    theme(axis.text.x = element_text(face='bold'))+
    theme(axis.text.y = element_text(face = "bold"))+
    theme(axis.title  = element_text(color="Black", face="bold", size=17))+
    theme(line        = element_line(size=1))+
    theme(legend.text = element_text(size = rel(0.8), face='bold'))+
    theme(legend.title = element_text(size = rel(0.8), face = "bold", hjust = 0))+
    theme(strip.text.x = element_text(size = 14, face='bold'))+
    theme(panel.grid.major =   element_blank())+
    theme(panel.grid.minor =   element_blank())+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank())+
    theme(axis.line.y = element_line(),
          axis.line.x=element_line())
}
