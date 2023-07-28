require( googlesheets4 )
require(dplyr)
require(magrittr)
require(ggplot2)
require(cowplot)

if(dir.exists("output")!=TRUE) dir.create("output") # check if output directory out exists, if false create

file <- "https://docs.google.com/spreadsheets/d/1-w62GXvKwQ868dwiMVzgPdLYbgSkXxSqasE-YoaGOUc/edit#gid=0"

gs4_deauth() # not a private sheet, so no need for authentication
dat <- as.data.frame(read_sheet(file)) %>%  ### INPUT DATA from googlesheet 
         filter(!is.na(measurer))			# remove spacer rows

dat %<>% mutate_at(c("measurer", "session", "jr"), as.factor) 		  

repeatability <- dat %>% filter(measurer !="JR") %>%
				filter(session==1 | session==2)
datmb <- dat %>% filter(measurer =="DG" & jr!=323 & (session==1 | session==2))

mod <- with(datmb, summary(aov(lm( humerus ~ jr ))))

s2_within <- ms_within <- mod[[1]][2,3]
s2_within

ms_among <- mod[[1]][1,3]
s2_among <- (ms_among-ms_within)/2
ME <- s2_within/(s2_within+s2_among) * 100
ME

## Plot data

p <- dat %>% ggplot(aes(svl, femur, color=measurer, label=jr))

make_plot <- function(p) {
  q1 <- p + geom_point(size = 3) + 
	geom_smooth( aes(group=measurer), method="lm", alpha=.1) + 	
	geom_text(nudge_y = .15) +
	theme_bw()
	
  q2 <- p + geom_point(size = 3) + 
	geom_smooth( method="lm", alpha=.1) + 	
	geom_text(nudge_y = .15) +
	facet_grid( measurer ~ . ) +
	theme_bw()

  plot_grid(q1, q2, labels="AUTO")	
}

femp <- dat %>% ggplot(aes(svl, femur, color=measurer, label=jr))
tibp <- dat %>% ggplot(aes(svl, tibiofibula, color=measurer, label=jr))
tarp <- dat %>% ggplot(aes(svl, tarsus, color=measurer, label=jr))
footp <- dat %>% ggplot(aes(svl, foot, color=measurer, label=jr))
hwp <- dat %>% ggplot(aes(svl, headW, color=measurer, label=jr))
hlp <- dat %>% ggplot(aes(svl, headL, color=measurer, label=jr))
hump <- dat %>% ggplot(aes(svl, humerus, color=measurer, label=jr))
radp <- dat %>% ggplot(aes(svl, radioulna, color=measurer, label=jr))
handp <- dat %>% ggplot(aes(svl, hand, color=measurer, label=jr))


make_plot( femp )
make_plot( tibp )
make_plot( tarp )
make_plot( footp )
make_plot( hwp )
make_plot( hlp )
make_plot( hump )
make_plot( radp )
make_plot( handp )
