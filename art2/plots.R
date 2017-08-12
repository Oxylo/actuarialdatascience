# ==========
# plots.R
#
# Code to spupport article at www.actuarialdatascience.com 
# (Should retirement planning start at birth)
#
# Objective: generating plots
# ========== 

library(ggplot2)
library(ggthemes)
library(ggrepel)
library(scales)
source("pension_accrual.R")

# data
anne.deposits <- rep(12 * 200, 70)
anne.timescale <- 1:70
brian.deposits <- rep(12 * 280, 50)
brian.timescale <- 21:70
charlie.deposits <- rep(12 * 700, 20)
charlie.timescale <- 51:70

# long format
df <- as.data.frame(x = list(
  scheme=c(rep("Scheme A (Anne)", length(anne.deposits)), rep("Scheme B (Brian)", length(brian.deposits)),
           rep("Scheme C (Charlie)", length(charlie.deposits))),
  timescale=c(anne.timescale, brian.timescale, charlie.timescale),
  deposits=c(anne.deposits, brian.deposits, charlie.deposits)
  ))

# Global settings axis and colors (the Economist)
COLOR <- "#337AB7"
BACKGROUND.COLOR <- "#EBF3F9"
ANNE.COLOR <- "#ED1B23"
BRIAN.COLOR <- "#00526D"
CHARLIE.COLOR <- "#00B0C8"
VERBOSE <- TRUE
MAXX <- 75
MAXY <- 600000

# Plot Nr 1
PLOT.JPG <- "plot1-no-return.jpg"
RATE <- 0.00
main.title <- "1. Anne, Brian and Charlie each invest the same amount…"
sub.title <- "no return"
ANNE.LABEL <- c(x=25, y=80000)
BRIAN.LABEL <- c(x=50, y=80000)
CHARLIE.LABEL <- c(x=65, y=80000)


# Plot Nr 2
PLOT.JPG <- "plot2-3pct-fixed-return.jpg" 
RATE <- 0.03
main.title <- "2. … but Anne’s long time horizon works in her favor… "
sub.title <- "real return rate 3% per annum"
ANNE.LABEL <- c(x=30, y=150000)
BRIAN.LABEL <- c(x=50, y=110000)
CHARLIE.LABEL <- c(x=65, y=80000)

# Plot Nr 3
PLOT.JPG <- "plot3-market-crash.jpg"
RATE <- function(t) {1.03^(min(67, t)) * (1-0.4)^(min(1, max(0, t-67))) * 1.03^(min(2, max(0, t-68)))}
main.title <- "3. … with retirement in sight a stock market crash may happen …" 
sub.title <- "-40% return 3 years before retirement"
ANNE.LABEL <- c(x=30, y=150000)
BRIAN.LABEL <- c(x=50, y=110000)
CHARLIE.LABEL <- c(x=65, y=80000)

# Plot Nr 4
PLOT.JPG <- "plot4-lifecycle.jpg"
RATE <- function(t) {1.03^(min(55, t)) * (1.02)^(min(10, max(0, t-55))) * 1.01^(min(5, max(0, t-65)))}
main.title <- "4. … so better to reduce risks as retirement draws near." 
sub.title <- "3% - 2% - 1% lifecycle investments"
ANNE.LABEL <- c(x=30, y=150000)
BRIAN.LABEL <- c(x=50, y=110000)
CHARLIE.LABEL <- c(x=65, y=80000)

# calculate future values for each individual
accrual.anne  <- fv(deposits = anne.deposits, timescale = anne.timescale, endpoint=70, rate = RATE, verbose = VERBOSE, breaks=TRUE)
accrual.brian <- fv(deposits = brian.deposits, timescale = brian.timescale, endpoint=70, rate = RATE, verbose = VERBOSE, breaks=TRUE)
accrual.charlie <- fv(deposits = charlie.deposits, timescale = charlie.timescale, endpoint=70, rate = RATE, verbose = VERBOSE, breaks=TRUE)

# long format
df <- as.data.frame(x = list(
  scheme=c(rep("Scheme A (Anne)", nrow(accrual.anne)), 
           rep("Scheme B (Brian)", nrow(accrual.brian)),
           rep("Scheme C (Charlie)", nrow(accrual.charlie))),
  timescale=c(accrual.anne$t, accrual.brian$t, accrual.charlie$t),
  balance=c(accrual.anne$balance, accrual.brian$balance, accrual.charlie$balance)
))

# get label postions
rnames <- rep(NA, 3)
counter <- 1
for (s in c("Scheme A (Anne)", "Scheme B (Brian)", "Scheme C (Charlie)")) {
  rnames[counter] <- as.numeric(rownames(tail(subset(df, scheme == s), 1)))
  counter <- counter + 1
}


plt <- ggplot(df, aes(x=timescale, y=balance, color=scheme, 
                      label=sprintf('$ %s', format(round(balance,0), nsmall=0, big.mark=",")))) +
  theme_economist() +
  scale_color_manual(values=c(ANNE.COLOR, BRIAN.COLOR, CHARLIE.COLOR)) +
  scale_fill_manual(values=c(ANNE.COLOR, BRIAN.COLOR, CHARLIE.COLOR)) +
  theme(plot.title = element_text(size=16, hjust = 0.5, colour=COLOR), 
        plot.subtitle = element_text(size=14, hjust = 0.5, face="italic", colour = COLOR),
        plot.background = element_rect(fill=BACKGROUND.COLOR),
        panel.background = element_rect(fill=BACKGROUND.COLOR),
        axis.text = element_text(colour=COLOR),
        axis.line.x = element_line(colour=COLOR),
        axis.ticks = element_line(colour=COLOR),
        axis.title.x = element_text(colour=COLOR),
        axis.title.y = element_text(colour=COLOR),
        legend.position = "none") +
  labs(x="age (years)", y="balance (US$)", title=main.title, subtitle=sub.title) +
  geom_line(size=1) + 
  scale_x_continuous(expand = c(0, 0), breaks=10*seq(0,7), limits=c(0, MAXX)) +
  scale_y_continuous(expand = c(0, 0), limits=c(0, MAXY), labels=comma) +
  
  annotate("text", x=ANNE.LABEL['x'], y=ANNE.LABEL['y'], label="Anne", color=ANNE.COLOR, fontface="bold") +
  annotate("text", x=BRIAN.LABEL['x'], y=BRIAN.LABEL['y'], label="Brian", color=BRIAN.COLOR, fontface="bold") +
  annotate("text", x=CHARLIE.LABEL['x'], y=CHARLIE.LABEL['y'], label="Charlie", color=CHARLIE.COLOR, fontface="bold") +
  geom_label(data=subset(df, rownames(df) %in% rnames),
                        aes(fill = scheme), colour = "white", fontface = "bold", vjust=-0.2)
  
# export to jpg
jpeg(PLOT.JPG, width = 2400, height = 1800, units = "px", res=300)
plt     
dev.off()


