library(ggplot2)
library(tidyr)
library(dplyr)
library(ggpubr)
library(gridExtra)
library(grid)
library(rjson)
library(stringr)

# plot counter (for filename)
i <- 0

# dataframe
df <- read.csv(file="./data.csv", sep=',', header=FALSE, quote='"', col.names=c("track", "var", "val"))

formatNumber <- function(x) {
  if (is.integer(x)) x else sprintf("%.2f", x)
}


# min/median/max vars
{
  vars <- list(
   "op_metrics.0.throughput.",
   "total_time_per_shard.",
   "indexing_throttle_time_per_shard.",
   "refresh_time_per_shard.",
   "flush_time_per_shard.",
   "merge_time_per_shard.",
   "merge_throttle_time_per_shard."
  )

  lapply(vars, function(v) {
    # throughput
    tp <- df %>% filter(startsWith(as.character(var), v))

    tpw <- spread(tp, var, val)

    tpw <- tpw[,c(
      "track",
      paste(v, "median", sep=""),
      paste(v, "min", sep=""),
      paste(v, "max", sep="")
    )]
    names(tpw) <- c("track", "median", "min", "max")

    g <- ggplot(tpw, aes_string(
      x = "track",
      y = "median",
      ymin = "min",
      ymax = "max",
      fill = "track"
    )) + geom_boxplot() + geom_crossbar() + ylim(0, NA) + theme(legend.position = "none", axis.title.y=element_blank(), axis.title.x=element_blank()) + ggtitle(paste("var: ", v, sep=""))

    tpw[2:4] <- lapply(tpw[2:4], formatNumber) # (the first column holds the track_name, we want to format all the others)

    tbl <- tableGrob(tpw, rows = NULL)

    #list(label = v, plot = g)
    ggsave(file=paste("plots/plot-", i, ".png", sep=""), plot=g)
    ggsave(file=paste("plots/table-", i, ".png", sep=""), plot=tbl)
    i <<- i+1
  })
}

# percentile vars
{
  vars <- list(
   "op_metrics.0.latency.",
   "op_metrics.0.service_time.",
   "op_metrics.0.processing_time."
  )

  lapply(vars, function(v) {
    # throughput
    tp <- df %>% filter(startsWith(as.character(var), v))

    tpw <- spread(tp, var, val)

    tpw <- tpw[,c(
      "track",
      paste(v, "50_0", sep=""),
      paste(v, "99_0", sep=""),
      paste(v, "99_9", sep=""),
      paste(v, "99_99", sep=""),
      paste(v, "100_0", sep="")
   )]
   names(tpw) <- c("track", "p50_0", "p99_0", "p99_9", "p99_99", "p100_0")

    g <- (
        ggplot(tpw, aes_string(
          x = "track",
          y = "p50_0"
        ))
      + geom_segment(aes_string(
          x = "track",
          y = "p50_0",
          xend = "track",
          yend = "p100_0",
          size = 2
        ), colour= "#FFCCCCFF")
      + geom_segment(aes_string(
          x = "track",
          y = "p50_0",
          xend = "track",
          yend = "p99_99",
          size = 2
        ), colour= "#FFAAAAFF")
      + geom_segment(aes_string(
          x = "track",
          y = "p50_0",
          xend = "track",
          yend = "p99_9",
          size = 2
        ), colour= "#FF8888FF")
      + geom_segment(aes_string(
          x = "track",
          y = "p50_0",
          xend = "track",
          yend = "p99_0",
          size = 2
        ), colour= "#FF0000FF")
      + geom_point(size = 2, shape = 18, colour= "#000000FF")
      + geom_point(aes_string(
          x = "track",
          y = "p100_0"
        ), size = 2, shape =18, colour= "#000000FF")
      + geom_point(aes_string(
          x = "track",
          y = "p99_99"
        ), size = 2, shape =18, colour= "#000000FF")
      + geom_point(aes_string(
          x = "track",
          y = "p99_9"
        ), size = 2, shape =18, colour= "#000000FF")
      + geom_point(aes_string(
          x = "track",
          y = "p99_0"
        ), size = 2, shape =18, colour= "#000000FF")
      + ylim(0, NA)
      + ggtitle(paste("var: ", v, sep=""))
    )

    tpw[2:6] <- lapply(tpw[2:6], formatNumber) # (the first column holds the track_name, we want to format all the others)

    tbl <- tableGrob(tpw, rows = NULL)

    #combined <- grid.arrange(g, tbl, nrow=2)

    #list(label = v, plot = g)
    ggsave(file=paste("plots/plot-", i, ".png", sep=""), plot=g)
    ggsave(file=paste("plots/table-", i, ".png", sep=""), plot=tbl)
    i <<- i+1
  })
}

# single value vars
{
  vars <- list(
   "op_metrics.0.error_rate",
   "total_time",
   "merge_time",
   "merge_count",
   "refresh_time",
   "refresh_count",
   "flush_time",
   "flush_count",
   "merge_throttle_time",
   "young_gc_time",
   "old_gc_time",
   "memory_doc_values",
   "memory_terms",
   "memory_norms",
   "memory_points",
   "memory_segments",
   "memory_stored_fields",
   "store_size",
   "translog_size",
   "segment_count"
  )

  lapply(vars, function(v) {
    tp <- df %>% filter(as.character(var) == v)

    tpw <- spread(tp, var, val)

    tpw <- tpw[,c(
      "track",
      v
    )]
    names(tpw) <- c("track", "value")

    g <- (
      ggplot(tpw, aes_string(
        x = "track",
        y = "value",
        fill = "track"
      ))
      + geom_bar(stat = "identity")
      + ylim(0, NA)
      + ggtitle(paste("var: ", v, sep=""))
    )

    tpw[2] <- lapply(tpw[2], formatNumber) # (the first column holds the track_name, we want to format all the others)

    tbl <- tableGrob(tpw, rows = NULL)

    #list(label = v, plot = g)
    ggsave(file=paste("plots/plot-", i, ".png", sep=""), plot=g)
    ggsave(file=paste("plots/table-", i, ".png", sep=""), plot=tbl)
    i <<- i+1
  })
}

jsonPlots <- toJSON(lapply(0:(i-1), function(x) list(
  plot = paste("plots/plot-", x, ".png", sep=""),
  table = paste("plots/table-", x, ".png", sep="")
)))

#write(jsonPlots, file = "./plots.json")

template <- readChar("./index.tpl.html",nchars=1e6)
indexHtml <- str_replace(template, fixed("{{ ./plots.json }}"), jsonPlots)
write(indexHtml, file = "./index.html")

#do.call(ggarrange, c(
#  #lapply(plots, function(x) { x[2] }),
#  plots,
#  list(
#    common.legend = TRUE,
#    legend = "bottom"
#    # labels = lapply(plots, function(x) x["label"])
#  )
#))
#

##dev.off()
#ggsave("plot.png")
