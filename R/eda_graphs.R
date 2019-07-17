#' @title Automates Exploratory Data Analysis Process for a Data
#'
#' @description The process of exploration might become especially when we are dealing with data sets that contains a large number of variables. This package helps to reduce the work and generate plots for numerical and categorical variables.
#'
#' @param data Data frame object
#'
#' @param var Index of variables for which EDA plots are required. Default is All.
#'
#' @param folder Export location of all the plots. Defaults to current working directory.
#'
#' @return NULL
#'
#' @examples eda_graphs(Boston)
#'
#' @export eda_graphs

eda_graphs <- function(data,var=0,folder = getwd())
{
  require(corrplot)
  if(!is.data.frame(data))
    stop("The given object is not a data frame")
  if (var == 0)
    var = 1:ncol(data)

  if (!file.exists(folder)){
    print(paste('The path does not exist. Plots will be saved in working directory:' , getwd()))
    folder = getwd()
  }
  for(i in var)
  {
    if(is.numeric(data[,i]))
    {
      data_level = length(levels(factor(data[,i])))
      if(data_level > 10){
        png(filename = paste(folder,paste('/',names(data)[i], ".png", sep=""),sep = '')) #NOTE this step

        par(mfrow=c(2,1))
        boxplot(data[,i], main = paste("Boxplot of", names(data)[i]),
                ylab = names(data)[i], col = "maroon", border = "grey5",
                horizontal = T)

        hist(data[,i], main = paste("Histogram of", names(data)[i]),
             xlab = names(data)[i], ylab = "Frequency", col = "lightgreen", border=F)
        dev.off()
      }
      else{
        png(filename = paste(folder,paste('/',names(data)[i], ".png", sep=""),sep = '')) #NOTE this step
        par(mfrow=c(2,1))
        pie(table(data[,i]), main = paste("Pie Chart of", names(data)[i]),
            col = topo.colors(data_level),radius = 1
        )

        barplot(table(data[,i]), main = paste("Barplot of", names(data)[i]),
                xlab = names(data)[i], ylab = "Frequency", col = "lightgreen")

        dev.off()
      }
      png(filename = paste(folder,"/Correlation_plot.png", sep=""))
      par(mfrow = c(1,1))

      corrplot(cor(data), method="color",
               diag=FALSE, # tl.pos="d",
               type="upper", order="hclust",
               title = 'Correlation plot of Data',
               addCoef.col = "black",
               mar=c(0,0,1,0)
      )
      dev.off()
    }
    png(filename = paste(folder,paste('/density_',names(data)[i], ".png", sep=""),sep = ''))

    plot(density(data[,i]), frame = FALSE, col = "lightblue",
         main = paste("Density plot of", names(data)[i]),
         xlab = names(data)[i])
    polygon(density(data[,i]), col = "lightblue")
    dev.off()
  }

}
