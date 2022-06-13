#' Conduct Heartbeat Analysis
#'
#' @param data A numeric data frame or object that can be coerced into a data frame.
#' @param type Type of output. The default is "counts" which provides tallies the up/neutral/down votes of each item, "standard" standardizes each rating of the original data frame. "votes" converts the standardization score into up-vote/down-vote/neutral based on the threshold value.
#' @param threshold A number that denotes the range of "Neutral", or conversely, what deviation score above/below the base-rate is necessary to count as an up/down-vote. Default = 1.
#' @param props Default is false. If props = TRUE, it will convert counts of each vote into proportions. This Will only change output when type = "counts".
#' @param controversial Default is true. If controversial = FALSE, the function will not provide the controversy score for each item.
#' @return Number of up-votes, down-votes, and neutral-votes for each variable in the data frame \code{data}.
#' @examples
#' \dontrun{heartbeat(data, type = "counts", threshold = 0.5, props = TRUE, controversial = FALSE)}
#' @import dplyr
#' @import tidyr
#' @export
heartbeat <- function(data, type = c("counts", "standard", "votes"), threshold = 1, props = FALSE, controversial = TRUE){


  if(missing(data)){
    stop("Please provide a data.frame containing numeric variables.")
  }

  if(missing(data) == FALSE && is.data.frame(data) == FALSE){
    data <- as.data.frame(data)
  }

  if(is.data.frame(data) == FALSE){
    stop("Must pass a data.frame or object that can be converted to data.frame.")
  }

  data <- as.data.frame(sapply(data,as.numeric))

  if(missing(data) == TRUE && is.data.frame(data) == TRUE && all(sapply(data,is.numeric)) == FALSE){
    stop("All variables must contain numeric values.")
  }

  if(missing(data) == TRUE && is.data.frame(data) == TRUE && all(sapply(data,is.numeric)) == FALSE && all(is.na(data)) == TRUE){
    warning("Dataset contained NA Values which will have been dropped prior to calculation. This could be a result of coercing data to Numeric.")
  }

  type <- match.arg(type)

  if(is.numeric(threshold) == FALSE){
    stop("Threshold must be a numeric value.")
  }

  . <- individual.sd <- individual.mean <- Item <- Var1 <- Freq <- NULL
    # Not necessary, likely not advisable to do this either. Just doing it for R CMD check...

  likert.items <- colnames(data)

  Standard <- data %>%
    mutate(
      individual.sd =
        sqrt(rowSums((select(data, likert.items) -
                        rowMeans(select(data, all_of(likert.items)),na.rm = T))^2,
                     na.rm = T) /
               (rowSums(!is.na(select(data, all_of(likert.items)))) - 1)),
      individual.mean =
        rowMeans(select(data, all_of(likert.items)),na.rm = T)) %>%
    mutate_at(likert.items, function(x){
      case_when(.$individual.sd == 0 ~ 0,
                .$individual.sd != 0 ~
                  (x - .$individual.mean) / (.$individual.sd)
      )}) %>%
    subset(., select = -c(individual.sd, individual.mean))

  Votes <- Standard %>%
    mutate_at(likert.items, function(x){
      case_when(x > threshold ~ 1,
                x < -1*threshold ~ -1,
                x < threshold & x > -1*threshold ~ 0)
    }
    )


  Counts <- Votes %>%
    lapply(table) %>%
    lapply(as.data.frame) %>%
    Map(cbind, Item = names(Votes),.) %>%
    bind_rows() %>%
    group_by(Item) %>%
    spread(Var1, Freq) %>%
    .[match(likert.items, .$Item),] %>%
    as.data.frame()

  Counts[is.na(Counts)] <- 0
  colnames(Counts)[2:4] <- c("Down_Vote", "Neutral", "Up_Vote")

  Cont <- Counts %>%
    mutate(
      Controversial = ((Counts$Up_Vote + Counts$Down_Vote) - abs(Counts$Up_Vote - Counts$Down_Vote))/(Counts$Neutral)
    ) %>%
    as.data.frame()

  Counts_Proportion <- Counts %>%
    mutate_if(is.numeric, ~ . / rowSums(select(Counts, where(is.numeric))))

  Cont_Proportion <- cbind(Counts_Proportion, "Controversial" = Cont[,5])






  if(type == "standard"){
    Standard
  }
  else if(type == "votes"){
    Votes
  }
  else if(type == "counts"){
    if(controversial == TRUE){
      if (props == TRUE){
        Cont_Proportion
      }else{
        Cont
      }
    }else{
      if (props == TRUE){
        Counts_Proportion
      }else{
        Counts
      }
    }
  }

}
