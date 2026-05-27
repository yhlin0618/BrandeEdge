#' Remove Illegal UTF-8 Characters from Data Frame
#'
#' This function removes illegal UTF-8 characters from all character and factor
#' /Users/che/Library/CloudStorage/Dropbox/che_workspace/projects/ai_martech/docscolumns in a data frame. It's a general utility function that can be used
#' across various contexts where data cleaning is needed.
#'
#' @param df A data frame containing columns to be cleaned
#' @return A data frame with illegal UTF-8 characters removed
#'
#' @details
#' The function processes each column in the data frame:
#' - Character columns: Uses iconv to remove illegal UTF-8 characters
#' - Factor columns: Converts to character, cleans, then converts back to factor
#' - Other columns: Left unchanged
#'
#' @examples
#' \dontrun{
#' # Clean a data frame with potential encoding issues
#' clean_df <- remove_illegal_utf8(messy_df)
#' }
#'
#' @export
remove_illegal_utf8 <- function(df) {
  df[] <- lapply(df, function(x) {
    if (is.character(x)) {
      # 嘗試將字串從 UTF-8 轉回 UTF-8，不合法的部分以空字串替換
      iconv(x, "UTF-8", "UTF-8", sub = "")
    } else if (is.factor(x)) {
      # 因子先轉為字串，處理後再轉回因子
      x_clean <- iconv(as.character(x), "UTF-8", "UTF-8", sub = "")
      factor(x_clean)
    } else {
      x
    }
  })
  return(df)
}
