#' Draw a bar graph about who will be given the time of day around here
#'
#' @return NULL
#' @export
pride_1 <- function() {
  W <- expand.grid(Sort=seqinr::rot13(c("bgure", "ovtbg", "genaf")), useR=c("does", "should"))
  W$order <- xor(W$Sort==seqinr::rot13("genaf"), W$useR=="does")
  W$value <- as.numeric(!(W$Sort==seqinr::rot13("ovtbg")))

  ggplot2::ggplot(W, ggplot2::aes_(x=W$Sort, color=W$order, fill=W$useR, weight=W$value, alpha=W$value)) +
    ggplot2::geom_bar(position="dodge", linetype=0, width=1.25) +
    ggplot2::ylab("Probability you'll be given the time of day around here") +
    ggplot2::xlab("Sort") +
    ggplot2::guides(fill=ggplot2::guide_legend("useR"), color=FALSE, alpha=FALSE) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(), panel.grid.minor.y = ggplot2::element_blank()) +
    ggplot2::scale_alpha_continuous(range=c(0, 2/3)) +
    ggplot2::coord_flip()
}
