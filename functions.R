library(dplyr)
library(tidyr)
library(forcats)
library(stringr)
library(kableExtra)
library(ggplot2)
library(scales)
library(purrr)

#' Compare top languages between two years (table + arrow plot)
#'
#' @param tulos Data frame with columns Year, Language, `Population 31 Dec`, Rank
#' @param start_year,end_year Years to compare
#' @param rank_limit Include languages that were top-N in EITHER year
#' @param text_size Multiplier for plot text sizes
#' @return list(table = kableExtra html table, plot = ggplot object)
analyze_language_trends <- function(tulos, start_year, end_year, rank_limit, text_size = 1) {

  language_start <- tulos %>%
    ungroup() %>%
    filter(Year == start_year & Rank <= rank_limit) %>%
    select(Language)

  language_end <- tulos %>%
    ungroup() %>%
    filter(Year == end_year & Rank <= rank_limit) %>%
    select(Language)

  languages_most <- unique(bind_rows(language_start, language_end))

  data_most <- tulos %>%
    select(Year, Language, `Population 31 Dec`, Rank) %>%
    filter(Language %in% pull(languages_most) & Year %in% c(start_year, end_year)) %>%
    arrange(Year) %>%
    mutate(Language = fct_reorder(stringr::str_remove(Language, "(,|;)\\s+\\w+"),
                                   `Population 31 Dec`, max),
           pop_label = 1000 * round(`Population 31 Dec` / 1000, 1)) %>%
    arrange(Year, desc(Rank))

  table <- data_most %>%
    select(-pop_label) %>%
    rename(Population = `Population 31 Dec`) %>%
    tidyr::pivot_wider(
      names_from = Year,
      values_from = c(Rank, Population)
    ) %>%
    mutate(`Change in rank` = .[[2]] - .[[3]],
           `Change in pop` = .[[5]] - .[[4]]) %>%
    arrange(.[[3]])

  ukr <- which(table$Language == "Ukrainian")

  table_html <- kbl(
    table, escape = FALSE, format = "html",
    col.names = gsub("_", " ", names(table)),
    caption = paste("The most spoken languages in years", start_year, "and", end_year, "by rank in", end_year),
    format.args = list(big.mark = " ")
  ) %>%
    kable_classic_2("striped", full_width = TRUE) %>%
    row_spec(0, bold = TRUE, color = "white", italic = TRUE, background = "#D7261E") %>%
    column_spec(1, bold = TRUE, color = "white", background = "#D7261E") %>%
    kable_styling(bootstrap_options = c("striped", "hover"))

  if (length(ukr) > 0) {
    table_html <- table_html %>%
      row_spec(ukr, bold = TRUE, italic = TRUE, angle = -5, color = "#210236", background = "#c6f7e5")
  }

  arrow_data <- data_most %>%
    group_by(Language) %>%
    summarize(
      start_year = min(Year),
      end_year = max(Year),
      start_pop = `Population 31 Dec`[Year == min(Year)],
      end_pop = `Population 31 Dec`[Year == max(Year)],
      .groups = "drop"
    ) %>%
    mutate(
      pop_diff = end_pop - start_pop,
      label = paste0("\u0394", pop_diff),
      growth_status = ifelse(end_pop > start_pop, "Growing", "Decreasing"),
      start_pop_zero = 0,
      end_pop_adjusted = end_pop - start_pop
    )

  plot <- ggplot(arrow_data) +
    geom_segment(
      aes(
        x = start_pop_zero, xend = end_pop_adjusted,
        y = reorder(Language, pop_diff), yend = reorder(Language, pop_diff),
        color = growth_status
      ),
      arrow = arrow(length = unit(3, "mm"), type = "closed"),
      linewidth = 1
    ) +
    geom_text(
      aes(
        x = (start_pop_zero + end_pop_adjusted) / 2,
        y = reorder(Language, pop_diff),
        label = label
      ),
      vjust = -0.5, size = 5 * text_size, family = "sans", color = "blue"
    ) +
    geom_text(
      aes(x = start_pop_zero, y = reorder(Language, pop_diff), label = start_pop),
      vjust = 1.5, size = 3.5 * text_size, family = "sans", color = "springgreen4"
    ) +
    geom_text(
      aes(x = end_pop_adjusted, y = reorder(Language, pop_diff), label = end_pop),
      vjust = -1.7, hjust = 0, size = 3.5 * text_size, family = "sans", color = "orangered3"
    ) +
    labs(
      title = paste("Population Changes Between", start_year, "and", end_year),
      x = "Population", y = "Language", color = "Growth Status"
    ) +
    scale_color_manual(values = c("Growing" = "seagreen", "Decreasing" = "tomato3")) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 18 * text_size, face = "bold"),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 14 * text_size, color = "darkgreen"),
      axis.title.x = element_text(size = 16 * text_size, color = "purple"),
      axis.title.y = element_text(size = 16 * text_size, color = "brown"),
      legend.title = element_text(size = 14 * text_size),
      legend.text = element_text(size = 12 * text_size)
    )

  list(table = table_html, plot = plot)
}

#' Compare top languages across several consecutive periods (faceted arrow plot)
#'
#' @param df Data frame with columns Year, Language, `Population 31 Dec`, Rank
#' @param breaks Vector of year boundaries, e.g. c(1994, 2004, 2014, 2025)
#' @param rank Include languages that were top-N in either boundary year of a period
#' @param label_size,text_offset,base_font_size Plot text sizing
#' @return ggplot object faceted by period
analyze_series <- function(df, breaks = c(1994, 2004, 2014, 2025), rank = 15,
                            label_size = 4, text_offset = 1.5, base_font_size = 10) {

  intervals <- data.frame(
    start = head(breaks, -1),
    end = tail(breaks, -1)
  )

  plot_data <- purrr::map2_dfr(intervals$start, intervals$end, function(start, end) {
    language_start <- df %>% filter(Year == start & Rank <= rank) %>% select(Language)
    language_end <- df %>% filter(Year == end & Rank <= rank) %>% select(Language)
    languages_most <- unique(dplyr::bind_rows(language_start, language_end))

    df %>%
      filter(Year %in% c(start, end), Language %in% languages_most$Language) %>%
      select(Language, Year, `Population 31 Dec`) %>%
      arrange(Language, Year) %>%
      group_by(Language) %>%
      summarize(
        start_pop = first(`Population 31 Dec`),
        end_pop = last(`Population 31 Dec`),
        diff = end_pop - start_pop,
        .groups = "drop"
      ) %>%
      mutate(
        label = paste0("\u0394", diff),
        growth_status = ifelse(diff >= 0, "Growing", "Decreasing"),
        start_pop_zero = 0,
        end_pop_adjusted = end_pop - start_pop,
        period = paste0(start, "\u2013", end)
      )
  })

  ggplot(plot_data) +
    geom_segment(
      aes(
        x = start_pop_zero, xend = end_pop_adjusted,
        y = reorder(Language, diff), yend = reorder(Language, diff),
        color = growth_status
      ),
      arrow = arrow(length = unit(3, "mm"), type = "closed"),
      linewidth = 1
    ) +
    geom_text(
      aes(x = (start_pop_zero + end_pop_adjusted) / 2, y = reorder(Language, diff), label = label),
      vjust = -text_offset, size = label_size, family = "sans", color = "blue"
    ) +
    geom_text(
      aes(x = start_pop_zero, y = reorder(Language, diff), label = start_pop),
      vjust = 1.2, size = label_size * 0.85, family = "sans", color = "springgreen4"
    ) +
    geom_text(
      aes(x = end_pop_adjusted, y = reorder(Language, diff), label = end_pop),
      vjust = -1.7, hjust = 0, size = label_size * 0.85, family = "sans", color = "orangered3"
    ) +
    facet_wrap(~period, ncol = 1) +
    labs(
      title = paste("Language Population Trends across", nrow(intervals), "Periods"),
      x = "Population Change", y = "Language", color = "Growth Status"
    ) +
    scale_color_manual(values = c("Growing" = "seagreen", "Decreasing" = "tomato3")) +
    theme_minimal(base_size = base_font_size) +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = base_font_size * 1.1, color = "darkgreen"),
      axis.title.x = element_text(size = base_font_size * 1.2, color = "purple"),
      axis.title.y = element_text(size = base_font_size * 1.2, color = "brown"),
      legend.title = element_text(size = base_font_size * 1.1),
      legend.text = element_text(size = base_font_size * 0.95)
    )
}
