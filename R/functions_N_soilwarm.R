
# ==============================================================================
# Nitrogen coupling helper functions
# ==============================================================================

transform_N_block <- function(df, process_vars, gene_vars, state_vars){

  out <- df

  out <- out %>%
    mutate(
      across(all_of(gene_vars), ~log10(.x + 1))
    )

  out <- out %>%
    mutate(
      across(all_of(c(process_vars, gene_vars, state_vars)),
             ~as.numeric(scale(.x)))
    )

  out
}


run_N_pca <- function(df, vars){

  x <- df[, vars, drop=FALSE]

  pca <- prcomp(x, center = FALSE, scale. = FALSE)

  scores <- as.data.frame(pca$x)
  loadings <- as.data.frame(pca$rotation)

  list(
    pca=pca,
    scores=scores,
    loadings=loadings,
    variance=(pca$sdev^2)/sum(pca$sdev^2)
  )
}


compute_temporal_delta <- function(df, score_vars){

  df %>%
    select(Number, Year, all_of(score_vars)) %>%
    tidyr::pivot_wider(
      names_from=Year,
      values_from=all_of(score_vars),
      names_sep="_"
    ) %>%
    mutate(
      across(
        ends_with("_2024"),
        ~ .x - get(sub("_2024$", "_2022", cur_column())),
        .names="{sub('_2024','_delta',.col)}"
      )
    )
}
