


# df <- dadosMelhoramento





makeSankey_echarts <- function(df,
                               label_color  = "#111111",
                               tooltip_bg   = "rgba(15,15,15,0.96)",
                               tooltip_txt  = "#FFFFFF",
                               tooltip_size = 14,
                               tooltip_font = "Segoe UI, Roboto, Helvetica Neue, Arial, sans-serif",
                               value_scale  = 0.25  # ⬅️ 0.10–0.40 deixam as linhas bem finas
) {
  
  # 1) Empilha e marca o nível
  links <- bind_rows(
    tibble(source = df$Orientador_Graduacao, target = df$Nome,     value = 1, nivel = "Graduação"),
    tibble(source = df$Orientador_Mestrado,  target = df$Nome,     value = 1, nivel = "Mestrado"),
    tibble(source = df$Orientador_Doutorado, target = df$Nome,     value = 1, nivel = "Doutorado")
  ) %>%
    mutate(
      source = source %>% as.character() %>% stringr::str_squish(),
      target = target %>% as.character() %>% stringr::str_squish()
    ) %>%
    filter(source != "", target != "", !is.na(source), !is.na(target)) %>%
    dplyr::count(source, target, nivel, name = "value") %>%
    mutate(value = value * value_scale)
  
  # 2) Nós únicos
  nodes <- tibble(name = unique(c(links$source, links$target))) %>% unique()
  
  # 3) Nível por nó (entrada priorizada)
  niveis_in <- links %>%
    group_by(target) %>%
    summarise(nivel_node_in = paste(sort(unique(nivel)), collapse = " / "), .groups = "drop")
  
  niveis_out <- links %>%
    group_by(source) %>%
    summarise(nivel_node_out = paste(sort(unique(nivel)), collapse = " / "), .groups = "drop")
  
  nodes <- nodes %>%
    left_join(niveis_in,  by = c("name" = "target")) %>%
    left_join(niveis_out, by = c("name" = "source")) %>%
    mutate(
      nivel_node = coalesce(nivel_node_in, nivel_node_out),
      nivel_node = replace_na(nivel_node, "")
    ) %>%
    select(name, nivel_node)
  
  # 4) Parâmetros para “linhas finas”
  n_nodes   <- nrow(nodes)
  node_gap  <- case_when(
    n_nodes <=  40 ~ 18L,
    n_nodes <= 120 ~ 26L,
    TRUE           ~ 34L
  )
  node_w    <- 16L  # nó mais estreito ajuda o look “slim”
  
  links %>%
    e_charts() %>%
    e_sankey(
      source, target, value,
      data            = nodes,
      draggable       = TRUE,
      nodeWidth       = node_w,
      nodeGap         = node_gap,
      layoutIterations= 0,
      # "Nome (Nível)"
      label = list(
        color     = label_color,
        fontSize  = 12,
        formatter = htmlwidgets::JS(
          "function(p){
             var nv = p.data.nivel_node;
             return nv ? (p.name + ' (' + nv + ')') : p.name;
           }"
        )
      ),
      # links com cor do nó de origem e baixa opacidade (visual mais leve)
      lineStyle = list(color = 'source', opacity = 0.30)
    ) %>%
    e_tooltip(
      trigger         = "item",
      confine         = TRUE,
      backgroundColor = tooltip_bg,
      borderColor     = "#222",
      borderWidth     = 1,
      textStyle       = list(
        color      = tooltip_txt,
        fontSize   = tooltip_size,
        fontFamily = tooltip_font,
        fontWeight = "600",
        lineHeight = 20
      ),
      extraCssText    = "padding:10px 12px;border-radius:8px;box-shadow:0 8px 24px rgba(0,0,0,.35);letter-spacing:.2px;",
      formatter = htmlwidgets::JS(
        "function(p){
           if (p.dataType === 'edge') {
             return `
               <div style='font-weight:700'>${p.data.source} → ${p.data.target}</div>
               <div style='opacity:.9'>Nível: ${p.data.nivel}</div>
               <div style='opacity:.9'>Interações: ${p.data.value}</div>
             `;
           } else {
             var nv = p.data.nivel_node || '';
             return `
               <div style='font-weight:700'>${p.name}</div>
               ${nv ? `<div style='opacity:.9'>Nível: ${nv}</div>` : ``}
             `;
           }
         }")
    )
}
