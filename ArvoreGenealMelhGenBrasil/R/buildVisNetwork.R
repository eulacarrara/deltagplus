

# ---- LIMPEZA PADRÃO (aspas, espaços, UTF-8, vazios) ----
clean_text <- function(x) {
    x <- enc2utf8(as.character(x))
    x <- stri_replace_all_regex(x, "[\u201C\u201D]", "\"")  # “ ”
    x <- gsub('^"|"$', "", x)
    x <- str_replace_all(x, "\\u00A0", " ")  # NBSP, se houver
    x <- str_squish(x)
    x[x == ""] <- NA_character_
    x
}

# df <- dadosMelhoramento %>% mutate(across(everything(), clean_text))


buildVisNetwork <- function(df) {
    # ---- ARESTAS: orientador -> aluno ----
    prof_cols <- c("Orientador_Graduacao","Orientador_Mestrado","Orientador_Doutorado")
    level_map <- c(Orientador_Graduacao="Graduação",
                   Orientador_Mestrado="Mestrado",
                   Orientador_Doutorado="Doutorado")
    
    edges <- df %>%
        pivot_longer(any_of(prof_cols), names_to = "level", values_to = "advisor") %>%
        filter(!is.na(advisor)) %>%
        separate_rows(advisor, sep = "\\s*[,;/|]\\s*|\\s*;\\s*") %>%
        mutate(advisor = clean_text(advisor)) %>%
        filter(!is.na(advisor), !is.na(Nome)) %>%
        transmute(from = advisor, to = Nome,
                  relation = recode(level, !!!level_map)) %>%
        distinct()
    
    # ---- NORMALIZAÇÃO p/ chave (junta 'João' ~ 'Joao', mas mantém rótulo com acento) ----
    normalizeKey <- function(x) stri_trans_general(tolower(x), "Latin-ASCII")
    
    # mapeia id normalizado -> rótulo "canônico" (pega o 1º visto)
    nameKey <- tibble(label = unique(c(edges$from, edges$to))) %>%
        mutate(id = normalizeKey(label)) %>%
        group_by(id) %>% slice(1) %>% ungroup()  # mantemos o 1º label encontrado
    
    # aplica o mapeamento às arestas
    edges2 <- edges %>%
        left_join(nameKey, by = c("from" = "label")) %>% rename(from_id = id) %>%
        left_join(nameKey, by = c("to"   = "label")) %>% rename(to_id   = id) %>%
        transmute(from = from_id, to = to_id, relation) %>%
        distinct()
    
    # ---- DIAGNÓSTICO DE NA (se houver) ----
    na_edges <- edges2 %>% filter(is.na(from) | is.na(to))
    if (nrow(na_edges) > 0) {
        # mostra quais nomes originais não mapearam
        problem_from <- edges %>%
            anti_join(nameKey, by = c("from" = "label")) %>% distinct(from)
        problem_to <- edges %>%
            anti_join(nameKey, by = c("to" = "label")) %>% distinct(to)
        message("⚠️ Arestas com NA detectadas. Verifique grafia/aspas/espacos:\n",
                "- from não mapeou: ", paste(problem_from$from, collapse = " | "), "\n",
                "- to   não mapeou: ", paste(problem_to$to,   collapse = " | "))
        # remove NAs para evitar erro no igraph
        edges2 <- edges2 %>% filter(!is.na(from), !is.na(to))
    }
    
    # ---- NODES garantidos a partir de edges2 (cobre 100% dos ids usados) ----
    nodes <- tibble(id = sort(unique(c(edges2$from, edges2$to)))) %>%
        left_join(nameKey, by = "id") %>%
        transmute(id, label)  # label com acento preservado
    
    # grupos/atributos
    prof_ids <- unique(edges2$from)
    stud_ids <- unique(edges2$to)
    
    nodes <- nodes %>%
        mutate(
            is_prof = id %in% prof_ids,
            is_stud = id %in% stud_ids,
            group   = case_when(
                is_prof & is_stud ~ "Professor & Ex-aluno",
                is_prof           ~ "Professor",
                TRUE              ~ "Aluno"
            )
        )
    
    # graus
    g <- graph_from_data_frame(edges2, directed = TRUE, vertices = nodes)
    deg_in  <- degree(g, mode = "in")
    deg_out <- degree(g, mode = "out")
    
    nodes <- nodes %>%
        mutate(
            in_degree  = deg_in[id],
            out_degree = deg_out[id],
            degree     = in_degree + out_degree,
            value      = pmax(5, 5 + sqrt(degree) * 3),
            title      = paste0("<b>", label, "</b><br/>Grupo: ", group,
                                "<br/>Orientadores (in): ", in_degree,
                                "<br/>Orientandos (out): ", out_degree)
        )
    
    edge_colors <- c("Graduação"="#1f77b4", "Mestrado"="#2ca02c", "Doutorado"="#d62728")
    edges_v <- edges2 %>%
        mutate(color = edge_colors[relation], title = relation)
    
    # ---- VISUALIZAÇÃO ----
    visNetwork(
        nodes %>% select(id, label, group, value, title),
        edges_v %>% select(from, to, color, title),
        height = "720px", width = "100%"
    ) %>%
        visEdges(arrows = "to", smooth = FALSE) %>%
        visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE),
                   nodesIdSelection = list(enabled = TRUE, useLabels = TRUE)) %>%
        visLegend(useGroups = FALSE,
                  addEdges = data.frame(label = names(edge_colors),
                                        color = unname(edge_colors))) %>%
        visPhysics(solver = "barnesHut",
                   barnesHut = list(avoidOverlap = 0.2),
                   stabilization = TRUE)
    
}
