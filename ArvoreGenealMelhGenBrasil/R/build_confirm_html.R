fmt_chr  <- function(x) x %>% as.character() %>% str_squish() %>% na_if("") %>% replace_na("—")
fmt_date <- function(x) ifelse(is.na(x), "—", format(as.Date(x), "%d/%m/%Y"))

# ⬇️ mesma função, só que com fonte menor (default 12px)
build_confirm_html <- function(row, font_px = 14, pad_v = 4, pad_h = 8) {
  
  cell_lbl <- sprintf("font-weight:600;padding:%dpx %dpx;font-size:%dpx;", pad_v, pad_h, font_px)
  cell_val <- sprintf("padding:%dpx %dpx;font-size:%dpx;",              pad_v, pad_h, font_px)
  
  linhas <- tibble::tribble(
    ~campo,                             ~valor,
    "Seu Nome",                            row$Nome,
    "Orientador de Graduação",           row$Orientador_Graduacao,
    "Universidade de Graduação",         row$Universidade_Graduacao,
    "Curso de Graduação",                row$Curso_Graduacao,
    "Data da defesa de Graduação",       fmt_date(row$DataDefesa_Graduacao),
    
    "Orientador de Mestrado",            row$Orientador_Mestrado,
    "Universidade de Mestrado",          row$Universidade_Mestrado,
    "Curso de Mestrado",                 row$Curso_Mestrado,
    "Data da defesa de Mestrado",        fmt_date(row$DataDefesa_Mestrado),
    
    "Orientador de Doutorado",           row$Orientador_Doutorado,
    "Universidade de Doutorado",         row$Universidade_Doutorado,
    "Curso de Doutorado",                row$Curso_Doutorado,
    "Data da defesa de Doutorado",       fmt_date(row$DataDefesa_Doutorado),
    
    "Doutorado Sandwich?",              ifelse(isTRUE(row$Dout_Sandwich), "Sim", "Não"),
    "Orientador do Sandwich",            row$Orientador_Sandwich,
    "Universidade do Sandwich",          row$Universidade_Sandwich,
    
  ) %>%
    mutate(
      valor = fmt_chr(valor),
      tr = sprintf(
        "<tr>
           <td style='%s'>%s</td>
           <td style='%s'>%s</td>
         </tr>",
        cell_lbl, htmlEscape(campo), cell_val, htmlEscape(valor)
      )
    ) %>%
    pull(tr) %>%
    paste(collapse = "")
  
  sprintf("
    <div style='max-height:55vh; overflow:auto; font-size:%dpx; line-height:1.25;'>
      <table class='table table-sm' style='width:100%%;'>
        <tbody>%s</tbody>
      </table>
    </div>", font_px, linhas)
}