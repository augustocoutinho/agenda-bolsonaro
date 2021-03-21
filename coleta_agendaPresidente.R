
## Função para coleta da agenda do Presidente Jair Bolsonaro
agenda_bolsonaro <- function(data_inicio, data_fim){
  agenda_presidente <- data.frame()
  dias <- seq(to=as.Date(data_inicio), from = as.Date(data_fim), by = 1)
  for (i in 1:length(dias)) {
    url_bolsonaro <- paste0("https://www.gov.br/planalto/pt-br/acompanhe-o-planalto/agenda-do-presidente-da-republica/", dias[i])
    html <- read_html(url_bolsonaro)
    
    html_nodes(html, "h4") %>% html_text() -> compromisso
    html_nodes(html, "div.compromisso-local") %>% html_text() -> local
    html_nodes(html, "time.compromisso-inicio") %>% html_text() %>% str_replace("h", "\\:") -> horario
    html_nodes(html, "span#breadcrumbs-current") %>% html_text() -> dia_agenda
    str_extract(string = dia_agenda, pattern = ".{2}\\/.{2}") -> dia_agenda
    html_nodes(html, "body.default-header-template.portal-institucional.template-view.portaltype-agendadiaria.site-pt-br.section-acompanhe-o-planalto.subsection-agenda-do-presidente-da-republica.subsection-agenda-do-presidente-da-republica-2020-10-13.userrole-anonymous:nth-child(2) div:nth-child(2) div:nth-child(4) div.documentByLine:nth-child(3) span.documentPublished > span.value:nth-child(2)") %>% html_text() %>% str_extract(".{10}") -> data_agenda
    agenda_nova <- data_frame("Autoridade" = rep("Jair Bolsonaro", length(compromisso)), "Dia" = rep(dia_agenda, length(compromisso)), "Horário" = horario, "Local" = local, "Compromisso" = compromisso)
    agenda_presidente <- rbind(agenda_presidente, agenda_nova)
  }
  return(assign("agenda_Presidente", agenda_presidente, envir = .GlobalEnv))
}


## Roda a função
agenda_bolsonaro("2021-03-19", "2021-03-15")