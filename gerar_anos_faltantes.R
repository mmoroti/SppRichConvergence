gerar_anos_faltantes <- function(identificadores, anos) {
  # Criando um data frame vazio para armazenar os resultados
  resultado <- data.frame(Cell_Id110 = character(), Ano = integer())
  
  # Preenchendo o data frame com os valores
  for (i in 1:length(identificadores)) {
    id_atual <- identificadores[i]
    anos_atuais <- anos
    
    # Criando um data frame temporário para o identificador atual
    df_temp <- data.frame(Cell_Id110 = rep(id_atual, length(anos_atuais)),
                          LastYear = anos_atuais)
    
    # Adicionando ao resultado final
    resultado <- rbind(resultado, df_temp)
  }
  
  return(resultado)
}
