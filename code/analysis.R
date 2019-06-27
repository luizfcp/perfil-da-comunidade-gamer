
library(dplyr);
questionario = read_csv("https://github.com/luizfcp/perfil-da-comunidade-gamer/raw/master/data/Perfil%20da%20Comunidade%20Gamer%20(respostas)%20-%20Respostas%20ao%20formul%C3%A1rio%201.csv")
  
console = questionario %>% 
  filter(`Qual plataforma prefere usar para jogar?` == "Console (video game)")
computador = questionario %>% 
  filter(`Qual plataforma prefere usar para jogar?` == "Desktop (PC de mesa)")
notebook = questionario %>% 
  filter(`Qual plataforma prefere usar para jogar?` == "Notebook")
celular = questionario %>% 
  filter(`Qual plataforma prefere usar para jogar?` == "Outros (Celular)")

