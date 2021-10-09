####### Regressao Logística #######

### OBS: As informações geradas através desse modelo podem ser geradas facilmente através da biblioteca CARET.
### O exemplo abaixo é para exercitar os os conceitos de estatística.

# Instalar e carregar biblioteca para gerar a curva ROC
Install.packages(‘pROC’) # Instala
library(pROC) # Carrega

# Carregar o dataset
dados <- data.frame(Prova_Logica = c(2, 2, 5, 5, 5, 2, 3, 2, 1, 4, 
									 5, 8, 1, 1, 3, 4, 3, 2, 1, 1, 
									 8, 8, 1, 2, 1, 5, 3, 3, 5, 4, 
									 4, 1, 8, 3, 2, 3, 3, 2, 1, 1, 
									 5, 4, 1, 5, 3, 1, 4, 6, 1, 1, 
									 8, 1, 1, 5, 1, 5, 3, 1, 1, 8, 
									 1, 1, 1, 1, 1, 2, 1, 5, 5, 4, 
									 2, 1, 8, 4, 5, 1, 3, 3, 3, 5, 
									 3, 1, 7, 1, 1, 2, 9, 5, 3, 1, 
									 5, 1, 4, 2, 1, 4, 3, 3, 8, 1, 
									 1, 8, 5, 1, 1, 1, 5, 8, 5, 1, 
									 4, 2, 5, 4, 5, 3, 3, 5, 5, 5, 
									 5, 5, 8, 5, 4, 9, 8, 1, 3, 4, 
									 2, 5, 1, 4, 3, 5, 5, 5, 6, 4, 
									 3, 5, 7, 1, 8, 5, 7, 3, 2, 3, 
									 2, 5, 5, 5, 5, 4, 4, 8, 1, 1, 
									 2, 5, 3, 2, 7, 4, 1, 1, 1, 4, 
									 5, 1, 1, 8, 3, 6, 8, 3, 1, 3, 
									 3, 2, 8, 4, 1, 1, 1, 1, 1, 2, 
									 3, 4, 6, 2, 3, 3, 4, 2, 1, 5, 
									 2, 4, 3, 3, 1, 3, 3, 3, 1, 3, 
									 5, 6, 1, 5, 1, 5, 4, 3, 1, 6, 
									 1, 4, 9, 3, 3, 2, 1, 1, 4, 3, 
									 1, 3, 1, 1, 3, 7, 8, 1, 3, 5, 
									 6, 3, 6, 5, 8, 5, 1, 1, 4, 2, 
									 1, 8, 7, 5, 1, 1, 1, 6, 5, 7, 
									 3, 3, 5, 1, 3, 5, 1, 8, 8, 1, 
									 2, 3, 3, 3, 3, 7, 1, 9, 8, 4, 
									 1, 7, 1, 1, 1, 5, 1, 1, 5, 3, 
									 5, 1, 3, 6, 2, 1, 3, 4, 5, 6, 
									 1, 5, 1, 5, 1, 1, 5, 1, 1, 1, 
									 5, 1, 3, 7, 1, 4, 3, 7, 1, 1, 
									 5, 4, 1, 1, 3, 5, 4, 2, 1, 5, 
									 1, 1, 1, 8, 8, 5, 1, 2, 1, 6, 
									 8, 3, 1, 5, 1, 5, 1, 4, 4, 8, 
									 1, 1, 1, 5, 1, 1, 5, 4, 6, 8, 
									 1, 3, 1, 6, 1, 1, 1, 1, 1, 8, 
									 1, 5, 3, 1, 4, 4, 7, 2, 3, 3, 
									 5, 8, 3, 1, 4, 1, 5, 1, 7, 2, 
									 6, 4, 1, 3, 1, 8, 5, 5, 5, 3, 
									 1, 4, 5, 3, 6, 1, 3, 3, 5, 4, 
									 5, 3, 1, 4, 5, 1, 3, 6, 1, 1, 
									 1, 3, 1, 1, 1, 1, 3, 1, 5, 4, 
									 8, 1, 5, 6, 6, 4, 2, 5, 5, 6, 
									 1, 2, 5, 6, 4, 2, 1, 2, 7, 2, 
									 8, 1, 3, 1, 1, 1, 6, 1, 4, 3, 
									 1, 5, 2, 1, 1, 5, 4, 3, 1, 3, 
									 1, 1, 2, 4, 5, 4, 5, 4, 3, 7, 
									 4, 1, 7, 1, 8, 5, 3, 3, 5, 1, 
									 2, 1, 5, 4, 4, 4, 3, 6, 8, 8, 
									 1, 1, 1, 8, 8, 1, 5, 1, 4, 5, 
									 1, 4, 1, 4, 1, 5, 1, 4, 4, 1, 
									 1, 2, 5, 1, 4, 4, 5, 4, 1, 1, 
									 5, 3, 5, 4, 1, 1, 5, 3, 3, 1, 
									 5, 5, 4, 5, 7, 2, 5, 9, 4, 6, 
									 1, 1, 1, 1, 8, 7, 2, 3, 2, 9, 
									 3, 1, 5, 1, 3, 5, 1, 4, 6, 3, 
									 1, 5, 1, 3, 9, 1, 4, 8, 8, 1, 
									 3, 2, 3, 3, 1, 5, 1, 1, 3, 1, 
									 5, 3, 4, 3, 4, 1, 4, 3, 5, 1, 
									 5, 2, 5, 8, 9, 4, 7, 4, 8, 5, 
									 7, 5, 3, 5, 6, 3, 1, 5, 6, 4, 
									 3, 5, 1, 2, 4, 1, 1, 2, 9, 5, 
									 1, 5, 1, 2, 1, 5, 1, 2, 3, 5, 
									 1, 1, 5, 3, 9, 5, 6, 9, 6, 1, 
									 1, 9, 1, 3, 5, 4, 8, 4, 2, 7, 
									 1, 6, 3, 7, 8, 1, 5, 5, 6, 1, 
									 4, 4, 3, 1, 5, 5, 8, 3, 3, 1, 
									 9, 5, 5, 5, 5, 1, 9, 6, 3, 1, 
									 1, 2, 4, 6, 5, 7, 6, 5, 1), 
						 Redacao = c(1, 1, 1, 4, 3, 3, 5, 5, 9, 1, 
						 			 1, 1, 1, 1, 4, 3, 3, 1, 1, 1, 
						 			 1, 7, 1, 1, 8, 1, 1, 1, 3, 8, 
						 			 3, 1, 5, 3, 3, 1, 2, 7, 1, 1, 
						 			 1, 1, 1, 1, 7, 1, 1, 8, 7, 1, 
						 			 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
						 			 5, 1, 8, 5, 1, 1, 1, 1, 1, 2, 
						 			 1, 1, 6, 1, 1, 1, 1, 1, 1, 1, 
						 			 4, 1, 8, 5, 1, 5, 8, 1, 1, 1, 
						 			 5, 1, 1, 1, 2, 3, 3, 1, 3, 8, 
						 			 1, 4, 6, 1, 1, 1, 1, 3, 1, 1, 
						 			 2, 3, 2, 1, 1, 1, 1, 4, 1, 1, 
						 			 1, 1, 4, 1, 8, 1, 5, 1, 1, 1, 
						 			 1, 1, 3, 6, 1, 2, 5, 6, 1, 2, 
						 			 2, 1, 8, 1, 4, 6, 9, 3, 1, 1, 
						 			 1, 1, 1, 1, 1, 1, 1, 7, 1, 1, 
						 			 1, 1, 1, 1, 6, 1, 1, 1, 1, 1, 
						 			 3, 5, 1, 1, 1, 1, 3, 1, 4, 1, 
						 			 1, 1, 2, 1, 3, 1, 1, 1, 4, 5, 
						 			 1, 1, 6, 1, 3, 1, 4, 1, 1, 1, 
						 			 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 
						 			 3, 6, 1, 8, 1, 1, 5, 1, 8, 2, 
						 			 6, 1, 5, 1, 6, 1, 1, 1, 1, 1, 
						 			 1, 1, 1, 3, 1, 4, 8, 1, 1, 1, 
						 			 8, 1, 3, 1, 6, 3, 1, 1, 1, 1, 
						 			 1, 1, 1, 1, 1, 1, 1, 1, 1, 5, 
						 			 1, 1, 3, 1, 1, 7, 4, 1, 1, 1, 
						 			 1, 6, 1, 3, 1, 4, 1, 1, 7, 2, 
						 			 6, 4, 1, 1, 1, 1, 1, 4, 7, 1, 
						 			 3, 1, 1, 9, 1, 1, 1, 1, 1, 1, 
						 			 1, 3, 1, 3, 1, 3, 1, 1, 3, 2, 
						 			 1, 1, 1, 5, 3, 1, 1, 2, 1, 1, 
						 			 4, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
						 			 1, 1, 4, 8, 1, 7, 1, 1, 3, 8, 
						 			 1, 1, 1, 1, 1, 4, 1, 1, 1, 2, 
						 			 2, 7, 1, 3, 1, 1, 1, 4, 2, 4, 
						 			 2, 2, 5, 3, 1, 1, 1, 5, 1, 9, 
						 			 1, 1, 3, 2, 1, 1, 5, 1, 2, 1, 
						 			 3, 8, 1, 5, 1, 4, 3, 1, 8, 1, 
						 			 6, 5, 1, 1, 1, 1, 1, 4, 5, 1, 
						 			 7, 8, 1, 4, 1, 1, 1, 1, 4, 1, 
						 			 1, 2, 1, 8, 2, 6, 2, 1, 4, 1, 
						 			 1, 1, 1, 1, 4, 1, 1, 1, 1, 1, 
						 			 8, 1, 1, 1, 3, 1, 1, 1, 8, 1, 
						 			 1, 1, 3, 1, 1, 1, 1, 1, 6, 1, 
						 			 1, 1, 1, 1, 4, 1, 1, 1, 1, 1, 
						 			 1, 1, 1, 3, 1, 2, 7, 2, 1, 1, 
						 			 1, 1, 1, 2, 2, 1, 3, 1, 1, 3, 
						 			 1, 1, 5, 1, 7, 1, 1, 1, 3, 6, 
						 			 1, 1, 1, 1, 1, 1, 1, 1, 4, 1, 
						 			 6, 8, 8, 7, 2, 1, 1, 1, 1, 1, 
						 			 1, 1, 5, 1, 1, 5, 1, 1, 1, 1, 
						 			 9, 1, 8, 1, 1, 2, 4, 1, 1, 6, 
						 			 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 
						 			 1, 1, 1, 1, 6, 1, 2, 1, 1, 5, 
						 			 4, 1, 8, 4, 6, 6, 1, 1, 1, 9, 
						 			 1, 1, 1, 1, 1, 8, 1, 1, 1, 1, 
						 			 1, 3, 1, 1, 4, 1, 1, 3, 4, 1, 
						 			 1, 3, 2, 3, 1, 2, 1, 1, 1, 1, 
						 			 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
						 			 2, 3, 1, 4, 1, 4, 2, 1, 6, 1, 
						 			 4, 2, 2, 1, 1, 1, 4, 1, 1, 1, 
						 			 1, 1, 6, 1, 1, 1, 3, 2, 8, 1, 
						 			 1, 1, 1, 1, 2, 3, 1, 1, 1, 1, 
						 			 1, 1, 6, 1, 6, 7, 1, 1, 5, 1, 
						 			 2, 5, 1, 1, 1, 1, 1, 2, 1, 3, 
						 			 1, 1, 1, 8, 7, 1, 1, 1, 1, 4, 
						 			 1, 6, 1, 2, 8, 4, 7, 1, 1, 1, 
						 			 5, 1, 1, 2, 1, 1, 7, 1, 1, 1, 
						 			 4, 1, 1, 3, 1, 5, 1, 7, 1), 
				  Auto_Avaliacao = c(1, 1, 1, 1, 1, 1, 1, 7, 1, 1, 
				  					 1, 9, 1, 1, 1, 3, 6, 1, 1, 1, 
				  					 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
				  					 1, 1, 3, 1, 1, 1, 1, 9, 1, 1, 
				  					 1, 1, 1, 1, 8, 1, 1, 9, 7, 1, 
				  					 2, 1, 1, 1, 1, 1, 1, 1, 1, 7, 
				  					 1, 3, 8, 1, 1, 1, 1, 1, 1, 6, 
				  					 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 
				  					 3, 1, 8, 3, 1, 6, 1, 6, 1, 1, 
				  					 3, 1, 1, 1, 1, 8, 5, 3, 3, 1, 
				  					 1, 3, 1, 1, 1, 1, 1, 6, 1, 2, 
				  					 1, 1, 1, 1, 1, 4, 1, 6, 1, 1, 
				  					 1, 2, 3, 1, 4, 7, 6, 1, 1, 1, 
				  					 1, 1, 1, 9, 1, 2, 1, 1, 1, 1, 
				  					 2, 1, 8, 1, 8, 1, 3, 2, 1, 1, 
				  					 1, 1, 2, 6, 1, 1, 1, 2, 1, 3, 
				  					 1, 1, 8, 1, 4, 1, 1, 1, 1, 1, 
				  					 1, 1, 1, 1, 1, 1, 9, 1, 5, 1, 
				  					 1, 1, 1, 1, 5, 1, 1, 1, 3, 5, 
				  					 1, 1, 1, 1, 1, 1, 3, 1, 1, 1, 
				  					 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
				  					 3, 6, 1, 8, 1, 2, 5, 2, 1, 1, 
				  					 7, 1, 1, 1, 8, 1, 1, 5, 1, 1, 
				  					 1, 3, 1, 1, 1, 9, 1, 1, 1, 5, 
				  					 9, 1, 5, 3, 4, 1, 1, 1, 1, 1, 
				  					 1, 7, 1, 1, 1, 1, 3, 3, 1, 3, 
				  					 1, 1, 1, 1, 1, 1, 3, 8, 1, 4, 
				  					 1, 4, 1, 1, 1, 6, 1, 3, 1, 1, 
				  					 2, 3, 1, 1, 1, 1, 1, 1, 1, 1, 
				  					 9, 1, 1, 2, 2, 8, 1, 1, 1, 1, 
				  					 1, 4, 1, 1, 1, 1, 1, 1, 2, 1, 
				  					 1, 1, 1, 4, 3, 1, 1, 4, 1, 6, 
				  					 2, 1, 5, 3, 1, 1, 2, 1, 1, 1, 
				  					 1, 1, 1, 8, 3, 4, 8, 1, 3, 7, 
				  					 7, 1, 1, 2, 1, 1, 1, 1, 1, 4, 
				  					 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
				  					 1, 3, 9, 1, 1, 1, 1, 8, 1, 7, 
				  					 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 
				  					 1, 1, 2, 9, 6, 3, 3, 1, 2, 1, 
				  					 8, 7, 1, 1, 1, 1, 1, 6, 3, 1, 
				  					 4, 5, 1, 6, 1, 1, 1, 1, 3, 1, 
				  					 1, 2, 1, 6, 3, 7, 1, 8, 3, 1, 
				  					 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
				  					 8, 1, 1, 1, 9, 1, 1, 1, 7, 1, 
				  					 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
				  					 1, 1, 1, 1, 3, 1, 1, 1, 1, 1, 
				  					 1, 1, 1, 6, 1, 1, 1, 1, 1, 1, 
				  					 2, 1, 1, 1, 2, 1, 7, 1, 1, 4, 
				  					 2, 1, 5, 1, 5, 1, 1, 1, 4, 6, 
				  					 1, 1, 4, 1, 2, 1, 1, 1, 9, 8, 
				  					 1, 9, 1, 1, 1, 1, 1, 1, 1, 3, 
				  					 1, 1, 1, 1, 1, 4, 1, 1, 1, 1, 
				  					 7, 1, 1, 1, 1, 1, 3, 1, 1, 8, 
				  					 1, 1, 6, 1, 1, 1, 2, 1, 1, 1, 
				  					 1, 1, 1, 2, 5, 1, 6, 3, 1, 4, 
				  					 3, 1, 7, 5, 1, 1, 1, 1, 1, 1, 
				  					 2, 1, 5, 9, 1, 1, 1, 2, 1, 1, 
				  					 1, 1, 3, 1, 8, 1, 1, 2, 5, 1, 
				  					 1, 5, 1, 4, 1, 1, 1, 1, 1, 1, 
				  					 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
				  					 1, 1, 1, 3, 1, 1, 3, 1, 5, 1, 
				  					 8, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
				  					 1, 1, 6, 1, 1, 1, 2, 1, 1, 2, 
				  					 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 
				  					 1, 1, 6, 1, 9, 5, 3, 1, 8, 1, 
				  					 1, 3, 1, 1, 1, 1, 1, 1, 1, 2, 
				  					 1, 1, 1, 8, 1, 1, 1, 1, 1, 1, 
				  					 1, 7, 1, 1, 1, 1, 1, 1, 1, 1, 
				  					 1, 1, 1, 1, 1, 2, 8, 1, 1, 1, 
				  					 6, 1, 1, 1, 1, 9, 1, 1, 1),
						Classe = c("Ruim", "Ruim", "Ruim", "Boa", "Ruim", "Ruim", "Boa", "Boa", "Boa", "Ruim", 
								   "Ruim", "Boa", "Ruim", "Ruim", "Boa", "Ruim", "Ruim", "Ruim", "Ruim", "Ruim",
								   "Boa", "Boa", "Ruim", "Ruim", "Boa", "Ruim", "Ruim", "Ruim", "Boa", "Boa", 
								   "Ruim", "Ruim", "Boa", "Boa", "Ruim", "Ruim", "Ruim", "Boa", "Ruim", "Ruim", 
								   "Boa", "Ruim", "Ruim", "Ruim", "Boa", "Ruim", "Ruim", "Boa", "Boa", "Ruim", 
								   "Boa", "Ruim", "Ruim", "Ruim", "Ruim", "Ruim", "Ruim", "Ruim", "Ruim", "Boa", 
								   "Boa", "Boa", "Boa", "Boa", "Ruim", "Ruim", "Ruim", "Ruim", "Ruim", "Boa", 
								   "Ruim", "Boa", "Boa", "Ruim", "Ruim", "Ruim", "Ruim", "Ruim", "Ruim", "Ruim", 
								   "Boa", "Ruim", "Boa", "Boa", "Ruim", "Boa", "Boa", "Boa", "Ruim", "Ruim", 
								   "Boa", "Ruim", "Ruim", "Ruim", "Ruim", "Ruim", "Ruim", "Boa", "Boa", "Boa", 
								   "Ruim", "Ruim", "Boa", "Ruim", "Ruim", "Ruim", "Ruim", "Boa", "Boa", "Boa", 
								   "Ruim", "Ruim", "Ruim", "Ruim", "Ruim", "Boa", "Ruim", "Ruim", "Ruim", "Ruim", 
								   "Boa", "Ruim", "Boa", "Ruim", "Boa", "Boa", "Boa", "Ruim", "Ruim", "Boa", 
								   "Ruim", "Ruim", "Boa", "Ruim", "Ruim", "Ruim", "Boa", "Boa", "Boa", "Ruim", 
								   "Ruim", "Ruim", "Boa", "Ruim", "Ruim", "Boa", "Boa", "Ruim", "Ruim", "Ruim", 
								   "Ruim", "Ruim", "Ruim", "Boa", "Ruim", "Ruim", "Ruim", "Boa", "Ruim", "Ruim", 
								   "Ruim", "Ruim", "Ruim", "Ruim", "Boa", "Boa", "Ruim", "Ruim", "Ruim", "Ruim", 
								   "Ruim", "Boa", "Boa", "Boa", "Ruim", "Boa", "Boa", "Ruim", "Boa", "Ruim", 
								   "Ruim", "Ruim", "Ruim", "Ruim", "Boa", "Ruim", "Ruim", "Ruim", "Boa", "Boa", 
								   "Ruim", "Ruim", "Boa", "Ruim", "Ruim", "Ruim", "Ruim", "Ruim", "Ruim", "Ruim", 
								   "Ruim", "Ruim", "Ruim", "Ruim", "Ruim", "Ruim", "Ruim", "Ruim", "Ruim", "Ruim", 
								   "Boa", "Boa", "Ruim", "Boa", "Ruim", "Ruim", "Boa", "Ruim", "Boa", "Ruim", 
								   "Boa", "Ruim", "Boa", "Ruim", "Boa", "Ruim", "Ruim", "Boa", "Ruim", "Ruim", 
								   "Ruim", "Ruim", "Ruim", "Boa", "Ruim", "Boa", "Boa", "Ruim", "Ruim", "Boa", 
								   "Boa", "Ruim", "Ruim", "Ruim", "Boa", "Ruim", "Ruim", "Ruim", "Ruim", "Ruim", 
								   "Ruim", "Boa", "Ruim", "Ruim", "Ruim", "Ruim", "Ruim", "Boa", "Ruim", "Boa", 
								   "Ruim", "Ruim", "Boa", "Ruim", "Ruim", "Boa", "Boa", "Boa", "Boa", "Boa", 
								   "Ruim", "Boa", "Ruim", "Ruim", "Ruim", "Boa", "Ruim", "Boa", "Boa", "Ruim", 
								   "Boa", "Boa", "Ruim", "Ruim", "Ruim", "Ruim", "Ruim", "Boa", "Boa", "Ruim", 
								   "Boa", "Ruim", "Ruim", "Ruim", "Ruim", "Boa", "Ruim", "Ruim", "Ruim", "Boa", 
								   "Boa", "Boa", "Ruim", "Ruim", "Ruim", "Boa", "Ruim", "Ruim", "Ruim", "Ruim", 
								   "Ruim", "Ruim", "Ruim", "Boa", "Ruim", "Ruim", "Ruim", "Boa", "Ruim", "Boa", 
								   "Ruim", "Boa", "Boa", "Boa", "Ruim", "Ruim", "Ruim", "Ruim", "Ruim", "Ruim", 
								   "Ruim", "Ruim", "Boa", "Boa", "Boa", "Ruim", "Boa", "Ruim", "Boa", "Ruim", 
								   "Boa", "Ruim", "Ruim", "Ruim", "Ruim", "Boa", "Ruim", "Ruim", "Ruim", "Boa", 
								   "Ruim", "Boa", "Ruim", "Ruim", "Boa", "Ruim", "Ruim", "Ruim", "Ruim", "Boa", 
								   "Ruim", "Ruim", "Boa", "Ruim", "Ruim", "Boa", "Ruim", "Boa", "Ruim", "Boa", 
								   "Ruim", "Ruim", "Boa", "Ruim", "Ruim", "Ruim", "Boa", "Ruim", "Ruim", "Ruim", 
								   "Ruim", "Boa", "Ruim", "Boa", "Ruim", "Boa", "Boa", "Ruim", "Boa", "Ruim", 
								   "Ruim", "Boa", "Ruim", "Ruim", "Boa", "Boa", "Ruim", "Boa", "Boa", "Ruim", 
								   "Boa", "Boa", "Ruim", "Ruim", "Ruim", "Ruim", "Ruim", "Ruim", "Boa", "Ruim", 
								   "Ruim", "Ruim", "Ruim", "Boa", "Ruim", "Boa", "Ruim", "Boa", "Boa", "Ruim", 
								   "Ruim", "Ruim", "Ruim", "Ruim", "Boa", "Ruim", "Ruim", "Ruim", "Ruim", "Ruim", 
								   "Boa", "Ruim", "Ruim", "Ruim", "Boa", "Ruim", "Ruim", "Ruim", "Boa", "Ruim", 
								   "Ruim", "Ruim", "Ruim", "Boa", "Ruim", "Ruim", "Ruim", "Ruim", "Boa", "Ruim", 
								   "Boa", "Ruim", "Ruim", "Ruim", "Boa", "Ruim", "Ruim", "Ruim", "Ruim", "Ruim", 
								   "Ruim", "Ruim", "Ruim", "Boa", "Ruim", "Boa", "Boa", "Ruim", "Ruim", "Ruim", 
								   "Ruim", "Ruim", "Ruim", "Ruim", "Ruim", "Ruim", "Ruim", "Ruim", "Ruim", "Boa", 
								   "Ruim", "Ruim", "Boa", "Ruim", "Boa", "Ruim", "Ruim", "Ruim", "Boa", "Boa", 
								   "Ruim", "Ruim", "Boa", "Ruim", "Ruim", "Ruim", "Ruim", "Ruim", "Boa", "Boa", 
								   "Boa", "Boa", "Boa", "Boa", "Boa", "Ruim", "Ruim", "Ruim", "Ruim", "Ruim", 
								   "Boa", "Ruim", "Boa", "Ruim", "Ruim", "Boa", "Ruim", "Ruim", "Ruim", "Ruim", 
								   "Boa", "Ruim", "Boa", "Ruim", "Ruim", "Ruim", "Ruim", "Ruim", "Ruim", "Boa", 
								   "Ruim", "Ruim", "Boa", "Ruim", "Ruim", "Ruim", "Ruim", "Ruim", "Ruim", "Boa", 
								   "Boa", "Ruim", "Ruim", "Ruim", "Boa", "Ruim", "Boa", "Boa", "Ruim", "Boa", 
								   "Boa", "Ruim", "Boa", "Boa", "Boa", "Boa", "Ruim", "Ruim", "Ruim", "Boa", 
								   "Ruim", "Ruim", "Boa", "Boa", "Ruim", "Boa", "Ruim", "Ruim", "Boa", "Ruim", 
								   "Boa", "Boa", "Boa", "Ruim", "Boa", "Ruim", "Ruim", "Ruim", "Boa", "Ruim", 
								   "Ruim", "Boa", "Ruim", "Boa", "Ruim", "Ruim", "Ruim", "Ruim", "Ruim", "Boa", 
								   "Ruim", "Ruim", "Ruim", "Ruim", "Ruim", "Boa", "Ruim", "Ruim", "Ruim", "Ruim", 
								   "Ruim", "Ruim", "Boa", "Boa", "Boa", "Ruim", "Boa", "Ruim", "Boa", "Ruim", 
								   "Boa", "Ruim", "Ruim", "Ruim", "Ruim", "Ruim", "Boa", "Ruim", "Ruim", "Ruim", 
								   "Ruim", "Boa", "Boa", "Ruim", "Ruim", "Ruim", "Ruim", "Ruim", "Boa", "Ruim", 
								   "Ruim", "Ruim", "Boa", "Ruim", "Ruim", "Ruim", "Ruim", "Ruim", "Ruim", "Boa", 
								   "Ruim", "Ruim", "Boa", "Ruim", "Boa", "Boa", "Boa", "Boa", "Boa", "Boa", 
								   "Ruim", "Boa", "Ruim", "Ruim", "Ruim", "Ruim", "Boa", "Ruim", "Ruim", "Boa", 
								   "Ruim", "Ruim", "Ruim", "Boa", "Boa", "Ruim", "Ruim", "Ruim", "Boa", "Boa", 
								   "Ruim", "Boa", "Ruim", "Ruim", "Boa", "Boa", "Boa", "Ruim", "Ruim", "Ruim", 
								   "Boa", "Ruim", "Ruim", "Ruim", "Ruim", "Ruim", "Boa", "Ruim", "Boa", "Ruim", 
								   "Boa", "Ruim", "Ruim", "Boa", "Ruim", "Boa", "Boa", "Boa", "Ruim"))


# Converte variável resposta para factor
dados$Classe <- factor(dados$Classe, levels = c('Ruim','Boa'))

# Pequena analise exploratória
library(dplyr)
dados %>% group_by(Classe) %>% summarise_all("mean")

# Ajusta regressão logística
fit <- glm(Classe ~ Prova_Logica + Redacao + Auto_Avaliacao , data = dados, family = binomial)

# Visualiza resumo do modelo ajustado
summary(fit)

# Aplica exponenciação nos coeficientes para interpretação
exp(fit$coefficients)

# Curva ROC
prob = predict(fit, newdata = dados, type = "response")
roc = roc(dados$Classe ~ prob, plot = TRUE, print.auc = TRUE)

# Obtém a predição/probabilidade para cada observação
Probabilidade <- predict(fit, newdata= dados,type = 'response')

# Se a probabilidade for maior que 50% classifica como 'Boa'
Classe_Predita <- ifelse(Probabilidade > 0.5,"Boa","Ruim")

# Visualiza data frame com as predições
View(data.frame(dados,Probabilidade,Classe_Predita))

# Gera matriz de confusão
confusao <- table(Classe_Predita = Classe_Predita, Classe_Original = relevel(dados$Classe,ref = 'Boa'))
confusao

# Armazena valores da matriz de confusão (LINHA X COLUNA)
vp <- confusao[1,1];vp 
fn <- confusao[2,1];fn
vn <- confusao[2,2];vn 
fp <- confusao[1,2];fp

# Calcula a acurácia
acuracia <- sum(diag(confusao))/ sum(confusao);acuracia

# Calcula Sensitividade
sensitividade <- vp /(vp+fn)

# Calcula Especificidade
especificidade <- vn / (vn + fp)

# Analise de Sensitividade e Especificidade
limiares <- sort(Probabilidade)
acuracia <- c() 					# inicia um vetor vazio
sensitividade <- c() 				# inicia um vetor vazio
especificidade <- c()				# inicia um vetor vazio


for ( i in 1:length(limiares)) {
		limiar_atual <- limiares[i]
		Classe_Predita <- ifelse(Probabilidade > limiar_atual,'Boa' , 'Ruim')

# Gera matriz de confusão
confusao <- table(Classe_Predita = Classe_Predita, Classe_Original = relevel(dados$Classe,ref = 'Boa'))

# Armazena valores da matriz de confusão
vp <- confusao[1,1];vp 
fn <- confusao[2,1];fn
vn <- confusao[2,2];vn 
fp <- confusao[1,2];fp

# Calcula a acurácia
acuracia[i] <- sum(diag(confusao))/ sum(confusao);acuracia

# Calcula a Sensitividade
sensitividade[i] <- vp /(vp+fn)

# Calcula a Especificidade
especificidade[i] <- vn / (vn + fp)
}


# Gerando os Gráficos
plot(y = sensitividade[1:698] , x = limiares[1:698], type="l", col="red", ylab = 'Sensitividade e Especificidade', xlab= 'Pontos de Corte')
grid()
lines(y = especificidade[1:698], x = limiares[1:698], type = 'l',col="blue" )
legend("bottomleft", c("sensibilidade","especificidade"), col=c("red","blue"), lty=c(1,1),bty="n", cex=1, lwd=1)
abline(v=0.225)



# Obtém novamente as probabilidades para classificar baseado no ponto de corte 22,5%
Probabilidade <- predict(fit, newdata= dados,type = 'response') 
Classe_Predita <- ifelse(Probabilidade > 0.225,"Boa","Ruim")
View(data.frame(dados,Probabilidade,Classe_Predita))

# Visualiza matriz de confusão final
confusao <- table(Classe_Predita = Classe_Predita, Classe_Original = relevel(dados$Classe,ref = 'Boa'))
confusao

# Armazena valores da matriz de confusão
vp <- confusao[1,1];vp
fn <- confusao[2,1];fn
vn <- confusao[2,2];vn 
fp <- confusao[1,2];fp

# Calcula a Acurácia final
acuracia <- sum(diag(confusao))/ sum(confusao);acuracia

# Calcula a Sensitividade final
sensitividade <- vp /(vp+fn)

# Calcula a Especificidade final
especificidade <- vn / (vn + fp)

