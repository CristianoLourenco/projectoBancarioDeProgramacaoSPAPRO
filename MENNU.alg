
//registar a conta de cadastro
registo Conta 
	literal numeroConta, codigoCliente
	inteiro tipoConta
	logico estado
fimregisto 

registo Pessoa
	literal codigo, nome, biOuNif
	inteiro telefone
	Conta contaCliente
fimregisto


	inteiro tamanhoClienteNoFicheiro <- 0, espacoClienteNoArray <- 10
	
// ------------------   Modulos para o ficheiro ---------------------- //
	funcao fficheiro criarFicheiro()	
		fficheiro ficheiro
		literal caminho <- "E:\\Ambiente de trabalho\\classDocs\\2 ano\\Programação 1\\projecto de programaçao\\projectoBancarioDeProgramacaoSPAPRO\\BD.txt"
		logico ficheiroExiste <- fExiste(caminho)
			se nao ficheiroExiste entao
				fcriar(caminho)
			fimse
		fAbrir(ficheiro , caminho, FALSO)
		retorne ficheiro
	fimFuncao

	//	Este procedimento conta o numero de linhas do meu ficheiro
	funcao inteiro contarLinhas(fficheiro file) 
		// A variavel tamanho diz o numero de clientes que estao no meu ficheiro
		inteiro linhas
		enquanto ftemlinha(file) faca
	       fLerLinha(file)
	       linhas <- linhas + 1
		fimenquanto
		 ffechar(file)
		 
		 retorne linhas
	fimFuncao

	
	// Esta funcao retorna um vector com todos os dados no ficheiro
	funcao pessoa[] lerFicheiro(fficheiro file)
		inteiro i

		tamanhoClienteNoFicheiro <- contarLinhas(file)

		// Abrindo novamente o ficheiro
		file <- criarFicheiro()
		
		Pessoa clienteNoFicheiro[tamanhoClienteNoFicheiro + espacoClienteNoArray]
			
		enquanto ftemlinha(file) faca 
	     literal content <-  fLerLinha(file)
	     literal data[] <- strRepartir(content, ",")
	      
	      para i de 0 ate atamanho(data, 0)
	      	clienteNoFicheiro[i].codigo<- data[0]
			clienteNoFicheiro[i].nome <- data[1]
			clienteNoFicheiro[i].biOuNif <- data[2]
			clienteNoFicheiro[i].telefone <- strValorInt(data[3])
//			clienteNoFicheiro[i].nif <-  data[2]
	      fimpara

		fimenquanto
		ffechar(file)
		
		retorne clienteNoFicheiro
	fimFuncao

	// Salvar clientes no Ficheiro
	procedimento salvarNoFicheiro(Pessoa p)
		fficheiro file <- criarFicheiro()	
			fescrever (file,p.codigo+","+  p.nome+","+ p.biOuNif+","+ strIntParaStr(p.telefone)+"\n")
		ffechar(file)
	fimprocedimento
	
//**--------------------------FINAL DOS MODELOS DO FICHEIRO ------------------------------------------------------------------------



	Pessoa clientes[] <- lerFicheiro(criarFicheiro())
 
// ----------- Modulo para cadastrar cliente -----------------  //
	
	//Ao fazer o cadastro o atributo telefone nao deve terminar com espacos
 	funcao logico cadastrarCliente(Pessoa p)
 		// Esta variavel nos permite achar a posicao do cliente a ser cadastrado
 		inteiro posicaoNoArray
 		logico clienteCadastrado
 		
 		se tamanhoClienteNoFicheiro >= espacoClienteNoArray entao 
 			// Isto nos permite registrar os clientes na linha a seguir dos que ja se encontram no ficheiro
			posicaoNoArray <- tamanhoClienteNoFicheiro + 1
 		senao
			posicaoNoArray <- tamanhoClienteNoFicheiro
		
 		fimse
 		
		se tamanhoClienteNoFicheiro = (atamanho(clientes,0)-1) entao
			mostre "Sem espaço -> Reinicie o programa!\n"
			clienteCadastrado <- falso
		senao
			clientes[posicaoNoArray] <- p
			salvarNoFicheiro(p)
			tamanhoClienteNoFicheiro <- tamanhoClienteNoFicheiro + 1
			clienteCadastrado <- verdadeiro
		fimse
		
		retorne clienteCadastrado
	fimFuncao 


	funcao logico actualizarDadosCliente(Pessoa p, inteiro codigoCliente)
		inteiro  linhas ,i
		fficheiro file <- criarFicheiro()
		linhas <- contarlinhas(file)
		

		para i de 0 ate linhas
			se i = codigoCliente entao
			fescrever(file, p.nome+","+ p.biOuNif+","+ strIntParaStr(p.telefone)+"\n")
			fimse
		fimPara
		ffechar(file)
	retorne verdadeiro
	fimFuncao 

	
	// Modelo Abertura de Conta
	funcao logico abrirConta(Pessoa p, inteiro tipoConta)
	//1 com base no biOuNif encontrar o cliente
	//2 procurar no meu vector um cliente onde o seu biOuNif seja igual ao biOuNif informado
		
		conta contaCliente
		inteiro i <- 0 
	
		repita 
//			se  clientes[i].biOuNif <> p.biOuNif entao
//				p.codigo <- strIntParaStr(tamanhoClienteNoFicheiro)
//				cadastrarCliente(p)
//			 
			se clientes[i].biOuNif = p.biOuNif entao
					contaCliente.estado <- verdadeiro
					contaCliente.tipoConta <- tipoConta 
					
				 	//	Para cliente particular
					se contaCliente.tipoConta  = 0 entao 
						contaCliente.numeroConta <- "2000" + clientes[i].codigo
					senao
					//Para cliente Empresa
						se contaCliente.tipoConta  = 1 entao 
							contaCliente.numeroConta <- "1000" + clientes[i].codigo
						fimse
					fimse			
				contaCliente.codigoCliente <- clientes[i].codigo
  				clientes[i].contaCliente <- contaCliente
  				actualizarDadosCliente(clientes[i],strValorInt(clientes[i].codigo))
			fimse
			i <- i + 1
		ate i = tamanhoClienteNoFicheiro
		retorne verdadeiro 
	fimFuncao

	procedimento listarClientes()
		inteiro i
		para i de 0 ate (tamanhoClienteNoFicheiro-1)
			mostre "\n"
			mostre "Nome: ", Clientes[i].nome, ","
			mostre "Bi/Nif: ", Clientes[i].biOuNif,","
			mostre "Telefone: ", Clientes[i].telefone
			mostre "----------------------------------"
		fimPara
	fimProcedimento
//-------------------------------------------------------------------------

inicio

	inteiro tecla, tipoConta, i
	literal biOuNif
	Pessoa cliente

	enquanto tecla <> 9 faca
		escreva "              MENU                   \n===================================\n"
		escreva "1- Cadastrar Cliente\n"
		escreva "2- Listar Clientes\n"
		escreva "3- Abertura de Conta\n"
		escreva "4- Depositar Dinheiro\n"
		escreva "5- Levantar Dinheiro\n"
		escreva "6- Consultar Saldo\n"
		escreva "7- Transferir Dinheiro\n"
		escreva "8- Actualizar Dados do Cliente\n"
		escreva "9- Terminar\n"
		escreva "==================================\n"
		escreva "\nOpção: "
		
		
		leia tecla

		escolha(tecla)
		caso 1:  					//Modulo cadastrar cliente
			mostre "Digite o nome: "
			receba cliente.nome
			mostre "Digite o Bi/Nif: "
			receba cliente.biOuNif
			mostre "Digite o telefone: "
			receba cliente.telefone
			cliente.codigo <- strIntParaStr(tamanhoClienteNoFicheiro)

			limpatela
			cadastrarCliente(cliente)
			
		caso 2:   					//Modulo Listar clientes
			mostre "Operacao 2\n\n"
		caso 3:						//Modulo Abertura de conta
			mostre "Digite o nome: "
			receba cliente.nome
			mostre "Digite o Bi/Nif: "
			receba cliente.biOuNif
			mostre "Digite o telefone: "
			receba cliente.telefone
			mostre "Informe o Tipo de Conta\n"
			mostre "0 - Particular\n"
			mostre "1 - Empresa\n"
			mostre "Tipo: "
			
			leia tipoConta
			 se tipoConta = 0 ou tipoConta = 1 entao
					 abrirConta(cliente, tipoConta)
			senao 
					mostre "Falha no processo, repita a operação!\nPressione qualquer tecla"
					leia i
 			fimSe
			limpaTela
		caso 4:						//Modulo Depositar dinheiro
			mostre "Operacao 4\n\n"
		caso 5:						//Modulo Levantar dinheiro
			mostre "Operacao 5"
		caso 6:						//Modulo Consultar dinheiro
			mostre "Operacao 6"
		caso 7:						//Modulo Actualizar dinheiro
			mostre "Operacao 7"
			mostre "Digite o nome: "
			receba cliente.nome
			mostre "Digite o Bi/Nif: "
			receba cliente.biOuNif
			mostre "Digite o telefone: "
			receba cliente.telefone
			
			actualizarDadosCliente(cliente,strValorInt(clientes[i].codigo))
		caso 8:			//Modulo Listar Dados do cliente
			limpaTela
			listarClientes()
		caso 9:						//TERMINAR
			
		defeito:
			limpatela
			
		fimescolha
	fimenquanto

fimalgoritmo