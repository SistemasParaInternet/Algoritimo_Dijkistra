program Dijkistra
    
    !
    !--> Instituto Federal de Educação, Ciência e Tecnologia do Sudeste de Minas Gerais
    !    Câmpus Barbacena
    !
    !--> Disciplina: TCP-IP e Rotemaento
    !--> Professor: Herlon Ayres Camargo
    !--> Aluno: Paulo Vitor Francisco
    !--> Barbacena, 13 de Maio de 2013
    !
    !--> Programa Dijkistra.f95
    !
    ! --> Este programa implementa o algorítimo de Dijkistra para o cálculo do caminho mais curto
    ! entre os elementos de um grafo (formado por roteadores em uma rede), utilizando a linguegem 
    ! de programação Fortran, versão 95;
    !
    

    ! Estrutura referente aos nodos que pertencem ao caminho mais curto, da origem até o destino
    type elemento
        integer :: anterior !refere ao nodo anterior que está no caminho mais curto
        integer :: distancia !distância da origem até o nodo atual
        character*14 :: situacao !recebe os valores "processado" e "nao_processado"
    end type        
    
    ! Declaração de Variáveis
    integer, dimension(:,:), allocatable:: matriz !matriz que receberá asdistâncias, e que será alocada dinâmicamente na memória
    integer, parameter :: arq = 10 !parâmetro que define um nome (arq) para o valor 10 que irá referênciar o arquivo em disco
    integer, parameter :: INFINITO = 1000000 !valor definido como infinito
    integer, parameter :: MAXIMO = 4096 !máximo de nodos que a matriz de distâncias pode possuir
    integer :: buffer !variável que verificará se foi passado algum valor como argumento pelo terminal (nome do arquivo de rotas)
    character*255 :: argv, nome_arquivo !parâmetro que recebe a localização do arquivo de rotas no disco (passado como argumento)
    integer :: tamanho !tamanho da matriz, que será obtida ao ler a primeira linha do arquivo em disco
    character*2048 :: l !variável que receberá o conteúdo lido de cada linha do arquivo
    integer:: linha, coluna, erro, fim_arquivo, n, pos1, pos2, terminou, origem, destino, minimo, indice, cont !variáveis de controle
    type (elemento), dimension(:), allocatable:: caminho_curto !vetor dinâmico que receberá os nodos que compõe o caminho mais curto
    
    
    ! >>>>> Início do programa principal <<<<<
    
    !limpa a tela do terminal (Linux)
    call system('clear')
    !limpa a tela do terminal (Windows)
    !call system('cls')
    
    print *, ''
    print *, ''
    print *, '|---------------------------------------------------------------|'
    print *, '| Algorítimo de Djikstra - Linguagem Fortran 95                 |'
    print *, '| Disciplina de TCP e Roteamento - IFET Campus Barbacena - 2013 |'
    print *, '|---------------------------------------------------------------|'    
    print *, ''
    print *, ''
    
    !verifica se foi passado o argumento pela linha de comando referente a localização do arquivo de rotas
    buffer = iargc()
    
    if (buffer < 3) then
        print *, 'Forneça a localização do arquivo de rotas e os nodos de origem e destino como argumentos pelo terminal.'
        print *, ''
        print *, ''
        stop
    end if
    
    !obtem a localização do arquivo de rotas, que foi passado como parâmetro no terminal
    call getarg(1, argv)
    read(argv, '(A)') nome_arquivo

    !Abertura do arquivo contendo as distâncias
    open(unit=arq, file=nome_arquivo, status='old', iostat=erro, access='sequential')
    
    !Verifica se o arquivo foi aberto com sucesso
    if (erro == 0) then

        !Aloca um espaço na memória para uma matriz dinâmica (ponteiro para uma matriz de valores inteiros)
        allocate(matriz(MAXIMO,MAXIMO))
        
        linha = 1
        coluna = 1
        cont = 0
        
        !faz a leituta do arquivo para obter os valores que preencherão a matriz
        do !1
            read(arq,'(A)', iostat=fim_arquivo) l
            if (fim_arquivo < 0) then
                exit
            end if
            
            !tokeniza a linha lida do arquivo para obter os valores das distâncias entre os elementos do grafo
            pos1 = 1
            n = 1
            do !2
                pos2 = index(l(pos1:), ' ')
                if (l(pos1:) == ' ' .and. l(pos1+1:) == '') exit
                if (pos2 == 0) then
                    read(l(pos1:), '(i10)') matriz(linha,n)
                    exit
                end if
                
                !armazena o valor extraido da string para a matriz de inteiros.
                read(l(pos1:pos1+pos2-2), '(i10)') matriz(linha,n)
                n = n + 1
                pos1 = pos2+pos1
                if(linha == 1) then
                    cont = cont + 1
                end if
             end do !2
             linha = linha + 1
        end do !1
        
        tamanho = n - 1
        
        !verifica se a matriz é uma matriz quadrada
        if ( (tamanho /= cont) .or. (tamanho <= 1) ) then
            print *, ''
            print *, 'A matriz fornecida não é uma matriz quadrada válida.'
            stop            
        end if

    else !se o arquivo não for aberto com sucesso
        print *,'Erro na abertura do arquivo ou localização incorreta.'
        print *,''
        print *,''
        stop
    end if
    
    terminou = 0
    
    !obtém o valor do nodo de origem
    call getarg(2, argv)
    read(argv, '(i10)', iostat=erro) origem
    if ( (origem > tamanho) .or. (origem < 1) .or. (erro /= 0) ) then
        print *, ''
        print *, ''
        print *, 'Valor de origem inválido.'
        print *, ''
        print *, ''
        terminou = 1
    end if
    
    !obtém o valor do nodo de destino
    call getarg(3, argv)
    read(argv, '(i10)', iostat=erro) destino
    if ( (destino > tamanho) .or. (destino < 1) .or. (erro /= 0)) then
        print *, ''
        print *, ''
        print *, 'Valor de destino inválido.'
        print *, ''
        print *, ''
        terminou = 1
    end if
    
    !se o valor de origem e/ou destino estiverem incorretos, o programa será finalizado
    if (terminou == 1) stop
    
    !inicialização do vetor de caminhos mais curtos com valores padrão
    allocate(caminho_curto(MAXIMO))
    indice = 1
    do
    	if (indice > MAXIMO) exit 
        caminho_curto(indice)%anterior = -1
        caminho_curto(indice)%distancia = INFINITO
        caminho_curto(indice)%situacao = 'nao_processado'
        indice = indice + 1
    end do
    
    !inicializa o vetor do caminho mais curto com os dados referentes ao nodo de origem
    caminho_curto(origem)%distancia = 0
    caminho_curto(origem)%situacao = 'processado'
    
    ! --> Cálculo do caminho mais curto <--

    linha = origem
    terminou = 0
    
    !procura um caminho melhor para chegar até o nodo que está representado na variável "linha"
    do !1
    
        !finaliza o loop quando todos os nodos forem visitados
    	if (terminou == tamanho) exit
    	
    	coluna = 1
    	do !2
    	
			if(coluna > tamanho) exit
			
			!verifica se o nodo atual não é o nodo de origem ou se o nodo atual ainda não foi processado
			if((matriz(linha,coluna)/=INFINITO).and.(matriz(linha,coluna)/=0).and.(caminho_curto(coluna)%situacao=='nao_processado')) then
				
				!verifica se a distância do nodo até a origem é maior que a distância de nodo vizinho + a distância
				!entre o nodo atual até o nodo anterior
				if (caminho_curto(coluna)%distancia > caminho_curto(linha)%distancia + matriz(linha,coluna)) then
					caminho_curto(coluna)%distancia = caminho_curto(linha)%distancia + matriz(linha,coluna)
					caminho_curto(coluna)%anterior = linha
				end if
			end if
			coluna = coluna + 1
			
    	end do !2
    	
    	!localiza o nodo de menor distância que possui o campo "situação" como "não_processado"
    	indice = 1
    	minimo = INFINITO
    	do !3
    		
    		if(indice > tamanho) exit
    		
    		!procura o nodo com menor distância para marcá-lo como processado
    		if((caminho_curto(indice)%situacao=='nao_processado').and.((caminho_curto(indice)%distancia<minimo))) then
    			minimo = caminho_curto(indice)%distancia
    			linha = indice
    		end if
    		indice = indice + 1
    	end do !3
    	
    	!altera o status o nodo de menor distância como "processado"
    	caminho_curto(linha)%situacao = 'processado'
    	terminou = terminou + 1

    end do !1
    
    !imprime uma tabela com todos os caminhos mais curtos de todos os nodos até o nodo de origem
    linha = 1
    print *, ''
    print *, '------------------------------------------------------------------'
    print *, '| Nodo atual    | Nodo anterior | Distância até o nodo de origem |'
    print *, '------------------------------------------------------------------'
    
    do
    	if(linha > tamanho) exit
    	print *, '| ', linha, ' | ', caminho_curto(linha)%anterior , ' | ', caminho_curto(linha)%distancia
  	    print *, '------------------------------------------------------------------'
    	linha = linha + 1
    end do
    
    !verifica se há conectividade entre os nodos de origem e de destino
    if ( (origem /= destino).and.(caminho_curto(2)%anterior == -1).or.(caminho_curto(destino)%distancia == INFINITO) ) then
        print *, 'Não há uma rota entre o nodo de origem e de destino.'
        stop
    end if
        
    !imprime o menor caminho do nodo de destino até o nodo de origem
    print *, 'Tamanho da matriz de distãncias: ', tamanho, 'x', tamanho
    print *, 'Nodo de destino: ', destino
    print *, 'Nodo de origem: ', origem
    print *, ''
    print *, 'Caminho mais curto do nodo de destino até o nodo de origem: '
    
    linha = destino
    print *, '--> ', destino
    do
        if (linha == origem) exit
        print *, '--> ', caminho_curto(linha)%anterior
        linha = caminho_curto(linha)%anterior
    end do
    print *, 'Distância = ', caminho_curto(destino)%distancia
    
    !fecha o arquivo
    close(arq)
    
    !libera o espaço de memória ocupado pelos ponterios
    deallocate(matriz)
    deallocate(caminho_curto)
    
    ! >>>>> Fim do programa principal <<<<<
end 
