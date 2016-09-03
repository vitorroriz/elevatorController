;====================== Inicializacoes, lembrar de salvar interrupcoes originais =======================
segment code
..start:
	mov		ax,data
	mov		ds,ax
	mov		ax,stack
	mov		ss,ax
	mov		sp,stacktop

; salvar modo corrente de video
	mov		ah,0Fh
	int		10h
	mov		[modo_anterior],al	 

; alterar modo de video para gráfico 640x480 16 cores
	mov		al,12h
	mov		ah,0
	int		10h

	xor		ax, ax             
	mov		es, ax
	mov		ax, [es:int9*4]		
	mov		[offset_dos], ax	; salvar IP da interrupcao original do teclado
	mov		ax, [es:int9*4+2]	
	mov		[cs_dos], ax 		; salvar CS da interrupcao original do teclado


	cli		;desabilitar interrupcoes para carregar nosso vetor
	mov		[es:int9*4+2], cs 	
	mov		word [es:int9*4], keyint   ;novo IP do par CS:IP 
	sti 	;habilitar novamente as interrupcoes



;   ==================== Inicializar saidas  ==================================

	mov 	dx, psaida2
	mov 	al, 00000001; 319h = e/s x x x x x x led    318h como saida e led apagado (led = 1 eh apagado)
	out 	dx,	al

	mov		dx, psaida1  ; 318h (M1,M2,L6,L4,L2,L5,L3,L1)
	mov		al, byte[saida1] ;saida1 inicia com 01000000b, M1 = 0 e M2 = 1 faz elevador subir
	out		dx, al

	;Mensagem inicial de posicionamento do elevador no quarto andar
		mov		byte[cor],branco_intenso
		mov     	cx,31			;numero de caracteres
    	mov     	bx,0
    	mov     	dh,15			;linha 0-29
    	mov     	dl,25			;coluna 0-79
l0:
		call	cursor
    	mov     al,[bx+mens0]
		call	caracter
    	inc     bx			;proximo caracter
		inc		dl			;avanca a coluna
    	loop    l0
	
; espera o elevador subir
;funcao delay do laboratorio com tempo suficiente para pior situacao (quando o elevador estiver no primeiro andar)

	mov		cx,3300
loopgordo:

	push	cx
	mov		cx,50000
loopgordo2:
	loop	loopgordo2 
	pop		cx
	
	loop	loopgordo
	
 sairgordo:

 ;===== Inicializar descida do elevador pro quarto andar

	 and		byte[saida1], 10111111b  ;zera M2 e guarda o estado dos LEDs
	 or			byte[saida1], 10000000b  ;M1 = 1 e M2 = 0, garante que vai descer e permanece com o estado dos LEDs
	 mov		dx, psaida1
	 mov		al, byte[saida1]
	 out		dx, al
	
	 push	dx
	
	 and		byte[saida2], 01111111b  
	 mov		dx, psaida2
	 mov		al, byte[saida2]
	 out		dx, al

	  mov		cx, 9 		;conta 9 transicoes do sensor enquando desce, numero suficiente pra ir pro quarto andar
 esperadescer:
	  inc		byte[contadebounce]
 ; Entrada com debounce
	  cmp		byte[contadebounce], 20
	  jb		esperadescer
	  mov		byte[contadebounce], 0
	  mov		dx, pentrada     ; 319h (-,Sensor,B6,B4,B2,B5,B3,B1)
	  in		al, dx
	  and		al, 01000000b
	  cmp		al, 01000000b    ; checa se o sensor está obstruído
	  jne		contador_zero1
	  cmp		byte[podecontar], 1  ; checa 'podecontar'
	  jne		fim_contador1
	  mov		byte[podecontar], 0
	  inc		byte[contador]
	  jmp		fim_contador1
 contador_zero1:
	  mov		byte[podecontar], 1
 fim_contador1:
	  test	byte[contador], 1   ; checa 'contador' 
	  jz		esperadescer   ; je
	  mov		byte[contador], 0
	  loop	esperadescer
	
	pop		dx
	
	and		byte[saida1], 00111111b ; motor parado, estados dos LEDS guardados, estamos no quarto andar
	mov		al, byte[saida1]
	out		dx, al             
	
;Apaga o Aguarde, por favor
    	mov     	cx,31			;numero de caracteres
    	mov     	bx,0
    	mov     	dh,15			;linha 0-29
    	mov     	dl,25			;coluna 0-79
l00:
		call	cursor
    	mov     al,[bx+mens00]		;mensagem vazia com a msm quantidade de caracteres da msg inicial de espera
		call	caracter
    	inc     bx			;proximo caracter
		inc		dl			;avanca a coluna
    	loop    l00
		
		;=============================== Imprimindo mensagens fixas na interface ==========================
		
		;Vitor Ribeiro Roriz
    	mov     	cx,19			;numero de caracteres
    	mov     	bx,0
    	mov     	dh,28			;linha 0-29
    	mov     	dl,10			;coluna 0-79
l1:
		call	cursor
    	mov     al,[bx+mens1]
		call	caracter
    	inc     bx			;proximo caracter
		inc		dl			;avanca a coluna
    	loop    l1
		
		;Tarcisio Prest Bernabe
    	mov     	cx,22			;numero de caracteres
    	mov     	bx,0
    	mov     	dh,27			;linha 0-29
    	mov     	dl,10			;coluna 0-79
l2:
		call	cursor
    	mov     al,[bx+mens2]
		call	caracter
    	inc     bx			;proximo caracter
		inc		dl			;avanca a coluna
    	loop    l2

		;Sistemas Embarcados 1 - 2014\1
    	mov     	cx,30			;numero de caracteres
    	mov     	bx,0
    	mov     	dh,25			;linha 0-29
    	mov     	dl,10			;coluna 0-79
l3:
		call	cursor
    	mov     al,[bx+mens3]
		call	caracter
    	inc     bx			;proximo caracter
		inc		dl			;avanca a coluna
    	loop    l3
		
		;Mayara Nascimento de Oliveira
    	mov     	cx,29			;numero de caracteres
    	mov     	bx,0
    	mov     	dh,26			;linha 0-29
    	mov     	dl,10			;coluna 0-79
l4:
		call	cursor
    	mov     al,[bx+mens4]
		call	caracter
    	inc     bx			;proximo caracter
		inc		dl			;avanca a coluna
    	loop    l4	

		;Botoes internos:   1   2   3   4
    	mov     	cx,32			;numero de caracteres
    	mov     	bx,0
    	mov     	dh,4			;linha 0-29
    	mov     	dl,10			;coluna 0-79
l5:
		call	cursor
    	mov     al,[bx+mens5]
		call	caracter
    	inc     bx			;proximo caracter
		inc		dl			;avanca a coluna
    	loop    l5		
		
		
		;Botoes externos (subir):  1  2  3  4
    	mov     	cx,36			;numero de caracteres
    	mov     	bx,0
    	mov     	dh,6			;linha 0-29
    	mov     	dl,10			;coluna 0-79
l6:
		call	cursor
    	mov     al,[bx+mens6]
		call	caracter
    	inc     bx			;proximo caracter
		inc		dl			;avanca a coluna
    	loop    l6	
		
		
		;Botoes externos (descer):  1  2  3  4
    	mov     	cx,37			;numero de caracteres
    	mov     	bx,0
    	mov     	dh,8			;linha 0-29
    	mov     	dl,10			;coluna 0-79
l18:
		call	cursor
    	mov     al,[bx+mens18]
		call	caracter
    	inc     bx			;proximo caracter
		inc		dl			;avanca a coluna
    	loop    l18
		
		;Andar atual:
    	mov     	cx,12			;numero de caracteres
    	mov     	bx,0
    	mov     	dh,10			;linha 0-29
    	mov     	dl,10			;coluna 0-79
l7:
		call	cursor
    	mov     al,[bx+mens7]
		call	caracter
    	inc     bx			;proximo caracter
		inc		dl			;avanca a coluna
    	loop    l7
		
		
		;Status do elevador:
    	mov     	cx,19			;numero de caracteres
    	mov     	bx,0
    	mov     	dh,12			;linha 0-29
    	mov     	dl,10			;coluna 0-79
l8:
		call	cursor
    	mov     al,[bx+mens8]
		call	caracter
    	inc     bx			;proximo caracter
		inc		dl			;avanca a coluna
    	loop    l8			
	
; diz a situação do elevador	
		;Status do elevador: Parado
    	mov     	cx,6			;numero de caracteres
    	mov     	bx,0
    	mov     	dh,12			;linha 0-29
    	mov     	dl,32			;coluna 0-79
l16:
		call	cursor
    	mov     al,[bx+mens16]
		call	caracter
    	inc     bx			;proximo caracter
		inc		dl			;avanca a coluna
    	loop    l16	

	
main:

; Ler os caracteres digitado pelo usuário
le_teclado:
	; lê o teclado, fifo eh um vetor de 8 bytes
	mov		bx, word[p_m]	;p_m eh um ponteiro para fifo pelo programa principal
	cmp		bx, word[p_i]	;p_i eh um ponteiro para fifo pela interrupcao
	je		fim_le_teclador ;se nada foi buferizado nao precisa tratar o teclado
	inc		bx
	and		bx, 7
	mov		word[p_m], bx
	xor		ax, ax
	mov		al, byte[fifo+bx]
	
	cmp		al, bteclas 	;se for capturado o break do s vai pra rotina de tratamento de saida
	je		roubosair1

; TODO - tirar este roubo!
	jmp		sroubosair1
roubosair1:
	jmp		sair
sroubosair1:

	jmp		fim_le_tecladors
fim_le_teclador: jmp fim_le_teclado
fim_le_tecladors:

	jmp		le_tecladors 	


; checa se a tecla pressionada foi ESC
le_tecladors:
	cmp		al, mteclaesc	;compara com make do esc
	jne		le_teclado2aux
	cmp		byte[emergenciaflag], 0
	je		emergenciav
	cmp		byte[emergenciaflag], 1
	je		le_teclado
	cmp		byte[emergenciaflag], 3
	je		emergenciap
	mov		byte[emergencia], 0
	mov		byte[emergenciaflag], 3
	jmp		emergenciap

emergenciav:
;EMERGENCIA!
		mov		byte[cor],vermelho
    	mov     	cx,11			;numero de caracteres
    	mov     	bx,0
    	mov     	dh,14			;linha 0-29
    	mov     	dl,10			;coluna 0-79
l9:
		call	cursor
    	mov     al,[bx+mens9]
		call	caracter
    	inc     bx			;proximo caracter
		inc		dl			;avanca a coluna
    	loop    l9	
	jmp		comecaemergencia 
	
emergenciap:
;EMERGENCIA!
		mov		byte[cor],preto
    	mov     	cx,11			;numero de caracteres
    	mov     	bx,0
    	mov     	dh,14			;linha 0-29
    	mov     	dl,10			;coluna 0-79
l92:
		call	cursor
    	mov     al,[bx+mens9]
		call	caracter
    	inc     bx			;proximo caracter
		inc		dl			;avanca a coluna
    	loop    l92
	jmp		le_teclado 
	

; desliga o motor e informa o usuário da emergência	
comecaemergencia:
	mov		byte[emergenciaflag], 1
	mov		byte[emergencia], 1
	and		byte[saida1], 00111111b  ; motor desligado
	mov		dx, psaida1
	mov		al, byte[saida1]
	out		dx, al
	jmp		le_teclado
le_teclado2aux: jmp le_teclado2


le_teclador: jmp le_teclado
le_teclado2:
	cmp		al, bteclaesc
	jne		le_teclado3
	cmp		byte[emergenciaflag], 1
	jne		le_teclado22
	mov		byte[emergenciaflag], 2
	jmp		le_teclado
le_teclado22:
	cmp		byte[emergenciaflag], 3
	jne		le_teclador
	mov		byte[emergenciaflag], 0
	jmp		le_teclado
	

le_teclado3:
	cmp		al, btecla1 	;se o codigo lido eh inferior ao break da tecla 1 entao nao eh um dos 4 codigos break de 1234
	jb		le_teclador 	
	cmp		al, btecla4 	;se o codigo lido eh superior ao break da tecla 4 entao nao eh um dos 4 codigos break de 1234
	ja		le_teclador

	;se chegou aqui eh porque o codigo detectado eh de um dos breaks de 1234
	xor		ah, ah 			;limpa a parte alta de ax
	mov		bx, ax 			;bx = ah:al = al = codigo de tecla capturado
	sub		bx, btecla1 	;cria um indice de 0 a 3 (break1 a break4)

	mov		byte[chamadasint+bx], 1 	;no vetor de chamadas internas seta a chamada correspondente
	
	imul	bx, 3 	;
	add		bx, interno1	;bx = endereço das estrutura db internox linha,coluna,caracter do chamado interno x correspondente


	;deixa verde o numero correspondente no indicativo de teclas do teclado interno pressionadas
	cmp		al,btecla1
	je		botaoa1
	cmp		al,btecla2
	je		botaoa2
	cmp		al,btecla3
	je		botaoa3
	jmp		botaoa4

botaoa1:
		;Botoes internos:   1   2   3   4
		mov		byte[cor],verde
    	mov     	cx,1			;numero de caracteres
    	mov     	bx,0
    	mov     	dh,4			;linha 0-29
    	mov     	dl,29			;coluna 0-79
		call	cursor
    	mov     al,[bx+mens51]
		call	caracter	
		jmp		le_teclado
		
botaoa2:
		;Botoes internos:   1   2   3   4
		mov		byte[cor],verde
    	mov     	cx,1			;numero de caracteres
    	mov     	bx,0
    	mov     	dh,4			;linha 0-29
    	mov     	dl,33			;coluna 0-79
		call	cursor
    	mov     al,[bx+mens52]
		call	caracter
		jmp		le_teclado
		
botaoa3:
		;Botoes internos:   1   2   3   4
		mov		byte[cor],verde
    	mov     	cx,1			;numero de caracteres
    	mov     	bx,0
    	mov     	dh,4			;linha 0-29
    	mov     	dl,37			;coluna 0-79
		call	cursor
    	mov     al,[bx+mens53]
		call	caracter
		jmp		le_teclado
		
botaoa4:
		;Botoes internos:   1   2   3   4
		mov		byte[cor],verde
    	mov     	cx,1			;numero de caracteres
    	mov     	bx,0
    	mov     	dh,4			;linha 0-29
    	mov     	dl,41			;coluna 0-79
		call	cursor
    	mov     al,[bx+mens54]
		call	caracter
		jmp		le_teclado
		
fim_le_teclado:

; Entrada com debounce
	mov		dx, pentrada
	in		al, dx
	mov		byte[ultimaentrada], al
	mov		cx, 50
loopdebounce:
	loop	loopdebounce
	in		al, dx
	cmp		al, byte[ultimaentrada]
	jne		fimdebounce
	;só muda o valor de 'entrada' se o debounce funcionar
	mov		byte[entrada], al
fimdebounce:

; Verifica os botões apertados
	mov		byte[cor], verde
	;Verifica se B1 foi pressionado
	test	byte[entrada], 00000001b   ;Difere da instrução AND, uma vez que não coloca o resultado no operador de destino. 
	jz		verb2                      ;Tem efeito sobre o registrador de flag.
	or		byte[saida1], 00000001b
	mov		byte[chamadassub], 1
	mov		bx, externosob1
	
	;acende o botao externo 1
		;Botoes externos (subir):  1  2  3  4
		mov		byte[cor],verde
    	mov     	cx,1			;numero de caracteres
    	mov     	bx,0
    	mov     	dh,6			;linha 0-29
    	mov     	dl,36			;coluna 0-79
		call	cursor
    	mov     al,[bx+mens51]
		call	caracter

	

verb2:
	test	byte[entrada], 00001000b
	jz		verb3
	or		byte[saida1], 00001000b
	mov		byte[chamadasdesc+1], 1
	mov		bx, externodesc2 	
	
	;acende o botao externo 2
		;Botoes externos (descer):  1  2  3  4
		mov		byte[cor],verde
    	mov     	cx,1			;numero de caracteres
    	mov     	bx,0
    	mov     	dh,8			;linha 0-29
    	mov     	dl,40			;coluna 0-79
		call	cursor
    	mov     al,[bx+mens52]
		call	caracter


verb3:
	test	byte[entrada], 00000010b
	jz		verb4
	or		byte[saida1], 00000010b
	mov		byte[chamadassub+1], 1
	mov		bx, externosob2 	

		
	;acende o botao externo 2
		;Botoes externos (subir):  1  2  3  4
		mov		byte[cor],verde
    	mov     	cx,1			;numero de caracteres
    	mov     	bx,0
    	mov     	dh,6			;linha 0-29
    	mov     	dl,39			;coluna 0-79
		call	cursor
    	mov     al,[bx+mens52]
		call	caracter

		
verb4:
	test	byte[entrada], 00010000b
	jz		verb5
	or		byte[saida1], 00010000b
	mov		byte[chamadasdesc+2], 1
	mov		bx, externodesc3  
		
	;acende o botao externo 3
		;Botoes externos (descer):  1  2  3  4
		mov		byte[cor],verde
    	mov     	cx,1			;numero de caracteres
    	mov     	bx,0
    	mov     	dh,8			;linha 0-29
    	mov     	dl,43			;coluna 0-79
		call	cursor
    	mov     al,[bx+mens53]
		call	caracter
		
verb5:
	test	byte[entrada], 00000100b
	jz		verb6
	or		byte[saida1], 00000100b
	mov		byte[chamadassub+2], 1
	mov		bx, externosob3
	
	;acende o botao externo 3
		;Botoes externos (subir):  1  2  3  4
		mov		byte[cor],verde
    	mov     	cx,1			;numero de caracteres
    	mov     	bx,0
    	mov     	dh,6			;linha 0-29
    	mov     	dl,42			;coluna 0-79
		call	cursor
    	mov     al,[bx+mens53]
		call	caracter
  
		
verb6:
	test	byte[entrada], 00100000b
	jz		fimverb
	or		byte[saida1], 00100000b
	mov		byte[chamadasdesc+3], 1
	mov		bx, externodesc4
	
	;acende o botao externo 4
		;Botoes externos (descer):  1  2  3  4
		mov		byte[cor],verde
    	mov     	cx,1			;numero de caracteres
    	mov     	bx,0
    	mov     	dh,8			;linha 0-29
    	mov     	dl,46			;coluna 0-79
		call	cursor
    	mov     al,[bx+mens54]
		call	caracter
		
;acende os LEDS dos botoes pressionados conferidos acima
fimverb:

	mov		dx, psaida1
	mov		al, byte[saida1]
	out		dx, al


;Atualizar o contador do sensor
	mov		al,	byte[entrada]
	test	al, 01000000b
	jz		contador_zero
	cmp		byte[podecontar], 1
	jne		fim_contador
	mov		byte[podecontar], 0
	inc		byte[contador]
	jmp		fim_contador
contador_zero:
	mov		byte[podecontar], 1
fim_contador:


;========================= IDENTIFICACAO DO ANDAR =============================================
; Verificar se chegou em algum andar
	cmp		byte[contador], 89
	jb		naochegouandar
	mov		byte[contador], 0
	jmp		chegouandar
naochegouandar:
	test	byte[emergencia], 1
	jnz		fimemergenciar
	cmp		byte[situacao], 0
	jne		movendorr1
;	jmp		movendor1
	jmp		fchamadas
chegouandar:
	
; Caso tenha chegado incrementa ou decrementa o andar dependendo de como o elevador esta andando
	cmp		byte[situacao], 1
	jne		vda2
	inc		byte[andar]
	jmp		vaf
vda2:
	dec		byte[andar]
vaf:

; TODO - roubo, tirar
	jmp	sfimemergenciar
fimemergenciar: jmp fimemergencia
sfimemergenciar:

	jmp	smovendor1
movendorr1:	jmp	movendor1
smovendor1:

fchamadas:
; Atualiza o andar na tela
	xor		bh, bh
	mov		bl, byte[andar]
	cmp		bl,4
	je		andar4
	cmp		bl,3
	je		andar3
	cmp		bl,2
	je		andar2
	jmp		andar1
	
andar4:
		;Andar atual: 4
		mov		byte[cor],branco_intenso
    	mov     	cx,1			;numero de caracteres
 ;   	mov     	bx,0
    	mov     	dh,10			;linha 0-29
    	mov     	dl,23			;coluna 0-79
		call	cursor
    	mov     al,[mens54]
		call	caracter
		jmp		proximo
		
andar3:	
		;Andar atual: 3
		mov		byte[cor],branco_intenso
    	mov     	cx,1			;numero de caracteres
 ;   	mov     	bx,0
    	mov     	dh,10			;linha 0-29
    	mov     	dl,23			;coluna 0-79
		call	cursor
    	mov     al,[mens53]
		call	caracter
		jmp		proximo
   		
andar2:		
		;Andar atual: 2
		mov		byte[cor],branco_intenso		
    	mov     	cx,1			;numero de caracteres
  ;  	mov     	bx,0
    	mov     	dh,10			;linha 0-29
    	mov     	dl,23			;coluna 0-79
		call	cursor
    	mov     al,[mens52]
		call	caracter
		jmp		proximo
		
andar1:		
		;Andar atual: 1
		mov		byte[cor],branco_intenso
    	mov     	cx,1			;numero de caracteres
    ;	mov     	bx,0
    	mov     	dh,10			;linha 0-29
    	mov     	dl,23			;coluna 0-79
		call	cursor
    	mov     al,[mens51]
		call	caracter

proximo:
	dec		bx ; Decrementa bx para usar como indice nos vetores

; Verificar se este andar foi requisitado
	test	byte[chamadasint+bx], 1
	jnz		parar_aqui ; Para caso tenha uma chamada interna neste andar
	cmp		byte[situacao], 1
	je		vsub ; Se não estiver subindo verifica as situações de descida
	cmp		byte[situacao], 2
	je		vdsc

	test	byte[chamadassub+bx], 1
	jnz		parar_aqui
	test	byte[chamadasdesc+bx], 1
	jnz		parar_aqui
	;test	byte[chamadasint+bx], 1
	;jnz		parar_aqui
	jmp		movendor1

vsub:
; O elevador está subindo
	mov		si, externosob1
	test	byte[chamadassub+bx], 1
	jnz		parar_aqui ; Para caso tenha uma chamada de subida neste andar
	inc		bx
	call	verifica_acima
	dec		bx
	test	al, 1
	jnz		movendor1 ; Se houver chamadas acima deste andar ele deve continuar subindo
	jmp		parar_aqui

vdsc:
; O elevador está descendo
	mov		si, externodesc1
	test	byte[chamadasdesc+bx], 1
	jnz		parar_aqui ; Para caso tenha uma chamada de descida neste andar
	inc		bx
	call	verifica_abaixo
	dec		bx
	test	al, 1
	jnz		movendor1 ; Se houver chamadas abaixo deste andar ele deve continuar descendo
	jmp		parar_aqui
movendor1:
	jmp		movendo

parar_aqui:

	cmp		byte[situacao], 0
	jne		naotaparado
	mov		byte[flagparado], 1
	jmp		naotaparadofim
naotaparado:
	mov		byte[flagparado], 0
	
naotaparadofim:

; Apagar o led do botao e as teclas necessarias
	mov		byte[chamadasint+bx], 0
	mov		cx, bx
	imul	bx, 3
	mov		dx, bx
	add		bx, interno1
	cmp		cx,0
	je		apaga1
	cmp		cx,1
	je		apaga2
	cmp		cx,2
	je		apaga3
	jmp		apaga4
apaga1:
	;Apaga botao interno:   1   2   3   4
		mov		byte[cor],branco_intenso
		push		cx
    	mov     	cx,1			;numero de caracteres
    ;	mov     	bx,0
    	mov     	dh,4			;linha 0-29
    	mov     	dl,29			;coluna 0-79
		call	cursor
    	mov     al,[mens51]
		call	caracter
		pop		cx
		jmp		continua2
apaga2:
	;Apaga botao interno:   1   2   3   4
		mov		byte[cor],branco_intenso
		push		cx
    	mov     	cx,1			;numero de caracteres
    ;	mov     	bx,0
    	mov     	dh,4			;linha 0-29
    	mov     	dl,33			;coluna 0-79
		call	cursor
    	mov     al,[mens52]
		call	caracter
		pop		cx
		jmp		continua2
apaga3:
	;Apaga botao interno:   1   2   3   4
		mov		byte[cor],branco_intenso
		push		cx
    	mov     	cx,1			;numero de caracteres
    ;	mov     	bx,0
    	mov     	dh,4			;linha 0-29
    	mov     	dl,37			;coluna 0-79
		call	cursor
    	mov     al,[mens53]
		call	caracter
		pop		cx
		jmp		continua2
apaga4:
	;Apaga botao interno:   1   2   3   4
		mov		byte[cor],branco_intenso
		push		cx
    	mov     	cx,1			;numero de caracteres
    ;	mov     	bx,0
    	mov     	dh,4			;linha 0-29
    	mov     	dl,41			;coluna 0-79
		call	cursor
    	mov     al,[mens54]
		call	caracter
		pop		cx
		
		
continua2:
	mov		bx, cx
	cmp		byte[situacao], 1
	jne		apagaleddescaux2 ; Apagar os leds no caso de descida
	
; Esta subindo
	mov		byte[chamadassub+bx], 0 ; Limpa a chamada de subida deste andar

;inicio gambiarra 2
	inc	 	bx
	call verifica_acima
	dec		bx
	
	cmp		al,0
	jne 	fim_gamb2

	mov		byte[flagc3],1
	
	cmp		bx,0
	je		apaga1111aux
	cmp		bx,1
	je		apaga2222aux
	cmp		bx,2
	je		apaga3333aux
	cmp		bx,3
	je		apaga4444aux
	
fim_gamb2:
	mov		byte[flagc3],0


;continuacao original
	cmp		bx,0
	je		apaga111
	cmp		bx,1
	je		apaga222
	cmp		bx,2
	je		apaga333
	cmp		bx,3
	je		apaga444
	jmp		continua3
apagaleddescaux2:
		jmp		apagaleddescaux
apaga111:
;apaga botao 1
		;Botoes externos (subir):  1  2  3
		mov		byte[cor],branco_intenso
		push		cx
    	mov     	cx,1			;numero de caracteres
    	;mov     	bx,0
    	mov     	dh,6			;linha 0-29
    	mov     	dl,36			;coluna 0-79
		call	cursor
    	mov     al,[mens51]
		call	caracter
		pop		cx

		cmp     byte[flagc2],1
		je 		fim_gambaux
		jmp		continua3
		
apaga222:
;apaga o botao 2
	;Botoes externos (subir):  1  2  3
		mov		byte[cor],branco_intenso
		push		cx
    	mov     	cx,1			;numero de caracteres
    	;mov     	bx,0
    	mov     	dh,6			;linha 0-29
    	mov     	dl,39			;coluna 0-79
		call	cursor
    	mov     al,[mens52]
		call	caracter
		pop		cx


		cmp     byte[flagc2],1
		je 		fim_gambaux		
		jmp		continua3
apagaleddescaux:	jmp		apagaleddesc	
apaga1111aux:
			jmp apaga1111
apaga2222aux:
			jmp apaga2222
apaga3333aux:
			jmp apaga3333
apaga4444aux:
			jmp apaga4444	
apaga333:
	;apaga o botao externo 3
		;Botoes externos (subir):  1  2  3
		mov		byte[cor],branco_intenso
		push		cx
    	mov     	cx,1			;numero de caracteres
    	;mov     	bx,0
    	mov     	dh,6			;linha 0-29
    	mov     	dl,42			;coluna 0-79
		call	cursor
    	mov     al,[mens53]
		call	caracter
		pop		cx

		cmp     byte[flagc2],1
		je 		fim_gambaux		
		jmp		continua3	

			
apaga444:	
	;apaga o botao externo 4
		;Botoes externos (descer):     2  3  4
		mov		byte[cor],branco_intenso
		push		cx
    	mov     	cx,1			;numero de caracteres
    	;mov     	bx,0
    	mov     	dh,8			;linha 0-29
    	mov     	dl,46			;coluna 0-79
		call	cursor
    	mov     al,[mens54]
		call	caracter
		pop		cx

		cmp     byte[flagc2],1
		je 		fim_gambaux
		jmp		continua3
apaga111aux:
		jmp 	apaga111
apaga222aux:
		jmp 	apaga222		
apaga333aux:
		jmp 	apaga333		
fim_gambaux:
		jmp 	fim_gamb

continua3:			
	mov		bx, dx
	add		bx, externosob1
	;call	des_tecla
	mov		bx, cx
	cmp		bx, 3 ; Ve se o elevador está no quarto andar
	je		apagaledsub1
	mov		cl, bl
	mov		al, 1
	shl		al, cl
	not		al
	and		byte[saida1], al
apagaledsub1:
	inc		bx
	call	verifica_acima
	dec		bx
	test	al, 1
	jnz		fazerpararaux
	inc		bx
	call	verifica_andar ; limpa tudo que tem nesse andar
	jmp		fazerparar

apagaleddesc:
; Esta descendo
;testando gambiarra	
gamb:
	inc 	bx
	call	verifica_abaixo
	dec 	bx

	cmp 	al,0    ;comparacao eh verdadeira se n tiver nada pra baixo
	jne		fim_gamb

	mov 	byte[flagc2],1

 	cmp		bx,0
	je		apaga111aux
	cmp		bx,1
	je		apaga222aux
	cmp		bx,2
	je		apaga333aux
	cmp		bx,3
	je		apaga444
	
	
fim_gamb:


	mov 	byte[flagc2],0

	mov		byte[chamadasdesc+bx], 0 ; Limpa a chamada de descida deste andar
	cmp		bx,0
	je		apaga1111
	cmp		bx,1
	je		apaga2222
	cmp		bx,2
	je		apaga3333
	cmp		bx,3
	je		apaga4444
	jmp		continua4
	
fim_gamb2aux:
jmp		fim_gamb2

apaga1111:
;apaga o botao 1	
	;Botoes externos (subir):  1  2  3
		mov		byte[cor],branco_intenso
		push		cx
    	mov     	cx,1			;numero de caracteres
    	;mov     	bx,0
    	mov     	dh,6			;linha 0-29
    	mov     	dl,36			;coluna 0-79
		call	cursor
    	mov     al,[mens51]
		call	caracter
		pop		cx
		cmp		byte[flagc3],1
		je		fim_gamb2aux
		jmp		continua4
fazerpararaux:	jmp		fazerparar			
apaga2222:
;apaga o botao 2
	;Botoes externos (descer):     2  3  4
		mov		byte[cor],branco_intenso
		push		cx
    	mov     	cx,1			;numero de caracteres
    	;mov     	bx,0
    	mov     	dh,8			;linha 0-29
    	mov     	dl,40			;coluna 0-79
		call	cursor
    	mov     al,[mens52]
		call	caracter
		pop		cx
		cmp		byte[flagc3],1
		je		fim_gamb2aux		
		jmp		continua4

fim_gamb3aux:
		jmp fim_gamb2aux
;fazerpararaux:	jmp		fazerparar		
apaga3333:
	;apaga o botao externo 3
		;Botoes externos (descer):     2  3  4
		mov		byte[cor],branco_intenso
		push		cx
    	mov     	cx,1			;numero de caracteres
    	;mov     	bx,0
    	mov     	dh,8			;linha 0-29
    	mov     	dl,43			;coluna 0-79
		call	cursor
    	mov     al,[mens53]
		call	caracter
		pop		cx
		cmp		byte[flagc3],1
		je		fim_gamb2aux		
		jmp		continua4	
		
apaga4444:	
;apaga o botao externo 4
		;Botoes externos (descer):     2  3  4
		mov		byte[cor],branco_intenso
		push		cx
    	mov     	cx,1			;numero de caracteres
    	;mov     	bx,0
    	mov     	dh,8			;linha 0-29
    	mov     	dl,46			;coluna 0-79
		call	cursor
    	mov     al,[mens54]
		call	caracter
		pop		cx
		cmp		byte[flagc3],1
		je		fim_gamb3aux	
		jmp		continua4
		

continua4:	
	mov		bx, dx
	add		bx, externodesc1
;	call	des_tecla
	mov		bx, cx
	cmp		bx, 0
	je		apagaleddesc1
	mov		cl, bl
	add		cl, 2
	mov		al, 1
	shl		al, cl
	not		al
	and		byte[saida1], al
apagaleddesc1:
	inc		bx
	call	verifica_abaixo
	dec		bx
	test	al, 1
	jnz		fazerparar
	inc		bx
	call	verifica_andar
	
fazerparar:
;Fazer o elevador ficar parado por x segundos
	and		byte[saida2], 11111110b
	and		byte[saida1], 00111111b
	mov		dx, psaida1
	mov		al, byte[saida1]
	out		dx, al
	mov		dx, psaida2
	mov		al, byte[saida2]
	out		dx, al
	
	;Status do elevador: Porta aberta
		mov			byte[cor],branco_intenso
    	mov     	cx,12			;numero de caracteres
    	mov     	bx,0
    	mov     	dh,12			;linha 0-29
    	mov     	dl,30			;coluna 0-79
l161:
		call	cursor
    	mov     al,[bx+mens16]
		call	caracter
    	inc     bx			;proximo caracter
		inc		dl			;avanca a coluna
    	loop    l161	
		
		
		mov		cx,500
loopgordo3:

	push	cx
	mov		cx,50000
loopgordo4:
	loop	loopgordo4 
	pop		cx
	
	loop	loopgordo3
	
	mov		bx,	aberta
	mov		dh, 2
	mov		dl, 17
	mov		byte[cor], branco_intenso
	mov		byte[parado], 1
	
	;Status do elevador: Parado
    	mov     	cx,12			;numero de caracteres
    	mov     	bx,0
    	mov     	dh,12			;linha 0-29
    	mov     	dl,30			;coluna 0-79
l17:
		call	cursor
    	mov     al,[bx+mens17]
		call	caracter
    	inc     bx			;proximo caracter
		inc		dl			;avanca a coluna
    	loop    l17	
		
	
movendo:

; Verifica se está em emergencia
	test	byte[emergencia], 1
	jnz		fimemergenciar2

;Se nao estiver movendo verificar se existe outro andar
	test	byte[parado], 1
	jnz		continua
	mov		al, byte[saida1]
	test	al, 11000000b
	jnz		continua
	xor		bh, bh
	mov		bl, byte[andar]
	cmp		byte[situacao], 1 ; Checa se esta subindo
	jne		movendo1
	call	efetua_acima
	test	al, 1
	jz		verifica1
	jmp		continua
verifica1:
	call	efetua_abaixo
	test	al, 1
	jnz		continua
	jmp		parar
movendo1:
	call	efetua_abaixo
	test	al, 1
	jz		verifica2
	jmp		continua
verifica2:
	call	efetua_acima
	test	al, 1
	jnz		continua
parar:
	call	verifica_andar
	and		byte[saida1], 00111111b
	mov		dx, psaida1
	mov		al, byte[saida1]
	out		dx, al
	mov		byte[situacao], 0
continua:

	jmp		sfimemergenciar2
fimemergenciar2: jmp fimemergencia
sfimemergenciar2:

	test	byte[parado], 1
	jz		dessit
	inc		word[contaparado]
	cmp		word[contaparado], 11111111111111b
	jb		fimparadoaux
	cmp		byte[flagparado], 1
	je		contparado
	inc		word[contaparado2]
	cmp		word[contaparado2], 60000
	jb		fimparado
contparado:
	mov		byte[contaparado2], 0
	mov		word[contaparado], 0
	mov		byte[parado], 0
	or		byte[saida2], 00000001b
	mov		al, byte[saida2]
	mov		dx, psaida2
	out		dx, al
	jmp		fimparado
dessit:
	mov		byte[cor], branco_intenso
	cmp		byte[situacao], 0
	je		escparado
	cmp		byte[situacao],1
	je		escsubindo
	cmp		byte[situacao],2
	je		escdescendo
	jmp		fimparado
fimparadoaux: jmp	fimparado
escparado:
		jmp		fimparado
escsubindo:
;Status do elevador: Subindo
    	mov     	cx,7			;numero de caracteres
    	mov     	bx,0
    	mov     	dh,12			;linha 0-29
    	mov     	dl,30			;coluna 0-79
l10:
		call	cursor
    	mov     al,[bx+mens10]
		call	caracter
    	inc     bx			;proximo caracter
		inc		dl			;avanca a coluna
    	loop    l10	
		jmp		fimparado
		
escdescendo:		
	;Status do elevador: Descendo
    	mov     	cx,8			;numero de caracteres
    	mov     	bx,0
    	mov     	dh,12			;linha 0-29
    	mov     	dl,30			;coluna 0-79
l11:
		call	cursor
    	mov     al,[bx+mens11]
		call	caracter
    	inc     bx			;proximo caracter
		inc		dl			;avanca a coluna
    	loop    l11	
		
fimparado:
fimemergencia:	

jmpmain:
	jmp		main

; sair do programa
sair:
	mov		al, 0
	mov		dx, psaida1		;zera saida 318h motores e leds
	out		dx, al
	inc		al
	mov		dx, psaida2		;zera saida 319h led da cabine
	out		dx, al

	;devolve a interrupcao habitual da int9
	cli
	xor     ax, ax
	mov     es, ax
	mov     ax, [cs_dos]
	mov     [es:int9*4+2], ax
	mov     ax, [offset_dos]
	mov     [es:int9*4], ax

	;restaura modo de video anterior
	mov		ah,0				; set video mode
	mov		al,[modo_anterior]	; modo anterior
	int		10h
	mov		ax,4c00h
	int		21h

; Verifica se há pedidos de andares para o andar atual
;	bx = andar atual;
;	retornos: al = resposta (1 = sim, 0 = nao)

verifica_andar:
	push	ax
	push	bx
	push	cx
	push	dx
	mov		al, 0
	dec		bx
	mov		dx, bx
;	mov		byte[cor], cinza
	mov		byte[chamadasint+bx], 0
	mov		byte[chamadassub+bx], 0
	mov		byte[chamadasdesc+bx], 0
	imul	bx, 3
	mov		cx, dx
	cmp		dx, 3
	je		verifica_andar2
	mov		al, 1
	shl		al, cl
	not		al
	and		byte[saida1], al
verifica_andar2:
	cmp		dx, 0
	je		fim_verifica_andar
	mov		al, 1
	add		cl, 2
	shl		al, cl
	not		al
	and		byte[saida1], al
fim_verifica_andar:
	and		byte[saida1], al
	mov		al, byte[saida1]
	mov		dx, psaida1
	out		dx, al
	pop		dx
	pop		cx
	pop		bx
	pop		ax
	ret
	
; Verifica se há pedidos de andares abaixo do andar atual
;	bx = andar atual;
;	retornos: al = resposta (1 = sim, 0 = nao)

efetua_abaixo:
	push	dx
	call	verifica_abaixo
	test	al, 1
	jz		fim_efetua_abaixo
	or		byte[saida1], 10000000b
	and		byte[saida1], 10111111b
	mov		al, byte[saida1]
	mov		dx, psaida1
	out		dx, al
	mov		byte[situacao], 2
	mov		al, 1
fim_efetua_abaixo:
	pop		dx
	ret

verifica_abaixo:
	push	bx
	push	cx
	push	dx
	push	si
	mov		al, 0
	mov		dx, bx
	mov		si, chamadasint
	mov		cx, 3
verifica_abaixo_loop:
	mov		bx, dx
verifica_abaixo1:
	dec		bx
	cmp		bx, 1
	jb		verifica_abaixo2
	test	byte[si+bx-1], 1
	jz		verifica_abaixo1
	jmp		verifica_abaixo_existe
verifica_abaixo2:
	add		si, numandares
	loop	verifica_abaixo_loop
	jmp		fim_verifica_abaixo
verifica_abaixo_existe:
	mov		al, 1
fim_verifica_abaixo:
	pop		si
	pop		dx
	pop		cx
	pop		bx
	ret

; Verifica se há pedidos de andares acima do andar atual
;	bx = andar atual;
;	retornos: al = resposta (1 = sim, 0 = nao)

efetua_acima:
	push	dx
	call	verifica_acima
	test	al, 1
	jz		fim_efetua_acima
	or		byte[saida1], 01000000b
	and		byte[saida1], 01111111b
	mov		al, byte[saida1]
	mov		dx, psaida1
	out		dx, al
	mov		byte[situacao], 1
	mov		al, 1
fim_efetua_acima:
	pop		dx
	ret

verifica_acima:
	push	bx
	push	cx
	push	dx
	push	si
	mov		al, 0
	mov		dx, bx
	mov		si, chamadasint
	mov		cx, 3
verifica_acima_loop:
	mov		bx, dx
verifica_acima1:
	inc		bx
	cmp		bx, numandares
	ja		verifica_acima2
	test	byte[si+bx-1], 1
	jz		verifica_acima1
	jmp		verifica_acima_existe
verifica_acima2:
	add		si, numandares
	loop	verifica_acima_loop
	jmp		fim_verifica_acima
verifica_acima_existe:
	mov		al, 1
fim_verifica_acima:
	pop		si
	pop		dx
	pop		cx
	pop		bx
	ret



; Caracter
;	al= caracter a ser escrito
caracter:
	pushf
	push	ax
	push	bx
	push	cx
	mov		ah, 9  ; write character and attribute at cursor position
	mov		bh, 0  ; número da página
	mov		cx, 1  ; número de vezes que o caractere deve ser escrito
	mov		bl, [cor]  ; atributo
	int		10h
	pop		cx
	pop		bx
	pop		ax
	popf
	ret


; Cursor
;	dh = linha (0-29) e	 dl=coluna	(0-79)
cursor:
	pushf
	push	ax
	push	bx
	mov		ah, 2 ; cursor
	mov		bh, 0
	int		10h
	pop		bx
	pop		ax
	popf
	ret

; interrupção do teclado
keyint:
	push    ax
	push    bx
	push    ds

;fifo eh um vetor de 8 bytes
;p_m eh um ponteiro para fifo pelo programa principal
;p_i eh um ponteiro para fifo pela interrupcao

	mov     ax, data
	mov     ds, ax
	in      al, kb_data  ;60h
	inc     word[p_i]
	and     word[p_i],7  ;00000111
	mov     bx, [p_i]
	mov     [bx+fifo], al
	in      al, kb_ctl   ;61h
	or      al, 80h      ;10000000
	out     kb_ctl, al   
	and     al, 7fh      ;01111111
	out     kb_ctl, al

	mov     al, eoi      ;20h end of interrupt, encerra a interrupcao
	out     pictrl, al   ;20h
	pop     ds
	pop     bx
	pop     ax
	iret

	
;
;   fun��o plot_xy
;
; push x; push y; call plot_xy;  (x<639, y<479)
; cor definida na variavel cor
plot_xy:
		push		bp
		mov		bp,sp
		pushf
		push 		ax
		push 		bx
		push		cx
		push		dx
		push		si
		push		di
	    mov     	ah,0ch
	    mov     	al,[cor]
	    mov     	bh,0
	    mov     	dx,479
		sub		dx,[bp+4]
	    mov     	cx,[bp+6]
	    int     	10h
		pop		di
		pop		si
		pop		dx
		pop		cx
		pop		bx
		pop		ax
		popf
		pop		bp
		ret		4

;*******************************************************************
segment data

	cor				db		branco

	preto			equ		0
	azul			equ		1
	verde			equ		2
	cyan			equ		3
	vermelho		equ		4
	magenta			equ		5
	marrom			equ		6
	branco			equ		7
	cinza			equ		8
	azul_claro		equ		9
	verde_claro		equ		10
	cyan_claro		equ		11
	rosa			equ		12
	magenta_claro	equ		13
	amarelo			equ		14
	branco_intenso	equ		15
	
	mteclaesc		equ		01h
	bteclaesc		equ		81h
	btecla1			equ		82h
	btecla2			equ		83h
	btecla3			equ		84h
	btecla4			equ		85h
	bteclas			equ		9fh	;10011111 break da tecla s
	
	psaida1			equ		318h
	psaida2			equ		319h
	pentrada		equ		319h
	
	numandares		equ		4
	

;Andar
;	valor, linha, coluna
	andar			db		4, 1, 14
	
	entrada			db		00000000b
	saida1			db		01000000b
	saida2			db		00000001b
	anterior		db		00000000b
	situacao		db		0 ; 0 = parado, 1 = subindo, 2 = descendo
	emergencia		db		0 ; 0 = desligada, 1 = ligada
	emergenciaflag	db		0
	chamadasint		db		0, 0, 0, 0 ;chamadas referentes as teclas 1 2 3 4 (break)
	chamadassub		db		0, 0, 0, 0
	chamadasdesc	db		0, 0, 0, 0
	contador		db		0
	podecontar		db		1
	parado			db		0
	flagparado		db		0
	contaparado		dw		0
	contaparado2	dw		0
	contadebounce	db		0
	ultimaentrada	db		0

	flagc2 			db 		0
	flagc3 			db 		0
	
	contaespera		db		0
	contasensor		db		0
	
	modo_anterior	db		0
	linha			dw		0
	coluna			dw		0
	deltax			dw		0
	deltay			dw		0
	
	kb_data			equ		60h	 ;PORTA DE LEITURA DE TECLADO
	kb_ctl			equ		61h	 ;PORTA DE RESET PARA PEDIR NOVA INTERRUPCAO
	pictrl			equ		20h
	eoi				equ		20h
	int9			equ		9h
	cs_dos			dw		1
	offset_dos		dw		1
	fifo			resb	8   ;reserva 8 bytes
    p_i				dw		0   ;ponteiro para fifo (pela interrupcao)  
    p_m				dw		0   ;ponteiro para fifo (pelo programa principal)

	mens0		db			'Posicionando elevador, aguarde!'
	mens00		db			'                               '
	mens1    	db  		'Vitor Ribeiro Roriz'
	mens2    	db  		'Tarcisio Prest Bernabe'
	mens3    	db  		'Sistemas Embarcados 1 - 2014/1'
	mens4    	db  		'Mayara Nascimento de Oliveira'
	mens5		db			'Botoes internos:   1   2   3   4'
	mens51		db			'1'
	mens52		db			'2'
	mens53		db			'3'
	mens54		db			'4'
	mens6		db			'Botoes externos (subir):  1  2  3   '
	mens7		db			'Andar atual:'
	mens8		db			'Status do elevador:'
	mens9		db			'EMERGENCIA!'
	mens10		db			'Subindo'
	mens11		db			'Descendo'
	mens16		db			'Porta aberta'
	mens17		db			'Parado      '
	mens18		db			'Botoes externos (descer):     2  3  4'
	paradostr		db		"Parado      $"
	subindo			db		"Subindo     $"
	descendo		db		"Descendo    $"
	aberta			db		"Porta aberta$"
	emergenciastr	db		"Emergencia!!$"
;*************************************************************************
segment stack stack
	resb		512
stacktop: