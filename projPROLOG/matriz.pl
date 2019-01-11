% A matriz na verdade vai ser uma lista 1D da forma
% largura * linha + coluna
%
%	#*#
%	##*
%	*##


%LEMBRANDO QUE O PRIMEIRO ELEMENTO DA MATRIZ
%TA NO INDICE LINHA 0 COLUNA 0


%A passagem de valor é do tipo In and Out, portanto, modificando o valor de R
%dentro da função, também será mudado fora
lugar1D(I, R) :- 
	%Criando uma variavel local com a "matriz" do campo
	Campo = [0, 1, 0, 0, 0, 1, 1, 0, 0],
	%Isso é um if-else: (exp -> exp == true ; exp == false).
	(I > -1, I < 9 ->
		nth0(I, Campo, R)
	; 	R is 0
	).


lugar(Linha, Coluna, R) :-
	%Só o is permite setar variaveis com alguma artimetica envolvida
	Pos is 3 * Linha + Coluna,
	%Repassando R
	lugar1D(Pos, R).


eh_mina1D(Pos) :-
	%Cria a variavel R dinamicamente que vai ser alterada na chamada da função
	lugar1D(Pos, R),
	%Verifica se R é igual a 1, se sim, retorna true
	R == 1.

eh_mina(Linha, Coluna) :- 
	Pos is 3 * Linha + Coluna,
	eh_mina1D(Pos).


%X e Y são variaveis de entrada e Contagem de saida que vai ter o numero de minas
%nos 8 campos ao redor do ponto de linha Y e coluna X da matriz
conta_arredor(X, Y, Contagem) :-
	NX is X - 1,
	NY is Y - 1,
	lugar(NY, NX, R1),

	%Ficar setando a mesma variavel com valores diferente varias vezes
	%me causou problemas, ai criei novas
	NX1 is X,
	NY1 is Y - 1,
	lugar(NY1, NX1, R2),

	NX2 is X + 1,
	NY2 is Y - 1,
	lugar(NY2, NX2, R3),

	NX3 is X + 1,
	NY3 is Y,
	lugar(NY3, NX3, R4),

	NX4 is X + 1,
	NY4 is Y + 1,
	lugar(NY4, NX4, R5),

	NX5 is X,
	NY5 is Y + 1,
	lugar(NY5, NX5, R6),

	NX6 is X - 1,
	NY6 is Y + 1,
	lugar(NY6, NX6, R7),

	NX7 is X - 1,
	NY7 is Y,
	lugar(NY7, NX7, R8),

	%Como R[N] é criado dinamicamente, basta somar tudo e por em contagem que
	%veio no parametro
	Contagem is R1 + R2 + R3 + R4 + R5 + R6 + R7 + R8.

%Testes
%conta_arredor(1, 1, C).
%Acima deve ser 3
%eh_mina(1, 2).
%Acima deve ser true

