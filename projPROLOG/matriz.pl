% A matriz na verdade vai ser uma lista 1D da forma
% largura * linha + coluna
%
%	#*#
%	##*
%	*##

lugar1D(I, R) :- 
	Campo = [0, 1, 0, 0, 0, 1, 1, 0, 0],
	(I > -1, I < 9 ->
		nth0(I, Campo, R)
	; 	R is 0
	).

lugar(Linha, Coluna, R) :-
	Pos is 3 * Linha + Coluna,
	lugar1D(Pos, R).


eh_mina1D(Pos) :-
	lugar1D(Pos, R),
	R == 1.

eh_mina(Linha, Coluna) :- 
	Pos is 3 * Linha + Coluna,
	eh_mina1D(Pos).


conta_arredor(X, Y, Contagem) :-
	NX is X - 1,
	NY is Y - 1,
	lugar(NY, NX, R1),

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

	Contagem is R1 + R2 + R3 + R4 + R5 + R6 + R7 + R8.

