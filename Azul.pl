% Autor:
% Fecha: 20/02/2020



%lista de la bolsa de fichas (20 unidades de cada ficha)   y 100 fichas en total (azul amarillo rojo negro y blanco)
%las fichas se reparten de forma aleatoria a la mesa y meterlas a las factorias de la mesa, cuatro para cada una (la factoria una lista de 4 elementos)
%otra lista del centro de la mesa, a�adiendo las fichas que se quitan de las factorias,
%estado completo del juego guardado en una lista que contenga todo

%tablero de jugador con tres sublistas, lineas de patron(5x5 pero con restricciones), mosaico (5x5) y la linea de suelo (siete posiciones)
%segun el numero de jugadores cambia el numero de tablero para 2->5 3->7 4->9

%DE QUE FACTORIA COGE COLOR Y QUE LINEA DE PATRON QUIERE COLOCAR (menu)
%presentar las factorias

%cada turno un jugador coge un color de una factoria (si hay dos de un mismo color coge ambas ) el resto de baldosas que no sean del color que ha elegido van al centro de la mesa eliminadas
%despues esas baldosas las tiene que poner en las lineas de patron
%en el centro quedan las sobrantes, un jugador puede coger un color entero del centro de la mesa
%cuando hay un color en una linea de patron este se debe mantener en toda la linea, si coges baldosas de mas que no caben en esa linea las sobrantes  pasan al suelo(papelera)

%cuando se acaben las fichas en la mesa completamos el mosaico, se vacian las lineas de patron y se mueven al mosaico rellenando solo el colo r de la linea que corresponda de la linea de patron al mosaico (solo se mueven las lineas que esten completas)
%teniendo el mosaico relleno de minusculas marcando como que esta vacio y al rellenar las pasamos a mayusculas, rellenando las lineas por las diagonales
%se reinicia el juego hasta que un jugador hace una fila completa

%NO HAY QUE HACER MEMORIA SOLO HAY QUE HACER UN FOLIO CON LOS NOMBRES Y HASTA DONDE SE HA LLEGADO.

ficha('A'). %Ficha Azul
ficha('B'). %Ficha Blanco
ficha('N'). %Ficha Negro
ficha('R'). %Ficha Rojo
ficha('O'). %Ficha Naranja

%Regla para seleccionar el n�mero de jugadores de la partida
pedir_numero_jugadores(NumJugadores):-
      repeat,
      write('Introduce el n�mero de jugadores: '),
      read(NumJugadores),
      ((NumJugadores >= 2, NumJugadores =< 4, !);
      writeln('Dato no v�lido, vuelva a intentarlo'),false).

%Reglas para generar la bolsa de fichas
generar_bolsa(ListaFichas, ListaFichasOut):-
%Considera el caso en el que la ficha no se encuentra en la bolsa
      ficha(X),
      \+(member(X, ListaFichas)), !,
      append(ListaFichas, [X], ListaAux),
      generar_bolsa(ListaAux, ListaFichasOut).

generar_bolsa(ListaFichas, ListaFichasOut):-
%Considera el caso en el que la ficha se encuentra en la bolsa
      ficha(X),
      member(X, ListaFichas),
      max_ficha_en_bolsa(ListaFichas, X, 0, NumFichasColorOut),
      NumFichasColorOut < 20, !,
      append(ListaFichas, [X], ListaAux),
      generar_bolsa(ListaAux, ListaFichasOut).

generar_bolsa(ListaFichasOut, ListaFichasOut).

%Reglas para contar el n�mero de veces que aparece una ficha en una lista
max_ficha_en_bolsa(ListaFichas, Ficha, NumFichasColor, NumFichasColorOut):-
%Considera el caso en el que la ficha es igual
      ListaFichas = [FichaAux|MasFichas],
      Ficha = FichaAux, !,
      NumFichasColorAux is NumFichasColor+1,
      max_ficha_en_bolsa(MasFichas, Ficha, NumFichasColorAux, NumFichasColorOut).
max_ficha_en_bolsa(ListaFichas, Ficha, NumFichasColor, NumFichasColorOut):-
%Considera el caso en el que la ficha no es igual
      ListaFichas = [FichaAux|MasFichas],
      Ficha \= FichaAux, !,
      max_ficha_en_bolsa(MasFichas, Ficha, NumFichasColor, NumFichasColorOut).
max_ficha_en_bolsa([], _, NumFichasColorOut, NumFichasColorOut).