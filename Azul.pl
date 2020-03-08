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
%factoria(['_','_','_','_']).
factoria([]).

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

%Elegir ficha de factoria o centro de la mesa cuando ficha sea del color que queremos
elegirFicha(Color, MazoFichas, NumFichas, MazoFichasAux, NumFichasOut, MazoFichasOut):- %Se escoge el color de la ficha, el mazo de donde se va a extraer, y se generan listas auxiliares
      MazoFichas = [Ficha|MasFichas], %separamos la primera ficha del resto
      Color = Ficha, !, %si el color que queremos es el color de la ficha que estamos mirando se hará lo siguiente:
      NumFichasAux is NumFichas+1, %se aumenta el numero de fichas que tenemos de ese color
      elegirFicha(Color, MasFichas, NumFichasAux, MazoFichasAux, NumFichasOut, MazoFichasOut). %se vuelve a llamar a la funcion para que siga con la siguiente ficha
%Elegir ficha de factoria o centro de la mesa cuando ficha no sea del color que queremos
elegirFicha(Color, MazoFichas, NumFichas, MazoFichasAux, NumFichasOut, MazoFichasOut):-
      MazoFichas = [Ficha|MasFichas],
      Color \= Ficha, !,%si el color que queremos es el color de la ficha que estamos mirando se hará lo siguiente:
      append(MazoFichasAux, [Ficha], MazoFichasAux2),%se actualiza la lista de valores quitando las fichas cogidas
      elegirFicha(Color, MasFichas, NumFichas, MazoFichasAux2, NumFichasOut, MazoFichasOut).%se vuelve a llamar a la funcion para que siga con la siguiente ficha

  elegirFicha(_, [], NumFichasOut, MazoFichasOut, NumFichasOut, MazoFichasOut).

devolverrandom(Init,Fin,X):-random_between(Init,Fin,X).
rellenarFactorias(FactoriaAux,FactoriaOut,BolsaIn,BolsaOut):-
   length(FactoriaAux,LongitudFactoria),
   (LongitudFactoria<4),
   length(BolsaIn,LongitudBolsa),
   LongitudBolsaF is (LongitudBolsa-1),
   devolverrandom(0,LongitudBolsaF,NumeroAleatorio),
   nth0(NumeroAleatorio,BolsaIn,FichaElegida),
   select(FichaElegida,BolsaIn,BolsaAux),
   FactoriaAux2=[FichaElegida|FactoriaAux],!,
   rellenarFactorias(FactoriaAux2,FactoriaOut,BolsaAux,BolsaOut).

rellenarFactorias(FactOut,FactOut,BolsaOut,BolsaOut).

%ejemplo de uso: rellenarFactorias(X,Y,[1,2,3,4,5,4],Z). siendo X es la factoria vacía, Y la factoria rellena y z la bolsa con los elementos metidos en la factoria eliminados y la lista siendo la bolsa inicial.

%Reglas para generar las factorias
generar_factorias(NumJugadores, ListaOut):-
%Genera factorias para 2 jugadores
   NumJugadores = 2, !,
   generar_factorias_aux(5, [], ListaOut).
generar_factorias(NumJugadores, ListaOut):-
%Genera factorias para 3 jugadores
   NumJugadores = 3, !,
   generar_factorias_aux(7, [], ListaOut).
generar_factorias(NumJugadores, ListaOut):-
%Genera factorias para 4 jugadores
   NumJugadores = 4, !,
   generar_factorias_aux(9, [], ListaOut).
%Introduce factorias en una lista si el número de factorias el mayor que 0
generar_factorias_aux(NumFactorias, ListaFactorias, ListaFactoriasOut):-
   NumFactorias \= 0, !,
   NumFactoriasAux is NumFactorias-1,
   factoria(X),
   append(ListaFactorias, [X], ListaFactoriasAux),
   generar_factorias_aux(NumFactoriasAux, ListaFactoriasAux, ListaFactoriasOut).
%Se devuelve la lista obtenida de factorias
generar_factorias_aux(_, ListaFactoriasOut, ListaFactoriasOut).

%Rellena todas las factorias
rellenar_factorias_generadas(ListaFactorias, ListaFactoriasAux, ListaFactoriasOut, Bolsa, BolsaOut):-
    ListaFactorias = [PrimeraFactoria|RestoFactorias],
    rellenarFactorias(PrimeraFactoria, FactoriaOut, Bolsa, BolsaOutAux),
    append(ListaFactoriasAux,[FactoriaOut],FactoriasCompletas), !,
    rellenar_factorias_generadas(RestoFactorias, FactoriasCompletas, ListaFactoriasOut, BolsaOutAux, BolsaOut).

rellenar_factorias_generadas(_, ListaFactoriasOut, ListaFactoriasOut, BolsaOut, BolsaOut).

%Mostrar fichas de una factoria
mostrar_fichas(Factoria):-
    Factoria = [Primera|Resto],
    write(X), n1,
    mostrar_fichas(Resto).

%Obtener maximo de factorias en partida
obtener_Max_Factorias([], Resultado):-
    Restultado is 0.

obtener_Max_Factorias([X|Y], Resultado):-
   len(Y, R),
   Resultado is R+1.

%Pedir una factoria valida de donde sacar ficha
pedir_Factoria(ListaFactorias, NumFactoria):-
      repeat,
      write('Introduce el numero de la factoria que quieras elegir: '),
      read(NumFactoria),
      obtener_Max_Factorias(ListaFactorias, MaxFactorias),
      ((NumFactoria >= 1, NumFactoria =< MaxFactorias, !);
      writeln('Dato no valido, vuelva a intentarlo'),false).

pedir_Color(ListaColores, ColorSeleccionado):-
      repeat,
      write('Introduce el color deseado: '),
      read(ColorSeleccionado),
      ((member(ColorSeleccionado, ListaColores), !);
      writeln('Color no valido, vuelva a intentarlo'),false).




%generar lineapatrones
lineapatrones([[],[],[],[],[]]).

%generar pared
pared([[],[],[],[],[]]).


%generar SUPERMATRIZ
%generar_supermatriz(NumJugadores, Lista_aux, Supermatriz):-
%    lista_aux = [], !,
%    generar_bolsa([],Bolsa),
%    generar_factorias(NumJugadores, ListaFactorias),
%    rellenar
%    generar_listaJugadores(),
%    lista_datos_comunes is [Bolsa, ListaFactorias, []),
%    append (Lista_aux, Lista_datos_comunes, Lista_aux2),
%    append (Lista_aux2, Lista_Jugadores, Lista_aux3).

%generar_supermatriz(NumJugadores, Lista_aux3, Supermatriz).
