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
      write('Introduce el numero de jugadores: '),
      read(NumJugadores),
      ((NumJugadores >= 2, NumJugadores =< 4, !);
      writeln('Dato no valido, vuelva a intentarlo'),false).

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
rellenar_factorias_generadas(ListaFactorias, ListaFactoriasAux, ListaFactoriasOut, Bolsa, BolsaOut):- %Situación para todas las factorias
    ListaFactorias = [PrimeraFactoria|RestoFactorias],
    length(RestoFactorias, LongitudRestoFactorias),
    LongitudRestoFactorias \= 0,
    rellenarFactorias(PrimeraFactoria, FactoriaOut, Bolsa, BolsaOutAux),
    append(ListaFactoriasAux,[FactoriaOut],FactoriasCompletas), !,
    rellenar_factorias_generadas(RestoFactorias, FactoriasCompletas, ListaFactoriasOut, BolsaOutAux, BolsaOut).

rellenar_factorias_generadas(ListaFactorias, ListaFactoriasAux, ListaFactoriasOut, Bolsa, BolsaOut):- %Situación para cuando llega a la posición del centro
    ListaFactorias = [PrimeraFactoria|RestoFactorias],
    length(RestoFactorias, LongitudRestoFactorias),
    LongitudRestoFactorias = 0,
    %rellenarFactorias(PrimeraFactoria, FactoriaOut, Bolsa, BolsaOutAux),
    append(ListaFactoriasAux,[PrimeraFactoria],FactoriasCompletas), !,
    rellenar_factorias_generadas(RestoFactorias, FactoriasCompletas, ListaFactoriasOut, Bolsa, BolsaOut).

rellenar_factorias_generadas(_, ListaFactoriasOut, ListaFactoriasOut, BolsaOut, BolsaOut).

%Mostrar fichas de una factoria
imprimirLista(Lista):-
    Lista = [Primera|Resto],
    write(Primera), write(' '),
    imprimirLista(Resto).

imprimirLista(_).

imprimirListaConCosas(Lista):-
    Lista = [PrimeraLista|RestoListas],
    write("|"),
    imprimirLista(PrimeraLista),
    write("|"),
    imprimirListaConCosas(RestoListas).

imprimirListaConCosas(_).

%Pedir una factoria valida de donde sacar ficha
pedir_Factoria(ListaFactorias, NumFactoria):-
    repeat,
    write('Introduce el numero de la factoria que quiera elegir: '),
    read(NumFactoria),
    length(ListaFactorias, MaxFactorias),
    ((NumFactoria >= 1, NumFactoria =< MaxFactorias, nth1(NumFactoria, ListaFactorias, Factoria), not(estaVacia(Factoria)), !);
      writeln('La factoria introducida esta vacia o no existe, vuelva a intentarlo'),false).

%Obtiene la lista de fichas de una factoria
coger_Factoria(ListaFactorias, NumFactoria, FactoriaOut):-
      nth1(NumFactoria, ListaFactorias, FactoriaOut).

%Pedir un color válido de la lista de colores
pedir_Color(ListaColores, ColorSeleccionado):-
    repeat,
    write('\nIntroduce el color deseado: '),
    read(ColorSeleccionado),
    ((member(ColorSeleccionado, ListaColores), !);
      writeln('Color no valido, vuelva a intentarlo'),false).

%Distribuye los elementos de una factoria en distintas listas de acuerdo al color seleccionado
coger_Color(ListaColores, ColorSeleccionado, ListaColor, ListaColorOut, CentroMesa, CentroMesaOut):-
    ListaColores = [Primero|Resto],
    member(ColorSeleccionado, Primero),
    append([Primero], ListaColor, ListaColorAux),
    coger_Color(Resto, ColorSeleccionado, ListaColorAux, ListaColorOut, CentroMesa, CentroMesaOut).

coger_Color(ListaColores, ColorSeleccionado, ListaColor, ListaColorOut, CentroMesa, CentroMesaOut):-
    ListaColores = [Primero|Resto],
    member(ColorSeleccionado, Primero),
    not(member(ColorSeleccionado, Primero)),
    append([Primero], CentroMesa, CentroMesaAux),
    coger_Color(Resto, ColorSeleccionado, ListaColor, ListaColorOut, CentroMesaAux, CentroMesaOut).

coger_Color(_, _, ListaColorOut, ListaColorOut, CentroMesaOut, CentroMesaOut).


%Generar lista de colores a partir de las fichas de una factoria
get_lista_colores(Factoria, ListaColores, ListaColoresOut):-
    Factoria = [Primero|Resto],
    \+(member(Primero, ListaColores)),
    append(ListaColores, [Primero], ListaColoresAux),
    get_lista_colores(Resto, ListaColoresAux, ListaColoresOut).

get_lista_colores(Factoria, ListaColores, ListaColoresOut):-
    Factoria = [Primero|Resto],
    (member(Primero, ListaColores)),
    get_lista_colores(Resto, ListaColores, ListaColoresOut).

get_lista_colores(_, ListaColoresOut, ListaColoresOut).

%generar lineapatrones
lineapatrones([[],[],[],[],[]]).

%generar pared
pared([[],[],[],[],[]]).

%generar suelo
suelo([]).

%comprobar si lista es vacia
estaVacia(Lista):-
    length(Lista, LongitudLista),
    LongitudLista = 0.

%Comprueba que una lista está vacía
isEmpty(Lista, ValorAux, _, Valor):-
%ValorAux se utiliza como valor de parada de las llamadas recursivas con un valor distinto de 0 (lista no vacía) y 1 (lista vacía)
    length(Lista, LongitudLista),
    LongitudLista = 0,
    ValorAux \= 1,
    ValorAux \= 0,
    ValorAux2 is 1, %Lista está vacía
    isEmpty(Lista, ValorAux2, ValorAux2, Valor),!.

isEmpty(Lista, ValorAux, _, Valor):-
%ValorAux se utiliza como valor de parada de las llamadas recursivas con un valor distinto de 0 (lista no vacía) y 1 (lista vacía)
    length(Lista, LongitudLista),
    LongitudLista \= 0,
    ValorAux \= 1,
    ValorAux \= 0,
    ValorAux2 is 0,  %Lista no está vacía
    isEmpty(Lista, ValorAux2, ValorAux2, Valor),!.

isEmpty(_, _, Valor, Valor).

%Verifica si todas las listas de una lista están vacías
isEmpty_ListaDeListas(ListaDeListas, ValorAux, _, Valor):- %Situación en la que todas las listas están vacías
    ValorAux = 1, % Lista sigue estando vacía
    length(ListaDeListas, LongitudListaDeListas),
    LongitudListaDeListas = 0, %No quedan elementos
    ValorAux2 = -1,
    ValorAux3 = 1, %La lista de listas está completamente vacía
    isEmpty_ListaDeListas(_, ValorAux2, ValorAux3, Valor),!.

isEmpty_ListaDeListas(ListaDeListas, ValorAux, _, Valor):- %Situación en la que una de las listas está vacía
    ValorAux = 1, % Lista sigue estando vacía
    length(ListaDeListas, LongitudListaDeListas),
    LongitudListaDeListas \= 0, %Quedan elementos
    ListaDeListas = [Primera|Resto],
    isEmpty(Primera, 2, _, ValorAux2),
    ValorAux2 = 1, %La lista está vacía
    isEmpty_ListaDeListas(Resto, ValorAux2,_, Valor),!.

isEmpty_ListaDeListas(ListaDeListas, ValorAux, _, Valor):- %Situación en la que una de las listas no está vacía
    ValorAux = 1, % Lista sigue estando vacía
    length(ListaDeListas, LongitudListaDeListas),
    LongitudListaDeListas \= 0, %Quedan elementos
    ListaDeListas = [Primera|Resto],
    isEmpty(Primera, 2, _, ValorAux2),
    ValorAux2 = 0, %La lista no está vacía
    isEmpty_ListaDeListas(Resto, ValorAux2,ValorAux2, Valor),!.

isEmpty_ListaDeListas(_,_,Valor,Valor).

%generar la supermatriz del juego
generar_supermatriz(NumJugadores, Lista_aux, Supermatriz):-
    not(member(_,Lista_aux)), !, %Para finalizar la recursividad si se han generado todos los elementos de la supermatriz
    generar_bolsa([],Bolsa),
    generar_factorias(NumJugadores, ListaFactorias),
    rellenar_factorias_generadas(ListaFactorias, [], ListaFactoriasAux, Bolsa, BolsaAux),
    append(ListaFactoriasAux, [], ListaFactoriasAux2), %Incluye el centro de la mesa
    Lista_datos_comunes = ([BolsaAux, ListaFactoriasAux2, []]), %Bolsa, Factorias, Centro y Caja
    generar_lista_datos_jugador(NumJugadores, [], Lista_datos_jugador),
    append(Lista_aux, [Lista_datos_comunes], Lista_aux2),
    append(Lista_aux2, [Lista_datos_jugador], Lista_aux3),
    generar_supermatriz(NumJugadores, Lista_aux3, Supermatriz).

generar_supermatriz(_, Supermatriz, Supermatriz).

%genera la lista de los datos de cada jugador
generar_lista_datos_jugador(NumJugadores, Lista_datos_jugadorAux, Lista_datos_jugador):-
    NumJugadores \= 0, !,
    lineapatrones(LineasPatrones),
    pared(Pared),
    suelo(Suelo),
    ListaDatosJugador = ([LineasPatrones, Pared, Suelo]),
    NumJugadoresAux is (NumJugadores-1),
    append(Lista_datos_jugadorAux, [ListaDatosJugador], Lista_datos_jugadorAux2),
    generar_lista_datos_jugador(NumJugadoresAux, Lista_datos_jugadorAux2, Lista_datos_jugador).

generar_lista_datos_jugador(_, Lista_datos_jugador, Lista_datos_jugador).

%Rellena una línea de patrón mientras que tenga espacio
rellenarPatron(Fila, Cantidad, Color, PatronIn, PatronOut, SueloIn, SueloOut, CajaIn, CajaOut):-
   length(PatronIn, Num_azulejos_colocados),
   Espacio_disponible is Fila-Num_azulejos_colocados,
   Espacio_disponible \= 0,
   Cantidad \= 0, !,
   append(PatronIn, [Color], Patron),
   CantidadAux is Cantidad-1,
   rellenarPatron(Fila, CantidadAux, Color, Patron, PatronOut, SueloIn, SueloOut, CajaIn, CajaOut).

rellenarPatron(Fila, Cantidad, Color, PatronIn, PatronOut, SueloIn, SueloOut, CajaIn, CajaOut):-
   length(PatronIn, Num_azulejos_colocados),
   Espacio_disponible is Fila-Num_azulejos_colocados,
   Espacio_disponible = 0,
   Cantidad \= 0, !,
   rellenarSuelo(SueloIn,Suelo,Cantidad,Color,CajaIn,Caja),
   rellenarPatron(Fila, 0, Color, PatronIn, PatronOut, Suelo, SueloOut, Caja, CajaOut).

rellenarPatron(_,_,_,PatronOut,PatronOut,SueloOut,SueloOut,CajaOut,CajaOut).

%Ajusta el tamaño del suelo
max_tamanno_suelo(7).

%Rellena el suelo con los azulejos que no han entrado en la línea de patrón y sino entra, lo coloca en la caja
rellenarSuelo(SueloIn,SueloOut,Cantidad,Color,CajaIn,CajaOut):-
   length(SueloIn, Num_azulejos_colocados),
   max_tamanno_suelo(Tamanno_suelo),
   Espacio_disponible is Tamanno_suelo - Num_azulejos_colocados,
   Espacio_disponible \= 0,
   Cantidad \= 0, !,
   append(SueloIn, [Color], Suelo),
   CantidadAux is Cantidad-1,
   rellenarSuelo(Suelo,SueloOut,CantidadAux,Color,CajaIn,CajaOut).

rellenarSuelo(SueloIn,SueloOut,Cantidad,Color,CajaIn,CajaOut):-
   length(SueloIn, Num_azulejos_colocados),
   max_tamanno_suelo(Tamanno_suelo),
   Espacio_disponible is Tamanno_suelo - Num_azulejos_colocados,
   Espacio_disponible = 0,
   Cantidad \= 0, !,
   append(CajaIn, [Color], Caja),
   CantidadAux is Cantidad-1,
   rellenarSuelo(SueloIn,SueloOut,CantidadAux,Color,Caja,CajaOut).

rellenarSuelo(SueloOut,SueloOut,_,_,CajaOut,CajaOut).

%Obtiene la lista de azulejos a colocar en la línea de patrones y la lista de azulejos que van a ir al centro de la mesa
get_azulejo_factoria(Factoria, Color, ListaAux, ListaFichas, ListaCentroAux, ListaCentro):-
   Factoria = ([Primero | Resto]),
   Primero = Color, !,
   append(ListaAux, [Primero], ListaAux2),
   get_azulejo_factoria(Resto, Color, ListaAux2, ListaFichas, ListaCentroAux, ListaCentro).

get_azulejo_factoria(Factoria, Color, ListaAux, ListaFichas, ListaCentroAux, ListaCentro):-
   Factoria = [Primero | Resto],
   Primero \= Color, !,
   append(ListaCentroAux, [Primero], ListaCentroAux2),
   get_azulejo_factoria(Resto, Color, ListaAux, ListaFichas, ListaCentroAux2, ListaCentro).

get_azulejo_factoria(_, _, ListaFichas, ListaFichas, ListaCentro, ListaCentro).

%Pedir una línea de patrón válida de donde sacar ficha
pedir_linea_patron(ListaLineasPatron, ColorIntroducido, FilaLineaPatron):-
    repeat,
    write('Introduce el numero de la linea de patron que quiera elegir: '),
    read(FilaLineaPatron),
    writeln(ListaLineasPatron),
    length(ListaLineasPatron, LongitudLineasPatron),
    ((FilaLineaPatron >= 1,
      FilaLineaPatron =< LongitudLineasPatron+1,
      nth1(FilaLineaPatron,
      ListaLineasPatron, LineaPatron),
      length(LineaPatron, NumAzulejos),
      NumAzulejos < FilaLineaPatron, %Comparación del número de azulejos en la línea con el número de elementos que admite cada fila (nº de línea = nº de azulejos en la línea)
      verificar_situacion_ficha(LineaPatron, ColorIntroducido), !);
    writeln('La línea introducida se encuentra fuera de rango o no tiene espacio para introducir más azulejos o el color del azulejo no es igual al que ya tiene la línea.'),false).

%Verificar si el color deseado se encuentra en la línea de patrón pasada o que la línea de patrón se encuentra vacía
verificar_situacion_ficha(LineaPatron, ColorDeseado):- %Verificar si el color de azulejo pasado es igual alguno de los de los azulejos en la lista
    member(ColorDeseado, LineaPatron), !.

verificar_situacion_ficha(LineaPatron, _):- %Verifica si la lista está vacía
    length(LineaPatron, NumElementosLineaPatron),
    NumElementosLineaPatron = 0.

%Separa los azulejos de un color seleccionado del resto en el centro de la mesa
get_azulejo_centro(Centro, ColorSeleccionado, ListaFichasAux, ListaFichas, ListaCentroAux, CentroActualizado):-
    Centro = [Primero|Resto],
    Primero = ColorSeleccionado, !,
    append(ListaFichasAux, [Primero], ListaFichasAux2),
    get_azulejo_centro(Resto, ColorSeleccionado, ListaFichasAux2, ListaFichas, ListaCentroAux, CentroActualizado).

get_azulejo_centro(Centro, ColorSeleccionado, ListaFichasAux, ListaFichas, ListaCentroAux, CentroActualizado):-
    Centro = [Primero|Resto],
    Primero \= ColorSeleccionado, !,
    append(ListaCentroAux, [Primero], ListaCentroAux2),
    get_azulejo_centro(Resto, ColorSeleccionado, ListaFichasAux, ListaFichas, ListaCentroAux2, CentroActualizado).

get_azulejo_centro(_,_, ListaFichas, ListaFichas, CentroActualizado, CentroActualizado).

rellenar_pared(Fila_actual, Lineas_patron, Lineas_patronAux, Lineas_PatronOut, Pared, ParedAux, ParedOut,CajaIn, CajaOut):-
    Lineas_patron = [Primero|Resto], %Separación de la cabeza de las líneas de patrón de la cola de la misma
    isEmpty(Primero, 2, _, Valor),
    Valor = 1, %Lista está vacía
    Pared = [PrimeraFila|RestoFilas],%Separación de la cabeza de las filas de la pared de la cola de la misma
    append(ParedAux, [PrimeraFila], ParedAux2), %Se va reconstruyendo la pared con lo que había de líneas anteriores de la pared con la que se acaba de estudiar
    append(Lineas_patronAux, [Primero], Lineas_patronAux2), %Se va reconstruyendo las líneas de patrón con lo que había de líneas anteriores de la misma con la que se acaba de estudiar
    Fila_siguiente is Fila_actual+1, %Se obtiene la fila siguiente a estudiar
    rellenar_pared(Fila_siguiente, Resto, Lineas_patronAux2, Lineas_PatronOut, RestoFilas, ParedAux2, ParedOut, CajaIn, CajaOut),!.

rellenar_pared(Fila_actual, Lineas_patron, Lineas_patronAux, Lineas_PatronOut, Pared, ParedAux, ParedOut,CajaIn, CajaOut):-
    Lineas_patron = [Primero|Resto], %Separación de la cabeza de las líneas de patrón de la cola de la misma
    isEmpty(Primero, 2, _, Valor),
    Valor \= 1, %Lista no está vacía
    %not(estaVacia(Primero)), !, %Verificar que la lista no está vacía
    length(Primero, NumFichasColocadas), %Se obtiene el número de azulejos colocados en una línea de patrón
    NumFichasColocadas = Fila_actual, %!, %El número de elemento en la fila es igual al número de fichas que se pueden colocar en dicha fila
    nth0(0, Primero, Color), %Obtiene el color del primer azulejo (Si el problema está bien formalizado, todos los azulejos tendrían el mismo color)
    Pared = [PrimeraFila|RestoFilas],%Separación de la cabeza de las filas de la pared de la cola de la misma
    not(member(Color, PrimeraFila)), %!, %Verificar si el color del azulejo no se encuentra aún en dicha línea de la pared
    append(PrimeraFila,[Color], PrimeraFilaActualizada), %Se añade el color del azulejo en dicha fila de la pared
    NumAzulejosCaja is Fila_actual-1, %Se obtiene el número de azulejos que no van a pasar de la línea de patrón a la caja descartando el que se ha colocado en la pared
    rellenar_caja(NumAzulejosCaja, Color, CajaIn, Caja), %!, %Se introducen los azulejos sobrantes a la caja
    append(ParedAux, [PrimeraFilaActualizada], ParedAux2), %Se va reconstruyendo la pared con lo que había de líneas anteriores de la pared con la que se acaba de estudiar
    append(Lineas_patronAux, [[]], Lineas_patronAux2), %Se va reconstruyendo las líneas de patrón con lo que había de líneas anteriores de la misma con la que se acaba de estudiar
    Fila_siguiente is Fila_actual+1, %Se obtiene la fila siguiente a estudiar
    rellenar_pared(Fila_siguiente, Resto, Lineas_patronAux2, Lineas_PatronOut, RestoFilas, ParedAux2, ParedOut, Caja, CajaOut),!.

rellenar_pared(Fila_actual, Lineas_patron, Lineas_patronAux, Lineas_PatronOut, Pared, ParedAux, ParedOut,CajaIn, CajaOut):-
    Lineas_patron = [Primero|Resto], %Separación de la cabeza de las líneas de patrón de la cola de la misma
    isEmpty(Primero, 2, _, Valor),
    Valor \= 1, %Lista no está vacía
    %not(estaVacia(Primero)), !, %Verificar que la lista no está vacía
    length(Primero, NumFichasColocadas), %Se obtiene el número de azulejos colocados en una línea de patrón
    NumFichasColocadas < Fila_actual, %!, %El número de elemento en la fila no es igual al número de fichas que se pueden colocar en dicha fila
    Pared = [PrimeraFila|RestoFilas],%Separación de la cabeza de las filas de la pared de la cola de la misma
    append(ParedAux, [PrimeraFila], ParedAux2), %Se va reconstruyendo la pared con lo que había de líneas anteriores de la pared con la que se acaba de estudiar
    append(Lineas_patronAux, [Primero], Lineas_patronAux2), %Se va reconstruyendo las líneas de patrón con lo que había de líneas anteriores de la misma con la que se acaba de estudiar
    Fila_siguiente is Fila_actual+1, %Se obtiene la fila siguiente a estudiar
    rellenar_pared(Fila_siguiente, Resto, Lineas_patronAux2, Lineas_PatronOut, RestoFilas, ParedAux2, ParedOut, CajaIn, CajaOut),!.

rellenar_pared(Fila_actual, Lineas_patron, Lineas_patronAux, Lineas_PatronOut, Pared, ParedAux, ParedOut,CajaIn, CajaOut):-
    Lineas_patron = [Primero|Resto], %Separación de la cabeza de las líneas de patrón de la cola de la misma
    isEmpty(Primero, 2, _, Valor),
    Valor \= 1, %Lista no está vacía
    %not(estaVacia(Primero)), !, %Verificar que la lista no está vacía
    length(Primero, NumFichasColocadas), %Se obtiene el número de azulejos colocados en una línea de patrón
    NumFichasColocadas = Fila_actual, %!, %El número de elemento en la fila es igual al número de fichas que se pueden colocar en dicha fila
    nth0(0, Primero, Color), %Obtiene el color del primer azulejo (Si el problema está bien formalizado, todos los azulejos tendrían el mismo color)
    Pared = [PrimeraFila|RestoFilas],%Separación de la cabeza de las filas de la pared de la cola de la misma
    member(Color, PrimeraFila), %!, %Verificar si el color del azulejo se encuentra aún en dicha línea de la pared
    NumAzulejosCaja is Fila_actual, %Se obtiene el número de azulejos que no van a pasar de la línea de patrón a la caja sin descartar el que se ha colocado en la pared si no estuviera en esa línea de la pared
    rellenar_caja(NumAzulejosCaja, Color, CajaIn, Caja), %!, %Se introducen los azulejos sobrantes a la caja
    append(ParedAux, [PrimeraFila], ParedAux2), %Se va reconstruyendo la pared con lo que había de líneas anteriores de la pared con la que se acaba de estudiar
    append(Lineas_patronAux, [[]], Lineas_patronAux2), %Se va reconstruyendo las líneas de patrón con lo que había de líneas anteriores de la misma con la que se acaba de estudiar
    Fila_siguiente is Fila_actual+1, %Se obtiene la fila siguiente a estudiar
    rellenar_pared(Fila_siguiente, Resto, Lineas_patronAux2, Lineas_PatronOut, RestoFilas, ParedAux2, ParedOut, Caja, CajaOut),!.

rellenar_pared(_,_, Lineas_PatronOut, Lineas_PatronOut, _, ParedOut, ParedOut, CajaOut, CajaOut).

rellenar_caja(Cantidad, Color, CajaIn, CajaOut):-
    Cantidad \= 0,
    append(CajaIn, [Color], Caja),
    CantidadAux is Cantidad-1,
    rellenar_caja(CantidadAux, Color, Caja, CajaOut).

rellenar_caja(_,_,CajaOut,CajaOut).

%Obtiene la lista de factorias y el centro en relación a la selección del jugador
get_azulejo(NumFactoria, MaxNumFactoria, Lista_factorias, Lista_factoriasOut, ListaFichas, ListaFichasOut, _, Color):-
   NumFactoria \= -1,
   NumFactoria \= MaxNumFactoria, %Separación de los casos en los que se trabaja con cualquier factoria y no al centro
   nth1(MaxNumFactoria, Lista_factorias, Centro, Lista_factorias_sin_centro), %Separación centro del resto de factorias
   coger_Factoria(Lista_factorias,NumFactoria, Factoria),
   writeln('Fichas factoria escogida:'),
   imprimirLista(Factoria),
   get_lista_colores(Factoria, [], ListaColores),
   writeln(''),
   writeln('Lista de colores disponible en esta factoria:'),
   imprimirLista(ListaColores),
   pedir_Color(Factoria, ColorSeleccionado),
   get_azulejo_factoria(Factoria, ColorSeleccionado, ListaFichas, ListaFichasAux, [], ListaCentro),
   append(Centro, ListaCentro, ListaCentroAux), %Se actualiza el centro
   nth1(NumFactoria, Lista_factorias_sin_centro, _, Resto_Factorias), %Separa la factoria usada del resto. Tener en cuenta para cambiarlo en coger_Factoria
   nth1(NumFactoria, Lista_factoriasOutAux, [], Resto_Factorias),    %Coloca la factoria usada modificada al resto
   nth1(MaxNumFactoria, Lista_factoriasOutAux2, ListaCentroAux, Lista_factoriasOutAux),    %Coloca el centro junto con las factorias
   get_azulejo(-1, MaxNumFactoria, Lista_factoriasOutAux2, Lista_factoriasOut, ListaFichasAux, ListaFichasOut, ColorSeleccionado, Color).

get_azulejo(NumFactoria, MaxNumFactoria, Lista_factorias, Lista_factoriasOut, ListaFichas, ListaFichasOut, _, Color):-
   NumFactoria \= -1,
   NumFactoria = MaxNumFactoria, %Separación de los casos en los que se trabaja con cualquier factoria y no al centro
   nth1(MaxNumFactoria, Lista_factorias, Centro, Lista_factorias_sin_centro), %Separación centro del resto de factorias
   writeln('Fichas factoria escogida:'),
   imprimirLista(Centro),
   get_lista_colores(Centro, [], ListaColores),
   writeln(''),
   writeln('Lista de colores disponible en el centro:'),
   imprimirLista(ListaColores),
   pedir_Color(Centro, ColorSeleccionado),
   get_azulejo_centro(Centro, ColorSeleccionado, ListaFichas, ListaFichasAux, [], ListaCentroAux),
   nth1(MaxNumFactoria, Lista_factorias_con_centro, ListaCentroAux, Lista_factorias_sin_centro),    %Coloca el centro junto con las factorias
   get_azulejo(-1, MaxNumFactoria, Lista_factorias_con_centro, Lista_factoriasOut, ListaFichasAux, ListaFichasOut, ColorSeleccionado, Color).

get_azulejo(_,_,Lista_factoriasOut,Lista_factoriasOut, ListaFichasOut, ListaFichasOut, Color, Color).

main():-
   pedir_numero_jugadores(NumJugadores),
   generar_supermatriz(NumJugadores, [], Supermatriz),
   writeln('Situacion inicial'),
   writeln(Supermatriz),
   juego(1, 1, NumJugadores, Supermatriz).

juego(NumJugadorInicial, NumJugador, NumJugadores, Supermatriz):-  %Situación en la que la factoria usada no es el centro de la mesa y el número de jugadores no se ha superado por el número de jugador
   nth0(0,Supermatriz,Datos_generales), %Se sacan los datos de la bolsa, las factorias, el centro y la caja
   nth0(1,Datos_generales,Lista_factorias), %Cogemos la lista de factorias junto con el centro
   isEmpty_ListaDeListas(Lista_factorias, 1, _, Valor),
   Valor = 0, %Hay al menos un azulejo entre las distintas factorias y el centro de la mesa
   %NumJugador =< NumJugadores, !,
   write('Turno jugador '),
   writeln(NumJugador),
   nth0(1,Supermatriz,Datos_jugadores), %Se sacan los datos de las líneas de patrón, la pared y el suelo de cada jugador
   nth0(0,Datos_generales,Bolsa), %Cogemos la bolsa
   nth0(2,Datos_generales,Caja), %Cogemos la caja
   length(Lista_factorias,LongitudListaFactorias), %Obtenemos la longitud de lista de factorias
   nth1(NumJugador,Datos_jugadores,Lista_datos_jugador, Lista_datos_otros_jugadores), %Separación datos jugador actual del resto
   nth0(0, Lista_datos_jugador, Lineas_patron),
   nth0(1, Lista_datos_jugador, Pared),
   nth0(2, Lista_datos_jugador, Suelo),
   pedir_Factoria(Lista_factorias, NumFactoria),
   get_azulejo(NumFactoria, LongitudListaFactorias, Lista_factorias, Lista_factoriasOut, [], ListaFichas,_, ColorSeleccionado),
   writeln(''),
   writeln(Lista_factoriasOut),
   pedir_linea_patron(Lineas_patron, ColorSeleccionado, FilaLineaPatron),
   length(ListaFichas, NumFichas),
   nth1(FilaLineaPatron, Lineas_patron, Linea_patron, OtrasLineasPatron), %Se separa la línea de patrón a modificar de las demás
   rellenarPatron(FilaLineaPatron,NumFichas,ColorSeleccionado,Linea_patron,PatronOut,Suelo,SueloOut,Caja,CajaOut),

   %NumSiguienteJugador is NumJugador+1,
   nth1(FilaLineaPatron, Lineas_patronAux, PatronOut, OtrasLineasPatron), %Se une la línea de patrón modificada con las demás
   Datos_jugador_actualizados = [Lineas_patronAux, Pared, SueloOut],
   nth1(NumJugador,Datos_jugadores_actualizados,Datos_jugador_actualizados, Lista_datos_otros_jugadores), %Unión de los datos jugador actual con el resto
   Datos_generales_actualizados = [Bolsa, Lista_factoriasOut, CajaOut],
   Supermatriz_actualizada = [Datos_generales_actualizados, Datos_jugadores_actualizados],
   writeln(Supermatriz_actualizada),
   get_siguiente_jugador(NumJugador, NumJugadores, _, NumSiguienteJugador),
   juego(NumJugadorInicial, NumSiguienteJugador, NumJugadores, Supermatriz_actualizada),!.


juego(NumJugadorInicial, NumJugador, NumJugadores, Supermatriz):-
   nth0(0,Supermatriz,Datos_generales), %Se sacan los datos de la bolsa, las factorias, el centro y la caja
   nth0(1,Datos_generales,Lista_factorias), %Cogemos la lista de factorias junto con el centro
   isEmpty_ListaDeListas(Lista_factorias, 1, _, Valor),
   Valor \= 0, %No hay ningún azulejo entre las distintas factorias y el centro de la mesa
   %NumJugador > NumJugadores, !,
   realizar_llenado_paredes_jugadores(1, NumJugadorInicial, NumJugadorInicial, NumJugadores, Supermatriz, Supermatriz_actualizada),
   nth0(0,Supermatriz_actualizada,Datos_generalesAux), %Se sacan los datos de la bolsa, las factorias, el centro y la caja
   nth0(1,Datos_generalesAux,Lista_factoriasAux), %Cogemos la lista de factorias junto con el centro
   nth0(1,Supermatriz_actualizada,Datos_jugadores), %Se sacan los datos de las líneas de patrón, la pared y el suelo de cada jugador
   nth0(0,Datos_generalesAux,Bolsa), %Cogemos la bolsa
   nth0(2,Datos_generalesAux,Caja), %Cogemos la caja
   length(Lista_factoriasAux,LongitudListaFactorias), %Obtenemos la longitud de lista de factorias
   nth1(LongitudListaFactorias, Lista_factoriasAux, Centro, Lista_factorias_sin_centro), %Separación centro del resto de factorias
   rellenar_factorias_generadas(Lista_factorias_sin_centro, [], ListaFactoriasOut, Bolsa, BolsaOut),
   nth1(LongitudListaFactorias, Lista_factorias_actualizada, Centro, ListaFactoriasOut), %Unión centro con el resto de factorias
   Datos_generales_actualizados = [BolsaOut, Lista_factorias_actualizada, Caja],
   Supermatriz_actualizada2 = [Datos_generales_actualizados, Datos_jugadores],
   writeln(Supermatriz_actualizada2),

   juego(NumJugador, NumJugador, NumJugadores, Supermatriz_actualizada2),!.


%Obtiene el siguiente jugador a jugar
get_siguiente_jugador(NumJugador, NumJugadores, _, NumJugadorOut):-
   NumJugador > 0,
   NumJugador < NumJugadores,
   SiguienteJugador is NumJugador+1,
   NumJugadorAux = 0,
   get_siguiente_jugador(NumJugadorAux, NumJugadores, SiguienteJugador, NumJugadorOut),!.

get_siguiente_jugador(NumJugador, NumJugadores, _, NumJugadorOut):-
   NumJugador > 0,
   NumJugador = NumJugadores,
   SiguienteJugador = 1,
   NumJugadorAux = 0,
   get_siguiente_jugador(NumJugadorAux, NumJugadores, SiguienteJugador, NumJugadorOut),!.

get_siguiente_jugador(_,_,NumJugadorOut, NumJugadorOut).

realizar_llenado_paredes_jugadores(Situacion, NumJugadorInicial, NumJugador, NumJugadores, Supermatriz, SupermatrizOut):- %Situación en la que no se trabaja con el primer jugador de la ronda
   NumJugador \= NumJugadorInicial, %No es el primer jugador al que se le rellena su pared
   writeln(NumJugador),
   %NumJugador =< NumJugadores, !,
   writeln(Supermatriz),
   nth0(0,Supermatriz,Datos_generales), %Se sacan los datos de la bolsa, las factorias, el centro y la caja
   nth0(1,Supermatriz,Datos_jugadores), %Se sacan los datos de las líneas de patrón, la pared y el suelo de cada jugador
   nth0(0,Datos_generales,Bolsa), %Cogemos la bolsa
   nth0(1,Datos_generales,Lista_factorias), %Cogemos la lista de factorias junto con el centro
   nth0(2,Datos_generales,Caja), %Cogemos la caja
   nth1(NumJugador,Datos_jugadores,Lista_datos_jugador, Lista_datos_otros_jugadores), %Separación datos jugador actual del resto
   nth0(0, Lista_datos_jugador, Lineas_patron),
   nth0(1, Lista_datos_jugador, Pared),
   nth0(2, Lista_datos_jugador, Suelo),
   rellenar_pared(1, Lineas_patron, [], Lineas_PatronOut, Pared, [], ParedOut,Caja, CajaOut),
   writeln('Lineas de patron:'),
   writeln(Lineas_PatronOut),
   writeln('Pared:'),
   writeln(ParedOut),
   writeln('Caja:'),
   writeln(CajaOut),
   %NumSiguienteJugador is NumJugador+1,
   Datos_jugador_actualizados = [Lineas_PatronOut, ParedOut, Suelo],
   Datos_generales_actualizados = [Bolsa, Lista_factorias, CajaOut],
   nth1(NumJugador,Datos_jugadores_actualizados,Datos_jugador_actualizados, Lista_datos_otros_jugadores), %Unión de los datos jugador actual con el resto
   Supermatriz_actualizada = [Datos_generales_actualizados, Datos_jugadores_actualizados],
   get_siguiente_jugador(NumJugador, NumJugadores, _, NumSiguienteJugador),
   realizar_llenado_paredes_jugadores(Situacion, NumJugadorInicial, NumSiguienteJugador, NumJugadores, Supermatriz_actualizada, SupermatrizOut),!.

realizar_llenado_paredes_jugadores(Situacion, NumJugadorInicial, NumJugador, NumJugadores, Supermatriz, SupermatrizOut):- %Situación en la que se trabaja con el primer jugador de la ronda por primera vez
   NumJugador = NumJugadorInicial, %No es el primer jugador al que se le rellena su pared
   Situacion = 1, %Se pasa por primera vez por el primer jugador
   writeln(NumJugador),
   %NumJugador =< NumJugadores, !,
   writeln(Supermatriz),
   nth0(0,Supermatriz,Datos_generales), %Se sacan los datos de la bolsa, las factorias, el centro y la caja
   nth0(1,Supermatriz,Datos_jugadores), %Se sacan los datos de las líneas de patrón, la pared y el suelo de cada jugador
   nth0(0,Datos_generales,Bolsa), %Cogemos la bolsa
   nth0(1,Datos_generales,Lista_factorias), %Cogemos la lista de factorias junto con el centro
   nth0(2,Datos_generales,Caja), %Cogemos la caja
   nth1(NumJugador,Datos_jugadores,Lista_datos_jugador, Lista_datos_otros_jugadores), %Separación datos jugador actual del resto
   nth0(0, Lista_datos_jugador, Lineas_patron),
   nth0(1, Lista_datos_jugador, Pared),
   nth0(2, Lista_datos_jugador, Suelo),
   rellenar_pared(1, Lineas_patron, [], Lineas_PatronOut, Pared, [], ParedOut,Caja, CajaOut),
   writeln('Lineas de patron:'),
   writeln(Lineas_PatronOut),
   writeln('Pared:'),
   writeln(ParedOut),
   writeln('Caja:'),
   writeln(CajaOut),
   %NumSiguienteJugador is NumJugador+1,
   Datos_jugador_actualizados = [Lineas_PatronOut, ParedOut, Suelo],
   Datos_generales_actualizados = [Bolsa, Lista_factorias, CajaOut],
   nth1(NumJugador,Datos_jugadores_actualizados,Datos_jugador_actualizados, Lista_datos_otros_jugadores), %Unión de los datos jugador actual con el resto
   Supermatriz_actualizada = [Datos_generales_actualizados, Datos_jugadores_actualizados],
   get_siguiente_jugador(NumJugador, NumJugadores, _, NumSiguienteJugador),
   realizar_llenado_paredes_jugadores(0, NumJugadorInicial, NumSiguienteJugador, NumJugadores, Supermatriz_actualizada, SupermatrizOut),!.

realizar_llenado_paredes_jugadores(_,_,_,_, SupermatrizOut, SupermatrizOut).

%Comprobar que la longitud de una línea de la pared tiene 5 azulejos
comprobarLongLista(LineaPared, ValorAux, _, Valor):- %Situación en la que tiene 5 azulejos
    length(LineaPared, LongitudLista),
    LongitudLista = 5,
    ValorAux \= 0,
    ValorAux \= 1,
    ValorAux2 is 1, %La lista tiene 5 elementos
    comprobarLongLista(LineaPared, ValorAux2, ValorAux2, Valor),!.

comprobarLongLista(LineaPared, ValorAux, _, Valor):- %Situación en la que no tiene 5 azulejos
    length(LineaPared, LongitudLista),
    LongitudLista \= 5,
    ValorAux \= 0,
    ValorAux \= 1,
    ValorAux2 is 0, %La lista no tiene 5 elementos
    comprobarLongLista(LineaPared, ValorAux2, ValorAux2, Valor),!.

comprobarLongLista(_,_,Valor,Valor).

%Comprueba la finalización o no de una partida
comprobarFinDeJuego(Pared, ValorAux, _, Valor):-%Situación en la que una de las filas tiene 5 azulejos
    ValorAux = 0, %Aún no se han encontrado ninguna fila con 5 azulejos
    length(Pared, LongitudPared),
    LongitudPared \=0, %Quedan aún más filas
    Pared = [PrimeraLinea|RestoLineas],
    comprobarLongLista(PrimeraLinea, 2, _, ValorAux2),
    ValorAux2 = 1, %La fila se encuentra completa
    comprobarFinDeJuego(RestoLineas, ValorAux2, ValorAux2, Valor),!.

comprobarFinDeJuego(Pared, ValorAux, _,Valor):- %Situación en la que no quedan más filas a comprobar de la pared
    ValorAux = 0, %Aún no se han encontrado ninguna fila con 5 azulejos
    length(Pared, LongitudPared),
    LongitudPared = 0, %No quedan más filas
    ValorAux2 = -1,
    ValorAux3 = 0, %No hay ninguna fila completa
    comprobarFinDeJuego(_, ValorAux2, ValorAux3, Valor),!.

comprobarFinDeJuego(Pared, ValorAux, _, Valor):- %Situación en la que la fila estudiada de la pared no tiene 5 azulejos
    ValorAux = 0, %Aún no se han encontrado ninguna fila con 5 azulejos
    length(Pared, LongitudPared),
    LongitudPared \=0,%Quedan aún más filas
    Pared = [PrimeraLinea|RestoLineas],
    comprobarLongLista(PrimeraLinea, 2, _, ValorAux2),
    ValorAux2 = 0,
    comprobarFinDeJuego(RestoLineas, ValorAux2, _, Valor),!.

comprobarFinDeJuego(_,_,Valor,Valor).

%Esta funcion comprueba si la caja, la bolsa, el centro y las factorias estan vacias y actua segun sea conveniente.
%Siendo CajaIn la caja actual y CajaOut la caja actualizada al meter fichas a las factorias si es que lo hace
%Siendo BolsaIn la bolsa actual  y BolsaOut la bolsa actualizada al haber metido las fichas a las factorias
%FactoriasIn sera una lista de listas vacia en la que meteremos las fichas de la bolsa o la caja en FactoriasOut, que sera las factorias actualizadas
%CentroIn es la lista de las fichas del centro
%Valor es el valor que dictara en que situacion nos encontramos
%Si valor==0 --> La bolsa, la caja y el centro estan vacios. No se puede continuar,se acaba el juego.
%Si valor==1 --> la caja tiene fichas y la bolsa esta vacia, el centro nos da igual en este caso. Se rellena con la caja directamente.
%Si valor==2 --> La bolsa tiene fichas y la caja no, el centro tambien da igual. Se rellena con la bolsa.
%Si valor==3 --> La bolsa y la caja tienen fichas, el centro da igual. Se rellena con la bolsa.
%Si valor==4 --> La bolsa y la caja estan vacias pero el centro tiene fichas. No hace falta hacer nada.

%Ejemplo para que se meta aqui:
%comprobarRellenarFactorias([],X,[],Y,[[],[],[],[],[],[]],Z,[],Valor).
%Siendo X la caja devuelta(que sera una lista vacia en este caso), Y la bolsa devuelta (que sera una lista vacia en este caso), Z las factorias rellenas, que en este caso siguen vacias y valor que sera 0
%La bolsa, la caja, las factorias y el centro estan vacios, fin del juego.
comprobarRellenarFactorias(CajaIn,CajaOut,BolsaIn,BolsaOut,FactoriasIn,FactoriasOut,CentroIn,Valor):-
   isEmpty_ListaDeListas(FactoriasIn,_,_,ValorAux1),
   ValorAux1 is 1,
   estaVacia(BolsaIn),
   BolsaIn = BolsaOut,
   estaVacia(CentroIn),
   estaVacia(CajaIn),
   CajaIn = CajaOut,
   FactoriasIn = FactoriasOut,
   Valor is 0,!
   .
%Ejemplo para que se meta aqui:
%comprobarRellenarFactorias([1],X,[],Y,[[],[],[],[],[],[]],Z,[],Valor).
%Siendo X la caja devuelta que estara vacia, Y que seguira vacia, Z que se habra rellenado con un 1 en la primera casilla y valor que sera 1
%caso en el que la bolsa y las factorias estan vacias, en este caso el centro no importa y se rellena con la caja
comprobarRellenarFactorias(CajaIn,CajaOut,BolsaIn,BolsaOut,FactoriasIn,FactoriasOut,CentroIn,Valor):-
    isEmpty_ListaDeListas(FabricasIn,_,_,ValorAux1),
    ValorAux1 is 1,
    estaVacia(BolsaIn),
    BolsaIn = BolsaOut,
    isEmpty(CajaIn,_,_,ValorAux6),
    ValorAux6 is 0,
    rellenar_factorias_generadas(FactoriasIn,[],FactoriasOut,CajaIn,CajaOut),
    Valor is 1,!
    .
%Ejemplo para que se meta aqui:
%comprobarRellenarFactorias([],X,[1],Y,[[],[],[],[],[],[]],Z,[],Valor).
%Siendo X la caja devuelta que seguira estando vacia, Y que sera la bolsa devuelta que se vaciara, Z que se habra rellenado con un 1 en la primera casilla y valor que sera 2
%caso en el que la caja y las factorias estan vacias, en este caso el centro no importa. La bolsa tiene alguna ficha por lo que se rellena con la bolsa
comprobarRellenarFactorias(CajaIn,CajaOut,BolsaIn,BolsaOut,FactoriasIn,FactoriasOut,CentroIn,Valor):-
    isEmpty_ListaDeListas(FabricasIn,_,_,ValorAux1),
    ValorAux1 is 1,
    estaVacia(CajaIn),
    CajaIn = CajaOut,
    isEmpty(BolsaIn,_,_,ValorAux2),
    ValorAux2 is 0,
    rellenar_factorias_generadas(FactoriasIn,[],FactoriasOut,BolsaIn,BolsaOut),
    Valor is 2,!
    .
%Ejemplo para que se meta aqui:
%comprobarRellenarFactorias([1],X,[2],Y,[[],[],[],[],[],[]],Z,[],Valor).
%Siendo X la caja devuelta que tendra un 1, Y que sera la bolsa devuelta que estara vacia,Z que tendra un 2 en la primera casilla y valor que sera 3.
%Si la caja y la bolsa tienen algo, se rellena con la bolsa
comprobarRellenarFactorias(CajaIn,CajaOut,BolsaIn,BolsaOut,FactoriasIn,FactoriasOut,_,Valor):-
   isEmpty_ListaDeListas(FabricasIn,_,_,ValorAux1),
   ValorAux1 is 1,
   isEmpty(BolsaIn,_,_,ValorAux2),
   ValorAux2 is 0,
   isEmpty(CajaIn,_,_,ValorAux4),
   ValorAux4 is 0,
   CajaIn = CajaOut,
   rellenar_factorias_generadas(FactoriasIn,[],FactoriasOut,BolsaIn,BolsaOut),
   Valor is 3,!
   .
%Ejemplo para que se meta aqui:
%comprobarRellenarFactorias([],X,[],Y,[[],[],[],[],[],[]],Z,[1],Valor).
%Siendo X la caja devuelta que estara vacia, Y que sera la bolsa devuelta que tambien estara vacia,Z las factorias devueltas que tambien estaran vacias y Valor que sera 4.
%si la bolsa y la caja esta vacias y el centro tiene fichas no se hace nada
comprobarRellenarFactorias(CajaIn,CajaOut,BolsaIn,BolsaOut,FactoriasIn,FactoriasOut,CentroIn,Valor):-
   isEmpty_ListaDeListas(FabricasIn,_,_,ValorAux1),
   ValorAux1 is 1,
   estaVacia(BolsaIn),
   BolsaIn = BolsaOut,
   isEmpty(CentroIn,_,_,ValorAux3),
   ValorAux3 is 0,
   estaVacia(CajaIn),
   CajaIn = CajaOut,
   FactoriasIn = FactoriasOut,
   Valor is 4,!
   .
