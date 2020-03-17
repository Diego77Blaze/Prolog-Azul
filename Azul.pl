% Autor:
% Fecha: 20/02/2020

ficha('A'). %Ficha Azul
ficha('B'). %Ficha Blanco
ficha('N'). %Ficha Negro
ficha('R'). %Ficha Rojo
ficha('O'). %Ficha Naranja

%Genera una factoria
factoria([]).

%Regla para seleccionar el n�mero de jugadores de la partida
pedir_numero_jugadores(NumJugadores):-
   repeat,
   write('Introduce el numero de jugadores: '),
   read(NumJugadores),
   ((NumJugadores >= 2, NumJugadores =< 4, !);
   writeln('Dato no valido, vuelva a intentarlo'),false).

%Reglas para generar la bolsa de fichas
generar_bolsa(ListaFichas, ListaFichasOut):- %Considera el caso en el que la ficha no se encuentra en la bolsa
   ficha(X),
   \+(member(X, ListaFichas)), !,
   append(ListaFichas, [X], ListaAux),
   generar_bolsa(ListaAux, ListaFichasOut).

generar_bolsa(ListaFichas, ListaFichasOut):- %Considera el caso en el que la ficha se encuentra en la bolsa
   ficha(X),
   member(X, ListaFichas),
   max_ficha_en_bolsa(ListaFichas, X, 0, NumFichasColorOut),
   NumFichasColorOut < 20, !,
   append(ListaFichas, [X], ListaAux),
   generar_bolsa(ListaAux, ListaFichasOut).

generar_bolsa(ListaFichasOut, ListaFichasOut).

%Reglas para contar el n�mero de veces que aparece una ficha en una lista
max_ficha_en_bolsa(ListaFichas, Ficha, NumFichasColor, NumFichasColorOut):- %Considera el caso en el que la ficha es igual
   ListaFichas = [FichaAux|MasFichas],
   Ficha = FichaAux, !,
   NumFichasColorAux is NumFichasColor+1,
   max_ficha_en_bolsa(MasFichas, Ficha, NumFichasColorAux, NumFichasColorOut).
          
max_ficha_en_bolsa(ListaFichas, Ficha, NumFichasColor, NumFichasColorOut):- %Considera el caso en el que la ficha no es igual
   ListaFichas = [FichaAux|MasFichas],
   Ficha \= FichaAux, !,
   max_ficha_en_bolsa(MasFichas, Ficha, NumFichasColor, NumFichasColorOut).
max_ficha_en_bolsa([], _, NumFichasColorOut, NumFichasColorOut).

%Elegir ficha de factoria o centro de la mesa cuando ficha sea del color que queremos
elegir_ficha(Color, MazoFichas, NumFichas, MazoFichasAux, NumFichasOut, MazoFichasOut):- %Se escoge el color de la ficha, el mazo de donde se va a extraer, y se generan listas auxiliares
   MazoFichas = [Ficha|MasFichas], %separamos la primera ficha del resto
   Color = Ficha, !, %si el color que queremos es el color de la ficha que estamos mirando se hará lo siguiente:
   NumFichasAux is NumFichas+1, %se aumenta el numero de fichas que tenemos de ese color
   elegir_ficha(Color, MasFichas, NumFichasAux, MazoFichasAux, NumFichasOut, MazoFichasOut). %se vuelve a llamar a la funcion para que siga con la siguiente ficha

%Elegir ficha de factoria o centro de la mesa cuando ficha no sea del color que queremos
elegir_ficha(Color, MazoFichas, NumFichas, MazoFichasAux, NumFichasOut, MazoFichasOut):-
   MazoFichas = [Ficha|MasFichas],
   Color \= Ficha, !,%si el color que queremos es el color de la ficha que estamos mirando se hará lo siguiente:
   append(MazoFichasAux, [Ficha], MazoFichasAux2),%se actualiza la lista de valores quitando las fichas cogidas
   elegir_ficha(Color, MasFichas, NumFichas, MazoFichasAux2, NumFichasOut, MazoFichasOut).%se vuelve a llamar a la funcion para que siga con la siguiente ficha

elegir_ficha(_, [], NumFichasOut, MazoFichasOut, NumFichasOut, MazoFichasOut).

%Devuelve un número aleatorio entre un número de inicio y otro de fin
devolverrandom(Init,Fin,X):-random_between(Init,Fin,X).

%Hace el rellenado de una factoria a partir de los azulejos en la bolsa
rellenar_factorias(FactoriaAux,FactoriaOut,BolsaIn,BolsaOut):-
   length(FactoriaAux,LongitudFactoria),
   (LongitudFactoria<4),
   length(BolsaIn,LongitudBolsa),
   LongitudBolsaF is (LongitudBolsa-1),
   devolverrandom(0,LongitudBolsaF,NumeroAleatorio),
   nth0(NumeroAleatorio,BolsaIn,FichaElegida),
   select(FichaElegida,BolsaIn,BolsaAux),
   FactoriaAux2=[FichaElegida|FactoriaAux],!,
   rellenar_factorias(FactoriaAux2,FactoriaOut,BolsaAux,BolsaOut).

rellenar_factorias(FactOut,FactOut,BolsaOut,BolsaOut).

%Reglas para generar las factorias
generar_factorias(NumJugadores, ListaOut):- %Genera factorias para 2 jugadores
   NumJugadores = 2, !,
   generar_factorias_aux(5, [], ListaOut).

generar_factorias(NumJugadores, ListaOut):- %Genera factorias para 3 jugadores
   NumJugadores = 3, !,
   generar_factorias_aux(7, [], ListaOut).

generar_factorias(NumJugadores, ListaOut):- %Genera factorias para 4 jugadores
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
    rellenar_factorias(PrimeraFactoria, FactoriaOut, Bolsa, BolsaOutAux),
    append(ListaFactoriasAux,[FactoriaOut],FactoriasCompletas), !,
    rellenar_factorias_generadas(RestoFactorias, FactoriasCompletas, ListaFactoriasOut, BolsaOutAux, BolsaOut).

rellenar_factorias_generadas(ListaFactorias, ListaFactoriasAux, ListaFactoriasOut, Bolsa, BolsaOut):- %Situación para cuando llega a la posición del centro
    ListaFactorias = [PrimeraFactoria|RestoFactorias],
    length(RestoFactorias, LongitudRestoFactorias),
    LongitudRestoFactorias = 0,
    %rellenar_factorias(PrimeraFactoria, FactoriaOut, Bolsa, BolsaOutAux),
    append(ListaFactoriasAux,[PrimeraFactoria],FactoriasCompletas), !,
    rellenar_factorias_generadas(RestoFactorias, FactoriasCompletas, ListaFactoriasOut, Bolsa, BolsaOut).

rellenar_factorias_generadas(_, ListaFactoriasOut, ListaFactoriasOut, BolsaOut, BolsaOut).

%Mostrar fichas de una factoria
imprimir_lista(Lista):-
    Lista = [Primera|Resto],
    write(Primera), write(' '),
    imprimir_lista(Resto).

imprimir_lista(_).

%Pedir una factoria válida de donde sacar ficha
pedir_factoria(ListaFactorias, NumFactoria):-
    repeat,
    write('Introduce el numero de la factoria que quiera elegir: '),
    read(NumFactoria),
    length(ListaFactorias, MaxFactorias),
    ((NumFactoria >= 1, NumFactoria =< MaxFactorias, nth1(NumFactoria, ListaFactorias, Factoria), is_empty(Factoria, Valor), Valor = 0, !);
      writeln('La factoria introducida esta vacia o no existe, vuelva a intentarlo'),false).

%Obtiene la lista de fichas de una factoria
coger_factoria(ListaFactorias, NumFactoria, FactoriaOut):-
      nth1(NumFactoria, ListaFactorias, FactoriaOut).

%Pedir un color válido de la lista de colores
pedir_color(ListaColores, ColorSeleccionado):-
    repeat,
    write('\nIntroduce el color deseado: '),
    read(ColorSeleccionado),
    ((member(ColorSeleccionado, ListaColores), !);
      writeln('Color no valido, vuelva a intentarlo'),false).

%Distribuye los elementos de una factoria en distintas listas de acuerdo al color seleccionado
coger_color(ListaColores, ColorSeleccionado, ListaColor, ListaColorOut, CentroMesa, CentroMesaOut):-
    ListaColores = [Primero|Resto],
    member(ColorSeleccionado, Primero),
    append([Primero], ListaColor, ListaColorAux),
    coger_color(Resto, ColorSeleccionado, ListaColorAux, ListaColorOut, CentroMesa, CentroMesaOut).

coger_color(ListaColores, ColorSeleccionado, ListaColor, ListaColorOut, CentroMesa, CentroMesaOut):-
    ListaColores = [Primero|Resto],
    member(ColorSeleccionado, Primero),
    not(member(ColorSeleccionado, Primero)),
    append([Primero], CentroMesa, CentroMesaAux),
    coger_color(Resto, ColorSeleccionado, ListaColor, ListaColorOut, CentroMesaAux, CentroMesaOut).

coger_color(_, _, ListaColorOut, ListaColorOut, CentroMesaOut, CentroMesaOut).

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

%Generar las líneas de patrones
linea_patrones([[],[],[],[],[]]).

%Generar la pared
pared([[],[],[],[],[]]).

%Generar el suelo
suelo([]).

%Verifica si una lista está vacía o no
is_empty(Lista, Valor):-
    length(Lista, LongitudLista),
    LongitudLista = 0,
    Valor is 1,!. %Lista está vacía

is_empty(Lista, Valor):-
    length(Lista, LongitudLista),
    LongitudLista \= 0,
    Valor is 0,!. %Lista tiene azulejos

%Verifica si todas las listas de una lista están vacías
is_empty_lista_de_listas(ListaDeListas, ValorAux, _, Valor):- %Situación en la que todas las listas están vacías
    ValorAux = 1, % Lista sigue estando vacía
    length(ListaDeListas, LongitudListaDeListas),
    LongitudListaDeListas = 0, %No quedan elementos
    ValorAux2 = -1,
    ValorAux3 = 1, %La lista de listas está completamente vacía
    is_empty_lista_de_listas(_, ValorAux2, ValorAux3, Valor),!.

is_empty_lista_de_listas(ListaDeListas, ValorAux, _, Valor):- %Situación en la que una de las listas está vacía
    ValorAux = 1, % Lista sigue estando vacía
    length(ListaDeListas, LongitudListaDeListas),
    LongitudListaDeListas \= 0, %Quedan elementos
    ListaDeListas = [Primera|Resto],
    is_empty(Primera, ValorAux2),
    ValorAux2 = 1, %La lista está vacía
    is_empty_lista_de_listas(Resto, ValorAux2,_, Valor),!.

is_empty_lista_de_listas(ListaDeListas, ValorAux, _, Valor):- %Situación en la que una de las listas no está vacía
    ValorAux = 1, % Lista sigue estando vacía
    length(ListaDeListas, LongitudListaDeListas),
    LongitudListaDeListas \= 0, %Quedan elementos
    ListaDeListas = [Primera|Resto],
    is_empty(Primera, ValorAux2),
    ValorAux2 = 0, %La lista no está vacía
    is_empty_lista_de_listas(Resto, ValorAux2,ValorAux2, Valor),!.

is_empty_lista_de_listas(_,_,Valor,Valor).

%Generar la supermatriz del juego
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
    linea_patrones(LineasPatrones),
    pared(Pared),
    suelo(Suelo),
    ListaDatosJugador = ([LineasPatrones, Pared, Suelo]),
    NumJugadoresAux is (NumJugadores-1),
    append(Lista_datos_jugadorAux, [ListaDatosJugador], Lista_datos_jugadorAux2),
    generar_lista_datos_jugador(NumJugadoresAux, Lista_datos_jugadorAux2, Lista_datos_jugador).

generar_lista_datos_jugador(_, Lista_datos_jugador, Lista_datos_jugador).

%Rellena una línea de patrón mientras que tenga espacio
rellenarPatron(Fila, Cantidad, Color, PatronIn, PatronOut, SueloIn, SueloOut, CajaIn, CajaOut):-
   length(PatronIn, NumAzulejosColocados),
   EspacioDisponible is Fila-NumAzulejosColocados,
   EspacioDisponible \= 0,
   Cantidad \= 0,
   append(PatronIn, [Color], Patron),
   CantidadAux is Cantidad-1,
   rellenarPatron(Fila, CantidadAux, Color, Patron, PatronOut, SueloIn, SueloOut, CajaIn, CajaOut),!.

rellenarPatron(Fila, Cantidad, Color, PatronIn, PatronOut, SueloIn, SueloOut, CajaIn, CajaOut):-
   length(PatronIn, NumAzulejosColocados),
   EspacioDisponible is Fila-NumAzulejosColocados,
   EspacioDisponible = 0,
   Cantidad \= 0, 
   rellenar_suelo(SueloIn,Suelo,Cantidad,Color,CajaIn,Caja),
   rellenarPatron(Fila, 0, Color, PatronIn, PatronOut, Suelo, SueloOut, Caja, CajaOut),!.

rellenarPatron(_,_,_,PatronOut,PatronOut,SueloOut,SueloOut,CajaOut,CajaOut).

%Ajusta el tamaño del suelo
max_tamanno_suelo(7).

%Rellena el suelo con los azulejos que no han entrado en la línea de patrón y sino entra, lo coloca en la caja
rellenar_suelo(SueloIn,SueloOut,Cantidad,Color,CajaIn,CajaOut):-
   length(SueloIn, NumAzulejosColocados),
   max_tamanno_suelo(TamannoSuelo),
   EspacioDisponible is TamannoSuelo - NumAzulejosColocados,
   EspacioDisponible \= 0,
   Cantidad \= 0, !,
   append(SueloIn, [Color], Suelo),
   CantidadAux is Cantidad-1,
   rellenar_suelo(Suelo,SueloOut,CantidadAux,Color,CajaIn,CajaOut).

rellenar_suelo(SueloIn,SueloOut,Cantidad,Color,CajaIn,CajaOut):-
   length(SueloIn, NumAzulejosColocados),
   max_tamanno_suelo(TamannoSuelo),
   EspacioDisponible is TamannoSuelo - NumAzulejosColocados,
   EspacioDisponible = 0,
   Cantidad \= 0, !,
   append(CajaIn, [Color], Caja),
   CantidadAux is Cantidad-1,
   rellenar_suelo(SueloIn,SueloOut,CantidadAux,Color,Caja,CajaOut).

rellenar_suelo(SueloOut,SueloOut,_,_,CajaOut,CajaOut).

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

%Realiza el rellenado de la pared o mosaico de un jugador
alicatado_pared(FilaActual, LineasPatron, LineasPatronAux, LineasPatronOut, Pared, ParedAux, ParedOut,CajaIn, CajaOut):-
    LineasPatron = [Primero|Resto], %Separación de la cabeza de las líneas de patrón de la cola de la misma
    is_empty(Primero, Valor),
    Valor = 1, %Lista está vacía
    Pared = [PrimeraFila|RestoFilas],%Separación de la cabeza de las filas de la pared de la cola de la misma
    append(ParedAux, [PrimeraFila], ParedAux2), %Se va reconstruyendo la pared con lo que había de líneas anteriores de la pared con la que se acaba de estudiar
    append(LineasPatronAux, [Primero], LineasPatronAux2), %Se va reconstruyendo las líneas de patrón con lo que había de líneas anteriores de la misma con la que se acaba de estudiar
    FilaSiguiente is FilaActual+1, %Se obtiene la fila siguiente a estudiar
    alicatado_pared(FilaSiguiente, Resto, LineasPatronAux2, LineasPatronOut, RestoFilas, ParedAux2, ParedOut, CajaIn, CajaOut),!.

alicatado_pared(FilaActual, LineasPatron, LineasPatronAux, LineasPatronOut, Pared, ParedAux, ParedOut,CajaIn, CajaOut):-
    LineasPatron = [Primero|Resto], %Separación de la cabeza de las líneas de patrón de la cola de la misma
    is_empty(Primero, Valor), %Verificar que la lista no está vacía
    Valor \= 1, %Lista no está vacía
    length(Primero, NumFichasColocadas), %Se obtiene el número de azulejos colocados en una línea de patrón
    NumFichasColocadas = FilaActual, %!, %El número de elemento en la fila es igual al número de fichas que se pueden colocar en dicha fila
    nth0(0, Primero, Color), %Obtiene el color del primer azulejo (Si el problema está bien formalizado, todos los azulejos tendrían el mismo color)
    Pared = [PrimeraFila|RestoFilas],%Separación de la cabeza de las filas de la pared de la cola de la misma
    not(member(Color, PrimeraFila)), %!, %Verificar si el color del azulejo no se encuentra aún en dicha línea de la pared
    append(PrimeraFila,[Color], PrimeraFilaActualizada), %Se añade el color del azulejo en dicha fila de la pared
    NumAzulejosCaja is FilaActual-1, %Se obtiene el número de azulejos que no van a pasar de la línea de patrón a la caja descartando el que se ha colocado en la pared
    rellenar_caja(NumAzulejosCaja, Color, CajaIn, Caja), %!, %Se introducen los azulejos sobrantes a la caja
    append(ParedAux, [PrimeraFilaActualizada], ParedAux2), %Se va reconstruyendo la pared con lo que había de líneas anteriores de la pared con la que se acaba de estudiar
    append(LineasPatronAux, [[]], LineasPatronAux2), %Se va reconstruyendo las líneas de patrón con lo que había de líneas anteriores de la misma con la que se acaba de estudiar
    FilaSiguiente is FilaActual+1, %Se obtiene la fila siguiente a estudiar
    alicatado_pared(FilaSiguiente, Resto, LineasPatronAux2, LineasPatronOut, RestoFilas, ParedAux2, ParedOut, Caja, CajaOut),!.

alicatado_pared(FilaActual, LineasPatron, LineasPatronAux, LineasPatronOut, Pared, ParedAux, ParedOut,CajaIn, CajaOut):-
    LineasPatron = [Primero|Resto], %Separación de la cabeza de las líneas de patrón de la cola de la misma
    is_empty(Primero, Valor),
    Valor \= 1, %Lista no está vacía
    %not(estaVacia(Primero)), !, %Verificar que la lista no está vacía
    length(Primero, NumFichasColocadas), %Se obtiene el número de azulejos colocados en una línea de patrón
    NumFichasColocadas < FilaActual, %!, %El número de elemento en la fila no es igual al número de fichas que se pueden colocar en dicha fila
    Pared = [PrimeraFila|RestoFilas],%Separación de la cabeza de las filas de la pared de la cola de la misma
    append(ParedAux, [PrimeraFila], ParedAux2), %Se va reconstruyendo la pared con lo que había de líneas anteriores de la pared con la que se acaba de estudiar
    append(LineasPatronAux, [Primero], LineasPatronAux2), %Se va reconstruyendo las líneas de patrón con lo que había de líneas anteriores de la misma con la que se acaba de estudiar
    FilaSiguiente is FilaActual+1, %Se obtiene la fila siguiente a estudiar
    alicatado_pared(FilaSiguiente, Resto, LineasPatronAux2, LineasPatronOut, RestoFilas, ParedAux2, ParedOut, CajaIn, CajaOut),!.

alicatado_pared(FilaActual, LineasPatron, LineasPatronAux, LineasPatronOut, Pared, ParedAux, ParedOut,CajaIn, CajaOut):-
    LineasPatron = [Primero|Resto], %Separación de la cabeza de las líneas de patrón de la cola de la misma
    is_empty(Primero, Valor),
    Valor \= 1, %Lista no está vacía
    %not(estaVacia(Primero)), !, %Verificar que la lista no está vacía
    length(Primero, NumFichasColocadas), %Se obtiene el número de azulejos colocados en una línea de patrón
    NumFichasColocadas = FilaActual, %!, %El número de elemento en la fila es igual al número de fichas que se pueden colocar en dicha fila
    nth0(0, Primero, Color), %Obtiene el color del primer azulejo (Si el problema está bien formalizado, todos los azulejos tendrían el mismo color)
    Pared = [PrimeraFila|RestoFilas],%Separación de la cabeza de las filas de la pared de la cola de la misma
    member(Color, PrimeraFila), %!, %Verificar si el color del azulejo se encuentra aún en dicha línea de la pared
    NumAzulejosCaja is FilaActual, %Se obtiene el número de azulejos que no van a pasar de la línea de patrón a la caja sin descartar el que se ha colocado en la pared si no estuviera en esa línea de la pared
    rellenar_caja(NumAzulejosCaja, Color, CajaIn, Caja), %!, %Se introducen los azulejos sobrantes a la caja
    append(ParedAux, [PrimeraFila], ParedAux2), %Se va reconstruyendo la pared con lo que había de líneas anteriores de la pared con la que se acaba de estudiar
    append(LineasPatronAux, [[]], LineasPatronAux2), %Se va reconstruyendo las líneas de patrón con lo que había de líneas anteriores de la misma con la que se acaba de estudiar
    FilaSiguiente is FilaActual+1, %Se obtiene la fila siguiente a estudiar
    alicatado_pared(FilaSiguiente, Resto, LineasPatronAux2, LineasPatronOut, RestoFilas, ParedAux2, ParedOut, Caja, CajaOut),!.

alicatado_pared(_,_, LineasPatronOut, LineasPatronOut, _, ParedOut, ParedOut, CajaOut, CajaOut).

%Realizar el rellenado de la caja
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
   coger_factoria(Lista_factorias,NumFactoria, Factoria),
   writeln('Fichas factoria escogida:'),
   imprimir_lista(Factoria),
   get_lista_colores(Factoria, [], ListaColores),
   writeln(''),
   writeln('Lista de colores disponible en esta factoria:'),
   imprimir_lista(ListaColores),
   pedir_color(Factoria, ColorSeleccionado),
   get_azulejo_factoria(Factoria, ColorSeleccionado, ListaFichas, ListaFichasAux, [], ListaCentro),
   append(Centro, ListaCentro, ListaCentroAux), %Se actualiza el centro
   nth1(NumFactoria, Lista_factorias_sin_centro, _, Resto_Factorias), %Separa la factoria usada del resto.
   nth1(NumFactoria, Lista_factoriasOutAux, [], Resto_Factorias),    %Coloca la factoria usada modificada al resto
   nth1(MaxNumFactoria, Lista_factoriasOutAux2, ListaCentroAux, Lista_factoriasOutAux),    %Coloca el centro junto con las factorias
   get_azulejo(-1, MaxNumFactoria, Lista_factoriasOutAux2, Lista_factoriasOut, ListaFichasAux, ListaFichasOut, ColorSeleccionado, Color).

get_azulejo(NumFactoria, MaxNumFactoria, Lista_factorias, Lista_factoriasOut, ListaFichas, ListaFichasOut, _, Color):-
   NumFactoria \= -1,
   NumFactoria = MaxNumFactoria, %Separación de los casos en los que se trabaja con cualquier factoria y no al centro
   nth1(MaxNumFactoria, Lista_factorias, Centro, Lista_factorias_sin_centro), %Separación centro del resto de factorias
   writeln('Fichas factoria escogida:'),
   imprimir_lista(Centro),
   get_lista_colores(Centro, [], ListaColores),
   writeln(''),
   writeln('Lista de colores disponible en el centro:'),
   imprimir_lista(ListaColores),
   pedir_color(Centro, ColorSeleccionado),
   get_azulejo_centro(Centro, ColorSeleccionado, ListaFichas, ListaFichasAux, [], ListaCentroAux),
   nth1(MaxNumFactoria, Lista_factorias_con_centro, ListaCentroAux, Lista_factorias_sin_centro),    %Coloca el centro junto con las factorias
   get_azulejo(-1, MaxNumFactoria, Lista_factorias_con_centro, Lista_factoriasOut, ListaFichasAux, ListaFichasOut, ColorSeleccionado, Color).

get_azulejo(_,_,Lista_factoriasOut,Lista_factoriasOut, ListaFichasOut, ListaFichasOut, Color, Color).

%Realiza la ejecución del juego
jugar():-
   pedir_numero_jugadores(NumJugadores),
   generar_supermatriz(NumJugadores, [], Supermatriz),
   writeln('Situacion inicial'),
   writeln(Supermatriz),
   juego(1, 1, 1, 1, NumJugadores, Supermatriz, Ganador),
   show_ganador(Ganador),!.

%Realiza las operaciones del funcionamiento del juego   
juego(Ronda, Turno, NumJugadorInicial, NumJugador, NumJugadores, Supermatriz, Ganador):-  %Situación en la que la factoria usada no es el centro de la mesa y el número de jugadores no se ha superado por el número de jugador
   nth0(0,Supermatriz,Datos_generales), %Se sacan los datos de la bolsa, las factorias, el centro y la caja
   nth0(1,Datos_generales,Lista_factorias), %Cogemos la lista de factorias junto con el centro
   is_empty_lista_de_listas(Lista_factorias, 1, _, Valor),
   Valor = 0, %Hay al menos un azulejo entre las distintas factorias y el centro de la mesa
   %NumJugador =< NumJugadores, !,
   write('Ronda '),
   write(Ronda),
   write(', Turno '),
   writeln(Turno),
   write('Turno del jugador '),
   writeln(NumJugador),
   nth0(1,Supermatriz,Datos_jugadores), %Se sacan los datos de las líneas de patrón, la pared y el suelo de cada jugador
   nth0(0,Datos_generales,Bolsa), %Cogemos la bolsa
   nth0(2,Datos_generales,Caja), %Cogemos la caja
   length(Lista_factorias,LongitudListaFactorias), %Obtenemos la longitud de lista de factorias
   nth1(NumJugador,Datos_jugadores,Lista_datos_jugador, Lista_datos_otros_jugadores), %Separación datos jugador actual del resto
   nth0(0, Lista_datos_jugador, LineasPatron),
   nth0(1, Lista_datos_jugador, Pared),
   nth0(2, Lista_datos_jugador, Suelo),
   pedir_factoria(Lista_factorias, NumFactoria),
   get_azulejo(NumFactoria, LongitudListaFactorias, Lista_factorias, Lista_factoriasOut, [], ListaFichas,_, ColorSeleccionado),
   writeln(''),
   writeln(Lista_factoriasOut),
   is_space_available(ColorSeleccionado,LineasPatron, Valor2),
   colocarAzulejos(ColorSeleccionado, ListaFichas, Valor2, LineasPatron, PatronOut, Suelo, SueloOut, Caja, CajaOut),
   Datos_jugador_actualizados = [PatronOut, Pared, SueloOut],
   nth1(NumJugador,Datos_jugadores_actualizados,Datos_jugador_actualizados, Lista_datos_otros_jugadores), %Unión de los datos jugador actual con el resto
   Datos_generales_actualizados = [Bolsa, Lista_factoriasOut, CajaOut],
   Supermatriz_actualizada = [Datos_generales_actualizados, Datos_jugadores_actualizados],
   writeln(Supermatriz_actualizada),
   get_siguiente_jugador(NumJugador, NumJugadores, _, NumSiguienteJugador),
   SiguienteTurno is Turno+1,
   juego(Ronda, SiguienteTurno, NumJugadorInicial, NumSiguienteJugador, NumJugadores, Supermatriz_actualizada, Ganador),!.

juego(Ronda, _, NumJugadorInicial, NumJugador, NumJugadores, Supermatriz, Ganador):-
   nth0(0,Supermatriz,Datos_generales), %Se sacan los datos de la bolsa, las factorias, el centro y la caja
   nth0(1,Datos_generales,Lista_factorias), %Cogemos la lista de factorias junto con el centro
   is_empty_lista_de_listas(Lista_factorias, 1, _, Valor),
   Valor \= 0, %No hay ningún azulejo entre las distintas factorias y el centro de la mesa
   alicatado_paredes_jugadores(1, NumJugadorInicial, NumJugadorInicial, NumJugadores, Supermatriz, Supermatriz_actualizada),
   nth0(0,Supermatriz_actualizada,Datos_generalesAux), %Se sacan los datos de la bolsa, las factorias, el centro y la caja
   nth0(1,Datos_generalesAux,Lista_factoriasAux), %Cogemos la lista de factorias junto con el centro
   nth0(1,Supermatriz_actualizada,Datos_jugadores), %Se sacan los datos de las líneas de patrón, la pared y el suelo de cada jugador
   nth0(0,Datos_generalesAux,Bolsa), %Cogemos la bolsa
   nth0(2,Datos_generalesAux,Caja), %Cogemos la caja
   length(Lista_factoriasAux,LongitudListaFactorias), %Obtenemos la longitud de lista de factorias
   nth1(LongitudListaFactorias, Lista_factoriasAux, Centro, Lista_factorias_sin_centro), %Separación centro del resto de factorias
   search_ganador(1, NumJugadorInicial, NumJugadorInicial, NumJugadores, Datos_jugadores, GanadorAux),
   GanadorAux = -1, %Se sigue jugando si no hay ningún jugador que haya completado una línea de la pared
   check_rellenar_factorias(Caja,Bolsa,ValorAux),
   ValorAux \= 0, %Situaciones en las que aún quedan azulejos
   realizar_rellenado_factorias(Caja,CajaOut,Bolsa,BolsaOut,Lista_factorias_sin_centro,ListaFactoriasOut,ValorAux),
   nth1(LongitudListaFactorias, Lista_factorias_actualizada, Centro, ListaFactoriasOut), %Unión centro con el resto de factorias
   Datos_generales_actualizados = [BolsaOut, Lista_factorias_actualizada, CajaOut],
   Supermatriz_actualizada2 = [Datos_generales_actualizados, Datos_jugadores],
   writeln(Supermatriz_actualizada2),
   SiguienteRonda is Ronda+1,
   juego(SiguienteRonda, 1, NumJugador, NumJugador, NumJugadores, Supermatriz_actualizada2, Ganador),!.

juego(_, _, NumJugadorInicial, _, NumJugadores, Supermatriz, Ganador):-
   nth0(0,Supermatriz,Datos_generales), %Se sacan los datos de la bolsa, las factorias, el centro y la caja
   nth0(1,Datos_generales,Lista_factorias), %Cogemos la lista de factorias junto con el centro
   is_empty_lista_de_listas(Lista_factorias, 1, _, Valor),
   Valor \= 0, %No hay ningún azulejo entre las distintas factorias y el centro de la mesa
   alicatado_paredes_jugadores(1, NumJugadorInicial, NumJugadorInicial, NumJugadores, Supermatriz, Supermatriz_actualizada),
   nth0(1,Supermatriz_actualizada,Datos_jugadores), %Se sacan los datos de las líneas de patrón, la pared y el suelo de cada jugador
   nth0(0,Datos_generalesAux,Bolsa), %Cogemos la bolsa
   nth0(2,Datos_generalesAux,Caja), %Cogemos la caja
   search_ganador(1, NumJugadorInicial, NumJugadorInicial, NumJugadores, Datos_jugadores, GanadorAux),
   GanadorAux = -1, %Se sigue jugando si no hay ningún jugador que haya completado una línea de la pared
   check_rellenar_factorias(Caja,Bolsa,ValorAux),
   ValorAux = 0, %Situaciones en las que el juego ha llegado a su fin
   Ganador = GanadorAux,!.

juego(_, _, NumJugadorInicial, _, NumJugadores, Supermatriz, Ganador):-
   nth0(0,Supermatriz,Datos_generales), %Se sacan los datos de la bolsa, las factorias, el centro y la caja
   nth0(1,Datos_generales,Lista_factorias), %Cogemos la lista de factorias junto con el centro
   is_empty_lista_de_listas(Lista_factorias, 1, _, Valor),
   Valor \= 0, %No hay ningún azulejo entre las distintas factorias y el centro de la mesa
   alicatado_paredes_jugadores(1, NumJugadorInicial, NumJugadorInicial, NumJugadores, Supermatriz, Supermatriz_actualizada),
   nth0(1,Supermatriz_actualizada,Datos_jugadores), %Se sacan los datos de las líneas de patrón, la pared y el suelo de cada jugador
   search_ganador(1, NumJugadorInicial, NumJugadorInicial, NumJugadores, Datos_jugadores, GanadorAux),
   GanadorAux \= -1, %Se sigue jugando si no hay ningún jugador que haya completado una línea de la pared
   Ganador = GanadorAux,!.

%Mostrar el ganador de la partida
show_ganador(Ganador):- %Situación en la que hay un ganador.
   Ganador \= -1,
   write('Ganador jugador '),
   write(Ganador),
   writeln('.').

show_ganador(Ganador):- %Situación en la que no hay ganador.
   Ganador = -1,
   writeln('Empate.').

%Obtiene el resultado de la comprobación para saber si hay espacio disponible en las líneas de patrones
is_space_available(Color,ListaLineaPatrones, Valor):-
   is_espacio_disponible(Color, 1, ListaLineaPatrones, 2, _, ValorAux),
   Valor = ValorAux.
   
%Comprobar si existe espacio para introducir un azulejo en las líneas de patrón de un jugador
is_espacio_disponible(Color, NumFila, ListaLineaPatrones, ValorAux, _, Valor):- %Situación en la que una línea no está completa y es del mismo color que el color pasado por parámetro
   ValorAux = 2,
   length(ListaLineaPatrones, NumLineasRestantes),
   NumLineasRestantes \= 0,
   nth1(1,ListaLineaPatrones,Primero),
   length(Primero, NumFichasEnFila),
   NumFichasEnFila < NumFila,
   NumFichasEnFila \= 0,
   member(Color, Primero),
   ValorAuxiliar = 1,
   is_espacio_disponible(Color, NumFila, ListaLineaPatrones, ValorAuxiliar, 1, Valor),!.
   
is_espacio_disponible(Color, NumFila, ListaLineaPatrones, ValorAux, _, Valor):- %Situación en la que una línea no está completa y no es del mismo color que el color pasado por parámetro
   ValorAux = 2,
   length(ListaLineaPatrones, NumLineasRestantes),
   NumLineasRestantes \= 0,
   ListaLineaPatrones = [Primero|Resto],
   length(Primero, NumFichasEnFila),
   NumFichasEnFila < NumFila,
   NumFichasEnFila \= 0,
   not(member(Color, Primero)),
   SiguienteFila is NumFila+1,
   ValorAuxiliar = ValorAux,
   is_espacio_disponible(Color, SiguienteFila, Resto, ValorAuxiliar, _, Valor),!.

is_espacio_disponible(Color, NumFila, ListaLineaPatrones, ValorAux, _, Valor):- %Situación en la que una línea está vacía
   ValorAux = 2,
   length(ListaLineaPatrones, NumLineasRestantes),
   NumLineasRestantes \= 0,
   nth1(1,ListaLineaPatrones,Primero),
   length(Primero, NumFichasEnFila),
   NumFichasEnFila < NumFila,
   NumFichasEnFila = 0,
   ValorAuxiliar = 1,
   is_espacio_disponible(Color, NumFila, ListaLineaPatrones, ValorAuxiliar, 1, Valor),!.

is_espacio_disponible(Color, NumFila, ListaLineaPatrones, ValorAux, _, Valor):- %Situación en la que una línea se encuentra completa y no es la última
   ValorAux = 2,
   length(ListaLineaPatrones, NumLineasRestantes),
   NumLineasRestantes \= 0,
   ListaLineaPatrones = [Primero|Resto],
   length(Primero, NumFichasEnFila),
   NumFichasEnFila >= NumFila,
   SiguienteFila is NumFila+1,
   ValorAuxiliar = ValorAux,
   is_espacio_disponible(Color, SiguienteFila, Resto, ValorAuxiliar, _, Valor),!.

is_espacio_disponible(Color, NumFila, ListaLineaPatrones, ValorAux, _, Valor):- %Situación en la no quedan más líneas
   ValorAux = 2,
   length(ListaLineaPatrones, NumLineasRestantes),
   NumLineasRestantes = 0,
   ValorAuxiliar = 0,
   is_espacio_disponible(Color, NumFila, ListaLineaPatrones, ValorAuxiliar, 0, Valor),!.

is_espacio_disponible(_,_,_,_,Valor,Valor).

colocarAzulejos(ColorSeleccionado, ListaFichas, Valor, LineasPatron, PatronOut, Suelo, SueloOut, Caja, CajaOut):-
   Valor = 1,
   pedir_linea_patron(LineasPatron, ColorSeleccionado, FilaLineaPatron),
   length(ListaFichas, NumFichas),
   nth1(FilaLineaPatron, LineasPatron, LineaPatron, OtrasLineasPatron), %Se separa la línea de patrón a modificar de las demás
   rellenarPatron(FilaLineaPatron,NumFichas,ColorSeleccionado,LineaPatron,PatronAux,Suelo,SueloAux,Caja,CajaAux),
   nth1(FilaLineaPatron, LineasPatronAux, PatronAux, OtrasLineasPatron), %Se une la línea de patrón modificada con las demás
   colocarAzulejos(ColorSeleccionado, ListaFichas, -1, LineasPatronAux, PatronOut, SueloAux, SueloOut, CajaAux, CajaOut),!.

colocarAzulejos(ColorSeleccionado, ListaFichas, Valor, LineasPatron, PatronOut, Suelo, SueloOut, Caja, CajaOut):-
   Valor = 0,
   length(Suelo, NumFichasSuelo),
   NumFichasSuelo = 7,
   writeln('Dado que no entrar los azulejos escogidos en ninguna línea de patrón, se van a distribuir entre el suelo, lugar prioritario, y en la caja, si no queda espacio en el suelo.'),
   append(Caja, ListaFichas, CajaAux),
   colocarAzulejos(ColorSeleccionado, ListaFichas, -1, LineasPatron, PatronOut, Suelo, SueloOut, CajaAux, CajaOut),!.

colocarAzulejos(ColorSeleccionado, ListaFichas, Valor, LineasPatron, PatronOut, Suelo, SueloOut, Caja, CajaOut):-
   Valor = 0,
   length(Suelo, NumFichasSuelo),
   NumFichasSuelo \= 7,
   ListaFichas = [Primero|Resto],
   append(Suelo, [Primero], SueloAux),
   colocarAzulejos(ColorSeleccionado, Resto, Valor, LineasPatron, PatronOut, SueloAux, SueloOut, Caja, CajaOut).
   
colocarAzulejos(_,_,_, PatronOut, PatronOut, SueloOut, SueloOut, CajaOut, CajaOut).

%Busca al ganador de la partida
search_ganador(_, NumJugadorInicial, NumJugador, _, ListaDatosJugador, Ganador):- %Situación en la que un jugador, que no es el inicial, es el ganador
   NumJugador \= NumJugadorInicial, %No es el primer jugador al que se le rellena su pared
   nth1(NumJugador, ListaDatosJugador, DatosJugador),
   nth1(2, DatosJugador, Pared),
   check_fin_juego(Pared, 0, _, Valor),
   Valor = 1, %Ganador
   Ganador = NumJugador,!.
   
search_ganador(Situacion, NumJugadorInicial, NumJugador, NumJugadores, ListaDatosJugador, Ganador):- %Situación en la que un jugador, que no es el inicial, no es el ganador
   NumJugador \= NumJugadorInicial, %No es el primer jugador al que se le rellena su pared
   nth1(NumJugador, ListaDatosJugador, DatosJugador),
   nth1(2, DatosJugador, Pared),
   check_fin_juego(Pared, 0, _, Valor),
   Valor \= 1, %No es el ganador
   get_siguiente_jugador(NumJugador, NumJugadores, _, NumSiguienteJugador),
   search_ganador(Situacion, NumJugadorInicial, NumSiguienteJugador, NumJugadores, ListaDatosJugador, Ganador),!.

search_ganador(Situacion, NumJugadorInicial, NumJugador, NumJugadores, ListaDatosJugador, Ganador):- %Situación en la que un jugador, es el inicial y es la primera vez que se estudia, no es el ganador
   NumJugador = NumJugadorInicial, %Es el primer jugador al que se le rellena su pared
   Situacion \= 0, %Primera vez que se estudia
   nth1(NumJugador, ListaDatosJugador, DatosJugador),
   nth1(2, DatosJugador, Pared),
   check_fin_juego(Pared, 0, _, Valor),
   Valor \= 1, %No es el ganador
   get_siguiente_jugador(NumJugador, NumJugadores, _, NumSiguienteJugador),
   search_ganador(0, NumJugadorInicial, NumSiguienteJugador, NumJugadores, ListaDatosJugador, Ganador),!.

search_ganador(Situacion, NumJugadorInicial, NumJugador, _, ListaDatosJugador, Ganador):- %Situación en la que un jugador, es el inicial y es la primera vez que se estudia, es el ganador
   NumJugador = NumJugadorInicial, %Es el primer jugador al que se le rellena su pared
   Situacion \= 0, %Primera vez que se estudia
   nth1(NumJugador, ListaDatosJugador, DatosJugador),
   nth1(2, DatosJugador, Pared),
   check_fin_juego(Pared, 0, _, Valor),
   Valor = 1, %Es el ganador
   Ganador = NumJugador,!.

search_ganador(Situacion, NumJugadorInicial, NumJugador, _, _, Ganador):- %Situación en la que un jugador, es el inicial y es la primera vez que se estudia, no es el ganador
   NumJugador = NumJugadorInicial, %Es el primer jugador al que se le rellena su pared
   Situacion = 0, %Primera vez que se estudia
   Ganador = -1,!. %No hay ganador

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

alicatado_paredes_jugadores(Situacion, NumJugadorInicial, NumJugador, NumJugadores, Supermatriz, SupermatrizOut):- %Situación en la que no se trabaja con el primer jugador de la ronda
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
   nth0(0, Lista_datos_jugador, LineasPatron),
   nth0(1, Lista_datos_jugador, Pared),
   nth0(2, Lista_datos_jugador, Suelo),
   alicatado_pared(1, LineasPatron, [], LineasPatronOut, Pared, [], ParedOut,Caja, CajaOut),
   writeln('Lineas de patron:'),
   writeln(LineasPatronOut),
   writeln('Pared:'),
   writeln(ParedOut),
   writeln('Caja:'),
   writeln(CajaOut),
   Datos_jugador_actualizados = [LineasPatronOut, ParedOut, Suelo],
   Datos_generales_actualizados = [Bolsa, Lista_factorias, CajaOut],
   nth1(NumJugador,Datos_jugadores_actualizados,Datos_jugador_actualizados, Lista_datos_otros_jugadores), %Unión de los datos jugador actual con el resto
   Supermatriz_actualizada = [Datos_generales_actualizados, Datos_jugadores_actualizados],
   get_siguiente_jugador(NumJugador, NumJugadores, _, NumSiguienteJugador),
   alicatado_paredes_jugadores(Situacion, NumJugadorInicial, NumSiguienteJugador, NumJugadores, Supermatriz_actualizada, SupermatrizOut),!.

alicatado_paredes_jugadores(Situacion, NumJugadorInicial, NumJugador, NumJugadores, Supermatriz, SupermatrizOut):- %Situación en la que se trabaja con el primer jugador de la ronda por primera vez
   NumJugador = NumJugadorInicial, %No es el primer jugador al que se le rellena su pared
   Situacion = 1, %Se pasa por primera vez por el primer jugador
   writeln(NumJugador),
   writeln(Supermatriz),
   nth0(0,Supermatriz,Datos_generales), %Se sacan los datos de la bolsa, las factorias, el centro y la caja
   nth0(1,Supermatriz,Datos_jugadores), %Se sacan los datos de las líneas de patrón, la pared y el suelo de cada jugador
   nth0(0,Datos_generales,Bolsa), %Cogemos la bolsa
   nth0(1,Datos_generales,Lista_factorias), %Cogemos la lista de factorias junto con el centro
   nth0(2,Datos_generales,Caja), %Cogemos la caja
   nth1(NumJugador,Datos_jugadores,Lista_datos_jugador, Lista_datos_otros_jugadores), %Separación datos jugador actual del resto
   nth0(0, Lista_datos_jugador, LineasPatron),
   nth0(1, Lista_datos_jugador, Pared),
   nth0(2, Lista_datos_jugador, Suelo),
   alicatado_pared(1, LineasPatron, [], LineasPatronOut, Pared, [], ParedOut,Caja, CajaOut),
   writeln('Lineas de patron:'),
   writeln(LineasPatronOut),
   writeln('Pared:'),
   writeln(ParedOut),
   writeln('Caja:'),
   writeln(CajaOut),
   Datos_jugador_actualizados = [LineasPatronOut, ParedOut, Suelo],
   Datos_generales_actualizados = [Bolsa, Lista_factorias, CajaOut],
   nth1(NumJugador,Datos_jugadores_actualizados,Datos_jugador_actualizados, Lista_datos_otros_jugadores), %Unión de los datos jugador actual con el resto
   Supermatriz_actualizada = [Datos_generales_actualizados, Datos_jugadores_actualizados],
   get_siguiente_jugador(NumJugador, NumJugadores, _, NumSiguienteJugador),
   alicatado_paredes_jugadores(0, NumJugadorInicial, NumSiguienteJugador, NumJugadores, Supermatriz_actualizada, SupermatrizOut),!.

alicatado_paredes_jugadores(_,_,_,_, SupermatrizOut, SupermatrizOut).

%Comprobar que la longitud de una línea de la pared tiene 5 azulejos
check_longitud_lista(LineaPared, Valor):- %Situación en la que tiene 5 azulejos
   length(LineaPared, LongitudLista),
   LongitudLista = 5,
   Valor is 1, !. %La lista tiene 5 elementos
    
check_longitud_lista(LineaPared, Valor):- %Situación en la que no tiene 5 azulejos
   length(LineaPared, LongitudLista),
   LongitudLista \= 5,
   Valor is 0, !. %La lista no tiene 5 elementos

%Comprueba la finalización o no de una partida
check_fin_juego(Pared, ValorAux, _, Valor):-%Situación en la que una de las filas tiene 5 azulejos
   ValorAux = 0, %Aún no se han encontrado ninguna fila con 5 azulejos
   length(Pared, LongitudPared),
   LongitudPared \=0, %Quedan aún más filas
   Pared = [PrimeraLinea|RestoLineas],
   check_longitud_lista(PrimeraLinea, ValorAux2),
   ValorAux2 = 1, %La fila se encuentra completa
   check_fin_juego(RestoLineas, ValorAux2, ValorAux2, Valor),!.

check_fin_juego(Pared, ValorAux, _,Valor):- %Situación en la que no quedan más filas a comprobar de la pared
   ValorAux = 0, %Aún no se han encontrado ninguna fila con 5 azulejos
   length(Pared, LongitudPared),
   LongitudPared = 0, %No quedan más filas
   ValorAux2 = -1,
   ValorAux3 = 0, %No hay ninguna fila completa
   check_fin_juego(_, ValorAux2, ValorAux3, Valor),!.

check_fin_juego(Pared, ValorAux, _, Valor):- %Situación en la que la fila estudiada de la pared no tiene 5 azulejos
   ValorAux = 0, %Aún no se han encontrado ninguna fila con 5 azulejos
   length(Pared, LongitudPared),
   LongitudPared \=0,%Quedan aún más filas
   Pared = [PrimeraLinea|RestoLineas],
   check_longitud_lista(PrimeraLinea, ValorAux2),
   ValorAux2 = 0,
   check_fin_juego(RestoLineas, ValorAux2, _, Valor),!.

check_fin_juego(_,_,Valor,Valor).

%Comprueba si la caja, la bolsa, el centro y las factorias estan vacias y actúa según sea conveniente.
check_rellenar_factorias(Caja,Bolsa,Valor):- %Situación en la que la bolsa, la caja, las factorias y el centro están vacías
   is_empty(Bolsa, ValorAux),
   ValorAux = 1,
   is_empty(Caja, ValorAux2),
   ValorAux2 = 1,
   Valor is 0,!.

check_rellenar_factorias(Caja,Bolsa,Valor):- %Situación en la que la bolsa, las factorias y el centro están vacías, pero la caja no
   is_empty(Bolsa, ValorAux),
   ValorAux = 1,
   is_empty(Caja, ValorAux2),
   ValorAux2 is 0, %Caja no vacía
   Valor is 1,!.

check_rellenar_factorias(_,Bolsa,Valor):- %Situación en la que las factorias y el centro están vacías, pero la bolsa no y el estado de la caja da igual
   is_empty(Bolsa, ValorAux),
   ValorAux is 0, %Caja no vacía
   Valor is 2,!.

%Realiza el llenado de la bolsa, si es necesario, y de las factorías si es posible
realizar_rellenado_factorias(CajaIn,CajaOut,BolsaIn,BolsaOut,FactoriasIn,FactoriasOut,Valor):- %Situación en la que la bolsa, la caja, las factorias y el centro están vacías
   Valor = 0,
   BolsaOut = BolsaIn,
   CajaOut = CajaIn,
   FactoriasOut = FactoriasIn.
   
realizar_rellenado_factorias(CajaIn,CajaOut,_,BolsaOut,FactoriasIn,FactoriasOut,Valor):- %Situación en la que la bolsa, las factorias y el centro están vacías, pero la caja no
   Valor = 1,
   BolsaAux = CajaIn, %Se rellena la bosa con respecto a lo de la caja
   CajaOut = [],
   rellenar_factorias_generadas(FactoriasIn,[],FactoriasOut,BolsaAux,BolsaOut).

realizar_rellenado_factorias(CajaIn,CajaOut,BolsaIn,BolsaOut,FactoriasIn,FactoriasOut,Valor):- %Situación en la que las factorias y el centro están vacías, pero la bolsa no y el estado de la caja da igual
   Valor = 2,
   rellenar_factorias_generadas(FactoriasIn,[],FactoriasOut,BolsaIn,BolsaOut),
   CajaOut = CajaIn.
