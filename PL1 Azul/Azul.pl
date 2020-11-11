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
   (LongitudFactoria<4), %Las factorías solo pueden tener 4 azulejos
   length(BolsaIn,LongitudBolsa),
   LongitudBolsaF is (LongitudBolsa-1),
   devolverrandom(0,LongitudBolsaF,NumeroAleatorio), %Devuelve una posición aleatoria de la bolsa
   nth0(NumeroAleatorio,BolsaIn,FichaElegida),
   select(FichaElegida,BolsaIn,BolsaAux),
   FactoriaAux2=[FichaElegida|FactoriaAux],!,
   rellenar_factorias(FactoriaAux2,FactoriaOut,BolsaAux,BolsaOut).

rellenar_factorias(FactOut,FactOut,BolsaOut,BolsaOut).

%Reglas para generar las factorias
generar_factorias(NumJugadores, ListaOut):- %Genera factorías para 2 jugadores
   NumJugadores = 2, !,
   generar_factorias_aux(6, [], ListaOut). %5 factorías y el centro de la mesa

generar_factorias(NumJugadores, ListaOut):- %Genera factorias para 3 jugadores
   NumJugadores = 3, !,
   generar_factorias_aux(8, [], ListaOut). %7 factorías y el centro de la mesa

generar_factorias(NumJugadores, ListaOut):- %Genera factorias para 4 jugadores
   NumJugadores = 4, !,
   generar_factorias_aux(10, [], ListaOut). %9 factorías y el centro de la mesa
   
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
coger_color(ListaColores, ColorSeleccionado, ListaColor, ListaColorOut, CentroMesa, CentroMesaOut):- %Situación en la que el azulejo es del mismo color al pasado por parámetro
    ListaColores = [Primero|Resto],
    member(ColorSeleccionado, Primero),
    append([Primero], ListaColor, ListaColorAux),
    coger_color(Resto, ColorSeleccionado, ListaColorAux, ListaColorOut, CentroMesa, CentroMesaOut).

coger_color(ListaColores, ColorSeleccionado, ListaColor, ListaColorOut, CentroMesa, CentroMesaOut):- %Situación en la que el azulejo no es del mismo color al pasado por parámetro
    ListaColores = [Primero|Resto],
    member(ColorSeleccionado, Primero),
    not(member(ColorSeleccionado, Primero)),
    append([Primero], CentroMesa, CentroMesaAux),
    coger_color(Resto, ColorSeleccionado, ListaColor, ListaColorOut, CentroMesaAux, CentroMesaOut).

coger_color(_, _, ListaColorOut, ListaColorOut, CentroMesaOut, CentroMesaOut).

%Generar lista de colores a partir de las fichas de una factoria
get_lista_colores(Factoria, ListaColores, ListaColoresOut):- %Situación en la que es un nuevo color a los guardados en la lista
    Factoria = [Primero|Resto],
    \+(member(Primero, ListaColores)),
    append(ListaColores, [Primero], ListaColoresAux),
    get_lista_colores(Resto, ListaColoresAux, ListaColoresOut).

get_lista_colores(Factoria, ListaColores, ListaColoresOut):- %Situación en la que no es un nuevo color a los guardados en la lista
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
    generar_lista_datos_jugador(NumJugadores, [], ListaDatosJugador),
    append(Lista_aux, [Lista_datos_comunes], Lista_aux2),
    append(Lista_aux2, [ListaDatosJugador], Lista_aux3),
    generar_supermatriz(NumJugadores, Lista_aux3, Supermatriz).

generar_supermatriz(_, Supermatriz, Supermatriz).

%genera la lista de los datos de cada jugador
generar_lista_datos_jugador(NumJugadores, ListaDatosJugadorAux, ListaDatosJugador):-
    NumJugadores \= 0, !,
    linea_patrones(LineasPatrones), %Genera las líneas de patrón
    pared(Pared), %Genera la pared
    suelo(Suelo), %Genera el suelo
    ListaDatosJugadorAux2 = ([LineasPatrones, Pared, Suelo]),
    NumJugadoresAux is (NumJugadores-1),
    append(ListaDatosJugadorAux, [ListaDatosJugadorAux2], ListaDatosJugadorAux3),
    generar_lista_datos_jugador(NumJugadoresAux, ListaDatosJugadorAux3, ListaDatosJugador).

generar_lista_datos_jugador(_, ListaDatosJugador, ListaDatosJugador).

%Rellena una línea de patrón mientras que tenga espacio
rellenarPatron(Fila, Cantidad, Color, PatronIn, PatronOut, SueloIn, SueloOut, CajaIn, CajaOut):- %Situación en la que hay aún espacio disponible en la línea de patrón
   length(PatronIn, NumAzulejosColocados),
   EspacioDisponible is Fila-NumAzulejosColocados,
   EspacioDisponible \= 0,
   Cantidad \= 0,
   append(PatronIn, [Color], Patron),
   CantidadAux is Cantidad-1,
   rellenarPatron(Fila, CantidadAux, Color, Patron, PatronOut, SueloIn, SueloOut, CajaIn, CajaOut),!.

rellenarPatron(Fila, Cantidad, Color, PatronIn, PatronOut, SueloIn, SueloOut, CajaIn, CajaOut):- %Situación en la que no hay espacio disponible en la línea de patrón
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
rellenar_suelo(SueloIn,SueloOut,Cantidad,Color,CajaIn,CajaOut):- %Situación en la que hay aún espacio disponible en el suelo
   length(SueloIn, NumAzulejosColocados),
   max_tamanno_suelo(TamannoSuelo),
   EspacioDisponible is TamannoSuelo - NumAzulejosColocados,
   EspacioDisponible \= 0,
   Cantidad \= 0, !,
   append(SueloIn, [Color], Suelo),
   CantidadAux is Cantidad-1,
   rellenar_suelo(Suelo,SueloOut,CantidadAux,Color,CajaIn,CajaOut).

rellenar_suelo(SueloIn,SueloOut,Cantidad,Color,CajaIn,CajaOut):- %Situación en la que no hay espacio disponible en el suelo
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
get_azulejo_factoria(Factoria, Color, ListaAux, ListaFichas, ListaCentroAux, ListaCentro):- %Situación en la que el color del azulejo del estudiado es igual al color pasado por parámetro
   Factoria = ([Primero | Resto]),
   Primero = Color, !,
   append(ListaAux, [Primero], ListaAux2),
   get_azulejo_factoria(Resto, Color, ListaAux2, ListaFichas, ListaCentroAux, ListaCentro).

get_azulejo_factoria(Factoria, Color, ListaAux, ListaFichas, ListaCentroAux, ListaCentro):- %Situación en la que el color del azulejo del estudiado es distinto al color pasado por parámetro
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
alicatado_pared(FilaActual, LineasPatron, LineasPatronAux, LineasPatronOut, Pared, ParedAux, ParedOut,CajaIn, CajaOut):- %Situación en la que la fila estudiada de la pared está vacía
    LineasPatron = [Primero|Resto], %Separación de la cabeza de las líneas de patrón de la cola de la misma
    is_empty(Primero, Valor),
    Valor = 1, %Lista está vacía
    Pared = [PrimeraFila|RestoFilas],%Separación de la cabeza de las filas de la pared de la cola de la misma
    append(ParedAux, [PrimeraFila], ParedAux2), %Se va reconstruyendo la pared con lo que había de líneas anteriores de la pared con la que se acaba de estudiar
    append(LineasPatronAux, [Primero], LineasPatronAux2), %Se va reconstruyendo las líneas de patrón con lo que había de líneas anteriores de la misma con la que se acaba de estudiar
    FilaSiguiente is FilaActual+1, %Se obtiene la fila siguiente a estudiar
    alicatado_pared(FilaSiguiente, Resto, LineasPatronAux2, LineasPatronOut, RestoFilas, ParedAux2, ParedOut, CajaIn, CajaOut),!.

alicatado_pared(FilaActual, LineasPatron, LineasPatronAux, LineasPatronOut, Pared, ParedAux, ParedOut,CajaIn, CajaOut):- %Situación en la que el color no se encuentra en la fila estudiada de la pared
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
    length(Primero, NumFichasColocadas), %Se obtiene el número de azulejos colocados en una línea de patrón
    NumFichasColocadas < FilaActual, %!, %El número de elemento en la fila no es igual al número de fichas que se pueden colocar en dicha fila
    Pared = [PrimeraFila|RestoFilas],%Separación de la cabeza de las filas de la pared de la cola de la misma
    append(ParedAux, [PrimeraFila], ParedAux2), %Se va reconstruyendo la pared con lo que había de líneas anteriores de la pared con la que se acaba de estudiar
    append(LineasPatronAux, [Primero], LineasPatronAux2), %Se va reconstruyendo las líneas de patrón con lo que había de líneas anteriores de la misma con la que se acaba de estudiar
    FilaSiguiente is FilaActual+1, %Se obtiene la fila siguiente a estudiar
    alicatado_pared(FilaSiguiente, Resto, LineasPatronAux2, LineasPatronOut, RestoFilas, ParedAux2, ParedOut, CajaIn, CajaOut),!.

alicatado_pared(FilaActual, LineasPatron, LineasPatronAux, LineasPatronOut, Pared, ParedAux, ParedOut,CajaIn, CajaOut):- %Situación en la que el color ya se encuentra en la fila estudiada de la pared
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
get_azulejo(NumFactoria, MaxNumFactoria, ListaFactorias, ListaFactoriasOut, ListaFichas, ListaFichasOut, _, Color):- %Situación en la que se trabaja con una de las factorias
   NumFactoria \= -1,
   NumFactoria \= MaxNumFactoria, %Separación de los casos en los que se trabaja con cualquier factoria y no al centro
   nth1(MaxNumFactoria, ListaFactorias, Centro, ListaFactoriasSinCentro), %Separación centro del resto de factorias
   coger_factoria(ListaFactorias,NumFactoria, Factoria),
   writeln('Fichas factoria escogida:'),
   imprimir_lista(Factoria),
   get_lista_colores(Factoria, [], ListaColores),
   writeln(''),
   writeln('Lista de colores disponible en esta factoria:'),
   imprimir_lista(ListaColores),
   pedir_color(Factoria, ColorSeleccionado),
   get_azulejo_factoria(Factoria, ColorSeleccionado, ListaFichas, ListaFichasAux, [], ListaCentro),
   append(Centro, ListaCentro, ListaCentroAux), %Se actualiza el centro
   nth1(NumFactoria, ListaFactoriasSinCentro, _, RestoFactorias), %Separa la factoria usada del resto.
   nth1(NumFactoria, ListaFactoriasOutAux, [], RestoFactorias),    %Coloca la factoria usada modificada al resto
   nth1(MaxNumFactoria, ListaFactoriasOutAux2, ListaCentroAux, ListaFactoriasOutAux),    %Coloca el centro junto con las factorias
   get_azulejo(-1, MaxNumFactoria, ListaFactoriasOutAux2, ListaFactoriasOut, ListaFichasAux, ListaFichasOut, ColorSeleccionado, Color).

get_azulejo(NumFactoria, MaxNumFactoria, ListaFactorias, ListaFactoriasOut, ListaFichas, ListaFichasOut, _, Color):- %Situación en la que se trabaja con el centro de la mesa
   NumFactoria \= -1,
   NumFactoria = MaxNumFactoria, %Separación de los casos en los que se trabaja con cualquier factoria y no al centro
   nth1(MaxNumFactoria, ListaFactorias, Centro, ListaFactoriasSinCentro), %Separación centro del resto de factorias
   writeln('Fichas factoria escogida:'),
   imprimir_lista(Centro),
   get_lista_colores(Centro, [], ListaColores),
   writeln(''),
   writeln('Lista de colores disponible en el centro:'),
   imprimir_lista(ListaColores),
   pedir_color(Centro, ColorSeleccionado),
   get_azulejo_centro(Centro, ColorSeleccionado, ListaFichas, ListaFichasAux, [], ListaCentroAux),
   nth1(MaxNumFactoria, ListaFactorias_con_centro, ListaCentroAux, ListaFactoriasSinCentro),    %Coloca el centro junto con las factorias
   get_azulejo(-1, MaxNumFactoria, ListaFactorias_con_centro, ListaFactoriasOut, ListaFichasAux, ListaFichasOut, ColorSeleccionado, Color).

get_azulejo(_,_,ListaFactoriasOut,ListaFactoriasOut, ListaFichasOut, ListaFichasOut, Color, Color).

%Realiza la ejecución del juego
jugar():-
   pedir_numero_jugadores(NumJugadores),
   generar_supermatriz(NumJugadores, [], Supermatriz),
   writeln('Situacion inicial'),
   writeln(Supermatriz),
   ejecucion_ronda(1, 1, 1, 1, NumJugadores, Supermatriz, Ganador),
   show_ganador(Ganador),!.

%Realiza las operaciones del funcionamiento del juego
ejecucion_ronda(Ronda, Turno, NumJugadorInicial, NumJugador, NumJugadores, Supermatriz, Ganador):-  %Situación en la que quedan azulejos en las factorias o en el centro de la mesa
   nth0(0,Supermatriz,DatosGenerales), %Se sacan los datos de la bolsa, las factorias, el centro y la caja
   nth0(1,DatosGenerales,ListaFactorias), %Cogemos la lista de factorias junto con el centro
   is_empty_lista_de_listas(ListaFactorias, 1, _, Valor),
   Valor = 0, %Hay al menos un azulejo entre las distintas factorias y el centro de la mesa
   write('Ronda '),
   write(Ronda),
   write(', Turno '),
   writeln(Turno),
   write('Turno del jugador '),
   writeln(NumJugador),
   realizar_oferta_factorias(NumJugador, Supermatriz, Supermatriz_actualizada),
   get_siguiente_jugador(NumJugador, NumJugadores, _, NumSiguienteJugador),
   SiguienteTurno is Turno+1,
   ejecucion_ronda(Ronda, SiguienteTurno, NumJugadorInicial, NumSiguienteJugador, NumJugadores, Supermatriz_actualizada, Ganador),!.

ejecucion_ronda(Ronda, _, NumJugadorInicial, NumJugador, NumJugadores, Supermatriz, Ganador):- %Situación en la que se rellenan las factorías y se pasa a la siguiente ronda del juego
   nth0(0,Supermatriz,DatosGenerales), %Se sacan los datos de la bolsa, las factorias, el centro y la caja
   nth0(1,DatosGenerales,ListaFactorias), %Cogemos la lista de factorias junto con el centro
   is_empty_lista_de_listas(ListaFactorias, 1, _, Valor),
   Valor \= 0, %No hay ningún azulejo entre las distintas factorias y el centro de la mesa
   alicatado_paredes_jugadores(1, NumJugadorInicial, NumJugadorInicial, NumJugadores, Supermatriz, Supermatriz_actualizada),
   nth0(0,Supermatriz_actualizada,DatosGeneralesAux), %Se sacan los datos de la bolsa, las factorias, el centro y la caja
   nth0(1,DatosGeneralesAux,ListaFactoriasAux), %Cogemos la lista de factorias junto con el centro
   nth0(1,Supermatriz_actualizada,DatosJugadores), %Se sacan los datos de las líneas de patrón, la pared y el suelo de cada jugador
   nth0(0,DatosGeneralesAux,Bolsa), %Cogemos la bolsa
   nth0(2,DatosGeneralesAux,Caja), %Cogemos la caja
   search_ganador(1, NumJugadorInicial, NumJugadorInicial, NumJugadores, DatosJugadores, GanadorAux),
   GanadorAux = -1, %Se sigue jugando si no hay ningún jugador que haya completado una línea de la pared
   check_rellenar_factorias(Caja,Bolsa,ValorAux),
   ValorAux \= 0, %Situaciones en las que aún quedan azulejos
   realizar_rellenado_factorias(Caja,CajaOut,Bolsa,BolsaOut,ListaFactoriasAux,ListaFactoriasOut,ValorAux),
   DatosGenerales_actualizados = [BolsaOut, ListaFactoriasOut, CajaOut],
   Supermatriz_actualizada2 = [DatosGenerales_actualizados, DatosJugadores],
   writeln(Supermatriz_actualizada2),
   SiguienteRonda is Ronda+1,
   ejecucion_ronda(SiguienteRonda, 1, NumJugador, NumJugador, NumJugadores, Supermatriz_actualizada2, Ganador),!.

ejecucion_ronda(_, _, NumJugadorInicial, _, NumJugadores, Supermatriz, Ganador):- %Situación en la que se acaba el juego por no quedar azulejos
   nth0(0,Supermatriz,DatosGenerales), %Se sacan los datos de la bolsa, las factorias, el centro y la caja
   nth0(1,DatosGenerales,ListaFactorias), %Cogemos la lista de factorias junto con el centro
   is_empty_lista_de_listas(ListaFactorias, 1, _, Valor),
   Valor \= 0, %No hay ningún azulejo entre las distintas factorias y el centro de la mesa
   alicatado_paredes_jugadores(1, NumJugadorInicial, NumJugadorInicial, NumJugadores, Supermatriz, Supermatriz_actualizada),
   nth0(1,Supermatriz_actualizada,DatosJugadores), %Se sacan los datos de las líneas de patrón, la pared y el suelo de cada jugador
   nth0(0,DatosGeneralesAux,Bolsa), %Cogemos la bolsa
   nth0(2,DatosGeneralesAux,Caja), %Cogemos la caja
   search_ganador(1, NumJugadorInicial, NumJugadorInicial, NumJugadores, DatosJugadores, GanadorAux),
   GanadorAux = -1, %Se sigue jugando si no hay ningún jugador que haya completado una línea de la pared
   check_rellenar_factorias(Caja,Bolsa,ValorAux),
   ValorAux = 0, %Situaciones en las que el juego ha llegado a su fin
   Ganador = GanadorAux,!.

ejecucion_ronda(_, _, NumJugadorInicial, _, NumJugadores, Supermatriz, Ganador):- %Situación en la que se acaba con la victoria de uno de los jugadores
   nth0(0,Supermatriz,DatosGenerales), %Se sacan los datos de la bolsa, las factorias, el centro y la caja
   nth0(1,DatosGenerales,ListaFactorias), %Cogemos la lista de factorias junto con el centro
   is_empty_lista_de_listas(ListaFactorias, 1, _, Valor),
   Valor \= 0, %No hay ningún azulejo entre las distintas factorias y el centro de la mesa
   alicatado_paredes_jugadores(1, NumJugadorInicial, NumJugadorInicial, NumJugadores, Supermatriz, Supermatriz_actualizada),
   nth0(1,Supermatriz_actualizada,DatosJugadores), %Se sacan los datos de las líneas de patrón, la pared y el suelo de cada jugador
   search_ganador(1, NumJugadorInicial, NumJugadorInicial, NumJugadores, DatosJugadores, GanadorAux),
   GanadorAux \= -1, %Se sigue jugando si no hay ningún jugador que haya completado una línea de la pared
   Ganador = GanadorAux,!.

%Se realiza la oferta de azulejos de las distintas factorias
realizar_oferta_factorias(NumJugador, Supermatriz, SupermatrizActualizada):-
   nth0(0,Supermatriz,DatosGenerales), %Se sacan los datos de la bolsa, las factorias, el centro y la caja
   nth0(1,DatosGenerales,ListaFactorias), %Cogemos la lista de factorias junto con el centro
   nth0(1,Supermatriz,DatosJugadores), %Se sacan los datos de las líneas de patrón, la pared y el suelo de cada jugador
   nth0(0,DatosGenerales,Bolsa), %Cogemos la bolsa
   nth0(2,DatosGenerales,Caja), %Cogemos la caja
   length(ListaFactorias, LongitudListaFactorias),%Obtenemos la longitud de lista de factorias
   nth1(LongitudListaFactorias, ListaFactorias, Centro, ListaFactoriasSinCentro), %Separación del centro del resto de factorías
   nth1(NumJugador,DatosJugadores,ListaDatosJugador, ListaDatosOtrosJugadores), %Separación datos jugador actual del resto
   nth0(0, ListaDatosJugador, LineasPatron),
   nth0(1, ListaDatosJugador, Pared),
   nth0(2, ListaDatosJugador, Suelo),
   
   writeln('Línes de patrones'),
   mostrar_pared(LineasPatron),
   writeln('Su mosaico:'),
   mostrar_pared(Pared),
   write('Su suelo:'),
   mostrar_suelo(Suelo),
   
   mostrar_lista_factorias(1, ListaFactoriasSinCentro),
   writeln('Contenido centro de la mesa:'),
   mostrar_centro(Centro),
   
   %Oferta de factorías
   pedir_factoria(ListaFactorias, NumFactoria),
   get_azulejo(NumFactoria, LongitudListaFactorias, ListaFactorias, ListaFactoriasOut, [], ListaFichas,_, ColorSeleccionado),
   writeln(''),
   writeln(ListaFactoriasOut),
   is_space_available(ColorSeleccionado,LineasPatron, Valor2),
   colocar_azulejos(ColorSeleccionado, ListaFichas, Valor2, LineasPatron, PatronOut, Suelo, SueloOut, Caja, CajaOut),
   DatosJugadorActualizados = [PatronOut, Pared, SueloOut],
   nth1(NumJugador,DatosJugadoresActualizados,DatosJugadorActualizados, ListaDatosOtrosJugadores), %Unión de los datos jugador actual con el resto
   DatosGenerales_actualizados = [Bolsa, ListaFactoriasOut, CajaOut],
   Supermatriz_actualizadaAux = [DatosGenerales_actualizados, DatosJugadoresActualizados],
   writeln(Supermatriz_actualizadaAux),
   SupermatrizActualizada = Supermatriz_actualizadaAux.

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

%Se realiza la colocación de los azulejos en una línea de patrón
colocar_azulejos(ColorSeleccionado, ListaFichas, Valor, LineasPatron, PatronOut, Suelo, SueloOut, Caja, CajaOut):- %Situación en la que entra al menos algunas de las fichas
   Valor = 1,
   pedir_linea_patron(LineasPatron, ColorSeleccionado, FilaLineaPatron),
   length(ListaFichas, NumFichas),
   nth1(FilaLineaPatron, LineasPatron, LineaPatron, OtrasLineasPatron), %Se separa la línea de patrón a modificar de las demás
   rellenarPatron(FilaLineaPatron,NumFichas,ColorSeleccionado,LineaPatron,PatronAux,Suelo,SueloAux,Caja,CajaAux),
   nth1(FilaLineaPatron, LineasPatronAux, PatronAux, OtrasLineasPatron), %Se une la línea de patrón modificada con las demás
   colocar_azulejos(ColorSeleccionado, ListaFichas, -1, LineasPatronAux, PatronOut, SueloAux, SueloOut, CajaAux, CajaOut),!.

colocar_azulejos(ColorSeleccionado, ListaFichas, Valor, LineasPatron, PatronOut, Suelo, SueloOut, Caja, CajaOut):- %Situación en la que no entran más fichas ni en las líneas de patrón ni en el suelo
   Valor = 0,
   length(Suelo, NumFichasSuelo),
   NumFichasSuelo = 7,
   writeln('Dado que no entrar los azulejos escogidos en ninguna línea de patrón, se van a distribuir entre el suelo, lugar prioritario, y en la caja, si no queda espacio en el suelo.'),
   append(Caja, ListaFichas, CajaAux),
   colocar_azulejos(ColorSeleccionado, ListaFichas, -1, LineasPatron, PatronOut, Suelo, SueloOut, CajaAux, CajaOut),!.

colocar_azulejos(ColorSeleccionado, ListaFichas, Valor, LineasPatron, PatronOut, Suelo, SueloOut, Caja, CajaOut):- %Situación en la que no entran más fichas en ninguna línea de patrón
   Valor = 0,
   length(Suelo, NumFichasSuelo),
   NumFichasSuelo \= 7,
   ListaFichas = [Primero|Resto],
   append(Suelo, [Primero], SueloAux),
   colocar_azulejos(ColorSeleccionado, Resto, Valor, LineasPatron, PatronOut, SueloAux, SueloOut, Caja, CajaOut).
   
colocar_azulejos(_,_,_, PatronOut, PatronOut, SueloOut, SueloOut, CajaOut, CajaOut).

%Busca al ganador de la partida
search_ganador(_, NumJugadorInicial, NumJugador, _, ListaDatosJugador, Ganador):- %Situación en la que un jugador, que no es el inicial, es el ganador
   NumJugador \= NumJugadorInicial, %No es el primer jugador al que se le rellena su pared
   nth1(NumJugador, ListaDatosJugador, DatosJugador),
   nth1(2, DatosJugador, Pared),
   check_fin_ejecucion_ronda(Pared, 0, _, Valor),
   Valor = 1, %Ganador
   Ganador = NumJugador,!.
   
search_ganador(Situacion, NumJugadorInicial, NumJugador, NumJugadores, ListaDatosJugador, Ganador):- %Situación en la que un jugador, que no es el inicial, no es el ganador
   NumJugador \= NumJugadorInicial, %No es el primer jugador al que se le rellena su pared
   nth1(NumJugador, ListaDatosJugador, DatosJugador),
   nth1(2, DatosJugador, Pared),
   check_fin_ejecucion_ronda(Pared, 0, _, Valor),
   Valor \= 1, %No es el ganador
   get_siguiente_jugador(NumJugador, NumJugadores, _, NumSiguienteJugador),
   search_ganador(Situacion, NumJugadorInicial, NumSiguienteJugador, NumJugadores, ListaDatosJugador, Ganador),!.

search_ganador(Situacion, NumJugadorInicial, NumJugador, NumJugadores, ListaDatosJugador, Ganador):- %Situación en la que un jugador, es el inicial y es la primera vez que se estudia, no es el ganador
   NumJugador = NumJugadorInicial, %Es el primer jugador al que se le rellena su pared
   Situacion \= 0, %Primera vez que se estudia
   nth1(NumJugador, ListaDatosJugador, DatosJugador),
   nth1(2, DatosJugador, Pared),
   check_fin_ejecucion_ronda(Pared, 0, _, Valor),
   Valor \= 1, %No es el ganador
   get_siguiente_jugador(NumJugador, NumJugadores, _, NumSiguienteJugador),
   search_ganador(0, NumJugadorInicial, NumSiguienteJugador, NumJugadores, ListaDatosJugador, Ganador),!.

search_ganador(Situacion, NumJugadorInicial, NumJugador, _, ListaDatosJugador, Ganador):- %Situación en la que un jugador, es el inicial y es la primera vez que se estudia, es el ganador
   NumJugador = NumJugadorInicial, %Es el primer jugador al que se le rellena su pared
   Situacion \= 0, %Primera vez que se estudia
   nth1(NumJugador, ListaDatosJugador, DatosJugador),
   nth1(2, DatosJugador, Pared),
   check_fin_ejecucion_ronda(Pared, 0, _, Valor),
   Valor = 1, %Es el ganador
   Ganador = NumJugador,!.

search_ganador(Situacion, NumJugadorInicial, NumJugador, _, _, Ganador):- %Situación en la que un jugador, es el inicial y es la primera vez que se estudia, no es el ganador
   NumJugador = NumJugadorInicial, %Es el primer jugador al que se le rellena su pared
   Situacion = 0, %Primera vez que se estudia
   Ganador = -1,!. %No hay ganador

%Obtiene el siguiente jugador a jugar
get_siguiente_jugador(NumJugador, NumJugadores, _, NumJugadorOut):- %Situación en la que no se ha llegado al último jugador
   NumJugador > 0,
   NumJugador < NumJugadores,
   SiguienteJugador is NumJugador+1,
   NumJugadorAux = 0,
   get_siguiente_jugador(NumJugadorAux, NumJugadores, SiguienteJugador, NumJugadorOut),!.

get_siguiente_jugador(NumJugador, NumJugadores, _, NumJugadorOut):- %Situación en la que se ha llegado al último jugador
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
   nth0(0,Supermatriz,DatosGenerales), %Se sacan los datos de la bolsa, las factorias, el centro y la caja
   nth0(1,Supermatriz,DatosJugadores), %Se sacan los datos de las líneas de patrón, la pared y el suelo de cada jugador
   nth0(0,DatosGenerales,Bolsa), %Cogemos la bolsa
   nth0(1,DatosGenerales,ListaFactorias), %Cogemos la lista de factorias junto con el centro
   nth0(2,DatosGenerales,Caja), %Cogemos la caja
   nth1(NumJugador,DatosJugadores,ListaDatosJugador, ListaDatosOtrosJugadores), %Separación datos jugador actual del resto
   nth0(0, ListaDatosJugador, LineasPatron),
   nth0(1, ListaDatosJugador, Pared),
   nth0(2, ListaDatosJugador, Suelo),
   alicatado_pared(1, LineasPatron, [], LineasPatronOut, Pared, [], ParedOut,Caja, CajaOut),
   
   write('Resultado alicatado para jugador '),
   write(NumJugador),
   writeln(':'),
   writeln('Línes de patrones'),
   mostrar_pared(LineasPatronOut),
   writeln('Su mosaico:'),
   mostrar_pared(ParedOut),
   write('Su suelo:'),
   mostrar_suelo(Suelo),
   
   DatosJugadorActualizados = [LineasPatronOut, ParedOut, Suelo],
   DatosGenerales_actualizados = [Bolsa, ListaFactorias, CajaOut],
   nth1(NumJugador,DatosJugadoresActualizados,DatosJugadorActualizados, ListaDatosOtrosJugadores), %Unión de los datos jugador actual con el resto
   Supermatriz_actualizada = [DatosGenerales_actualizados, DatosJugadoresActualizados],
   get_siguiente_jugador(NumJugador, NumJugadores, _, NumSiguienteJugador),
   alicatado_paredes_jugadores(Situacion, NumJugadorInicial, NumSiguienteJugador, NumJugadores, Supermatriz_actualizada, SupermatrizOut),!.

alicatado_paredes_jugadores(Situacion, NumJugadorInicial, NumJugador, NumJugadores, Supermatriz, SupermatrizOut):- %Situación en la que se trabaja con el primer jugador de la ronda por primera vez
   NumJugador = NumJugadorInicial, %No es el primer jugador al que se le rellena su pared
   Situacion = 1, %Se pasa por primera vez por el primer jugador
   writeln(NumJugador),
   writeln(Supermatriz),
   nth0(0,Supermatriz,DatosGenerales), %Se sacan los datos de la bolsa, las factorías, el centro y la caja
   nth0(1,Supermatriz,DatosJugadores), %Se sacan los datos de las líneas de patrón, la pared y el suelo de cada jugador
   nth0(0,DatosGenerales,Bolsa), %Cogemos la bolsa
   nth0(1,DatosGenerales,ListaFactorias), %Cogemos la lista de factorías junto con el centro
   nth0(2,DatosGenerales,Caja), %Cogemos la caja
   nth1(NumJugador,DatosJugadores,ListaDatosJugador, ListaDatosOtrosJugadores), %Separación datos jugador actual del resto
   nth0(0, ListaDatosJugador, LineasPatron),
   nth0(1, ListaDatosJugador, Pared),
   nth0(2, ListaDatosJugador, Suelo),
   alicatado_pared(1, LineasPatron, [], LineasPatronOut, Pared, [], ParedOut,Caja, CajaOut),
   
   write('Resultado alicatado para jugador '),
   write(NumJugador),
   writeln(':'),
   writeln('Línes de patrones'),
   mostrar_pared(LineasPatronOut),
   writeln('Su mosaico:'),
   mostrar_pared(ParedOut),
   write('Su suelo:'),
   mostrar_suelo(Suelo),
   
   DatosJugadorActualizados = [LineasPatronOut, ParedOut, Suelo],
   DatosGenerales_actualizados = [Bolsa, ListaFactorias, CajaOut],
   nth1(NumJugador,DatosJugadoresActualizados,DatosJugadorActualizados, ListaDatosOtrosJugadores), %Unión de los datos jugador actual con el resto
   Supermatriz_actualizada = [DatosGenerales_actualizados, DatosJugadoresActualizados],
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
check_fin_ejecucion_ronda(Pared, ValorAux, _, Valor):-%Situación en la que una de las filas tiene 5 azulejos
   ValorAux = 0, %Aún no se han encontrado ninguna fila con 5 azulejos
   length(Pared, LongitudPared),
   LongitudPared \=0, %Quedan aún más filas
   Pared = [PrimeraLinea|RestoLineas],
   check_longitud_lista(PrimeraLinea, ValorAux2),
   ValorAux2 = 1, %La fila se encuentra completa
   check_fin_ejecucion_ronda(RestoLineas, ValorAux2, ValorAux2, Valor),!.

check_fin_ejecucion_ronda(Pared, ValorAux, _,Valor):- %Situación en la que no quedan más filas a comprobar de la pared
   ValorAux = 0, %Aún no se han encontrado ninguna fila con 5 azulejos
   length(Pared, LongitudPared),
   LongitudPared = 0, %No quedan más filas
   ValorAux2 = -1,
   ValorAux3 = 0, %No hay ninguna fila completa
   check_fin_ejecucion_ronda(_, ValorAux2, ValorAux3, Valor),!.

check_fin_ejecucion_ronda(Pared, ValorAux, _, Valor):- %Situación en la que la fila estudiada de la pared no tiene 5 azulejos
   ValorAux = 0, %Aún no se han encontrado ninguna fila con 5 azulejos
   length(Pared, LongitudPared),
   LongitudPared \=0,%Quedan aún más filas
   Pared = [PrimeraLinea|RestoLineas],
   check_longitud_lista(PrimeraLinea, ValorAux2),
   ValorAux2 = 0,
   check_fin_ejecucion_ronda(RestoLineas, ValorAux2, _, Valor),!.

check_fin_ejecucion_ronda(_,_,Valor,Valor).

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
   %writeln('GUAU'),
   Valor = 1,
   BolsaAux = CajaIn, %Se rellena la bosa con respecto a lo de la caja
   CajaOut = [],
   rellenar_factorias_generadas(FactoriasIn,[],FactoriasOut,BolsaAux,BolsaOut).

realizar_rellenado_factorias(CajaIn,CajaOut,BolsaIn,BolsaOut,FactoriasIn,FactoriasOut,Valor):- %Situación en la que las factorias y el centro están vacías, pero la bolsa no y el estado de la caja da igual
   %writeln('GUAU2'),
   Valor = 2,
   rellenar_factorias_generadas(FactoriasIn,[],FactoriasOut,BolsaIn,BolsaOut),
   CajaOut = CajaIn.

%Muestra la pared o mosaico de un jugador
mostrar_pared(Pared):- %Situación para las filas intermedias y la última
   length(Pared, NumFilas),
   NumFilas \= 0,
   NumFilas \= 5,
   Pared = [Primero|Resto],
   mostrar_fila(Primero, 1, 5),
   writeln(''),
   writeln('+---------+'),
   mostrar_pared(Resto),!.
   
mostrar_pared(Pared):- %Situación para la primera fila
   length(Pared, NumFilas),
   NumFilas = 5,
   Pared = [Primero|Resto],
   writeln('+---------+'),
   mostrar_fila(Primero, 1, 5),
   writeln(''),
   writeln('+---------+'),
   mostrar_pared(Resto),!.

mostrar_pared(Pared):- %Situación de fin
   length(Pared, NumFilas),
   NumFilas = 0,
   write(''),!.

%Muestra una fila de la pared
mostrar_fila(Fila, NumElemento, NumElemPorFila):- %Situación en la que quedan elementos en la fila y no es el primer elemento de la fila
   length(Fila, NumElementos),
   NumElementos \= 0,
   NumElemento > 1,
   Fila = [Primero|Resto],
   write(Primero),
   write('|'),
   NumElementoAux is NumElemento+1,
   NumElementosAux is NumElemPorFila-1,
   mostrar_fila(Resto,NumElementoAux, NumElementosAux),!.

mostrar_fila(Fila, NumElemento, NumElemPorFila):- %Situación en la que quedan elementos en la fila y es el primer elemento de la fila
   length(Fila, NumElementos),
   NumElementos \= 0,
   NumElemento = 1,
   Fila = [Primero|Resto],
   write('|'),
   write(Primero),
   write('|'),
   NumElementoAux is NumElemento+1,
   NumElementosAux is NumElemPorFila-1,
   mostrar_fila(Resto,NumElementoAux, NumElementosAux),!.
   
mostrar_fila(Fila, NumElemento, NumElemPorFila):- %Situación en la que no quedan elementos en la fila, no es el primer elemento de la fila y aún quedan huecos que completar del tablero hasta llegar a la quinta posición de la fila
   length(Fila, NumElementos),
   NumElementos = 0,
   NumElemento > 1,
   NumElemPorFila \= 0,
   write(' |'),
   NumElementoAux is NumElemento+1,
   NumElementosAux is NumElemPorFila-1,
   mostrar_fila(Fila, NumElementoAux, NumElementosAux),!.

mostrar_fila(Fila, NumElemento, NumElemPorFila):- %Situación en la que no quedan elementos en la fila, es el primer elemento de la fila y aún quedan huecos que completar del tablero hasta llegar a la quinta posición de la fila
   length(Fila, NumElementos),
   NumElementos = 0,
   NumElemento = 1,
   NumElemPorFila \= 0,
   write('| |'),
   NumElementoAux is NumElemento+1,
   NumElementosAux is NumElemPorFila-1,
   mostrar_fila(Fila, NumElementoAux, NumElementosAux),!.
   
mostrar_fila(Fila, _, NumElemPorFila):- %Situación en la que ni quedan elementos en la fila ni quedan huecos que completar del tablero hasta llegar a la quinta posición de la fila
   length(Fila, NumElementos),
   NumElementos = 0,
   NumElemPorFila = 0,
   write(''),!.

%Muestra una lista de elementos
mostrar_lista(Lista):- %Situación en la que quedan elementos en la lista y no es el primer elemento el que se estudia
   length(Lista, NumElementos),
   NumElementos \= 0,
   NumElementos \= 1,
   Lista = [Primero|Resto],
   write(Primero),
   write(' '),
   mostrar_lista(Resto).

mostrar_lista(Lista):- %Situación en la que quedan elementos en la lista y es el primer elemento el que se estudia
   length(Lista, NumElementos),
   NumElementos \= 0,
   NumElementos = 1,
   Lista = [Primero|Resto],
   write(Primero),
   mostrar_lista(Resto).

mostrar_lista(Lista):- %Situación en la que no quedan elementos en la lista
   length(Lista, NumElementos),
   NumElementos = 0,
   write('').

%Muestra la lista de factorias
mostrar_lista_factorias(NumFactoria, ListaFactorias):-
   length(ListaFactorias, NumFactorias),
   NumFactorias \= 0,
   ListaFactorias = [Primero|Resto],
   write('Factoria '),
   write(NumFactoria),
   write(':'),
   mostrar_lista(Primero),
   writeln('.'),
   SiguienteNumFactoria is NumFactoria+1,
   mostrar_lista_factorias(SiguienteNumFactoria, Resto).

mostrar_lista_factorias(_, ListaFactorias):-
   length(ListaFactorias, NumFactorias),
   NumFactorias = 0,
   writeln('').

%Muestra el centro de la mesa
mostrar_centro(Centro):-
   write('Centro: '),
   mostrar_lista(Centro),
   writeln('.').

%Muestra las distintas líneas de patrón de un jugador   
mostrar_lineas_patron(LineasPatron):-
   LineasPatron = [Primero|Resto],
   mostrar_lista(Primero),
   writeln(''),
   mostrar_lineas_patron(Resto).
   
%Muestra las distintas líneas de patrón de un jugador
mostrar_suelo(Suelo):-
   mostrar_lista(Suelo),
   writeln('.').
