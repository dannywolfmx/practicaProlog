%Daniel Menchaca Luna


%alfabeto(Letras), generadorPalabras(Letras,10,15,Palabras,1000),listaArbol(Palabras,Arbol),aplanaArbol(Arbol,Re),write(Re).


alfabeto(['a','b','c','d','e','f','g','h','i','j','k','l','m','n','ñ','o','p','q','r','s','t','u','v','w','x','y','z']).
%alfabeto([a,e,i,o,u]). % alfabeto mas pequeño para pruebas

% Le indicamos el caso en que la lista es Vacia, dado que si no lo colocamos no se cicla. 
% el 0 tecnicamente lo comprobamos en Tam > 0, pero no podemos usar una variable anonima, 
% dado que entraria en cualquier caso.


%alfabeto(Letras), generadorPalabras(Letras,5,15,Palabras,10).
%alfabeto(Letras), generadorPalabras(Letras,5,15,Palabras,1000),listaArbol(Palabras,Arbol),write(Re).


menu():-%Generamos las palabras aleatorias.
        writeln("-----Generadorando las palabras aleatorias-----"),
        alfabeto(Letras), generadorPalabras(Letras,5,15,Palabras,1000),
        %Generamos el arbol.
        writeln("-----Generando arbol-----"),
        listaArbol(Palabras,Arbol),
        %imprime arbol.
        writeln("-----Imprime arbol-----"),
        write(Arbol),
        write("\n"),
        writeln("-----Arbol-----"),
        read(X),
        write(X).


imprimePalabras([]):-

            write("\n"),
            write("-----Fin de la lista----"),
            write("\n").

imprimePalabras([X|Y]):- 
            write("\n"),
            imprimeCaracter(X),
            write("\n"),
            imprimePalabras(Y).

imprimeCaracter([]). 
imprimeCaracter([X|Y]):- 
            write(X),
            imprimeCaracter(Y).


ordenaListaPalabras(Lista,Salida):-
            sort(Lista,Salida).
            
%Nodo es definido como nodo(NodoHijoIzquierdo,Palabra,Indice,NodoHijoDerecho)
%Palabra, es la palabra como tal.
%Indice, es utilizado para la comparacion y balancear.
%NodoHijoIzquierdo es el nodo hijo del lado izquierdo
%NodoHijoDerecho es el nodo hijo del lado derecho.



insertaArbol(nulo, X, nodo(nulo, X, nulo)).

insertaArbol(Arbol, X, Arbol):-Arbol =nodo(_,X,_).

insertaArbol(nodo(Izquierda,Indice, Derecha), X,  nodo(IzquierdaNueva,Indice, Derecha)) :-
         X @< Indice, 
         insertaArbol(Izquierda, X, IzquierdaNueva).

insertaArbol(nodo(Izquierda,Indice, Derecha), X , nodo(Izquierda,Indice, DerechaNueva)) :-
        X @> Indice, 
        insertaArbol(Derecha, X, DerechaNueva).
		

listaArbol([],_). %hijo

listaArbol([Palabra|Y],Arbol):-
                listaArbol(Y,NuevaRama),
                insertaArbol(NuevaRama,Palabra,Arbol).


aplanaArbol(nulo,[]).
aplanaArbol(nodo(NodoHijoIzquierdo,_,NodoHijoDerecho),ListaDePalabras):-
		write(Palabra),
		aplanaArbol(NodoHijoIzquierdo,PalabrasIzquierda),
                N = [Palabra|PalabrasIzquierda],
		aplanaArbol(NodoHijoDerecho,PalabrasDerecha),
		ListaDePalabras = [N|PalabrasDerecha].
	


generadorPalabras(_,_,_,[],0).% Caso en el cual la lista y el tamaño es 0, por lo cual ya termino su cometido.
generadorPalabras(Alfabeto,TamMin,TamMax,Salida,Iterador):-
            Iterador > 0,% Si TAM es menor que 0, significa que ya acabo.
            random_between(TamMin,TamMax,TamPalabra),
            generaPalabra(Alfabeto,TamPalabra,PalabraGenerada),
            %Colocar aqui el member, (Ya que la lista este en uso).
            %\+ member(PalabraGenerada,Lista)
            NuevoIterador is Iterador - 1,
            generadorPalabras(Alfabeto,TamMin,TamMax,Lista,NuevoIterador),
            compara(PalabraGenerada,Lista,Salida,Alfabeto,TamMin,TamMax,NuevoIterador).
                        

compara(PalabraGenerada,Lista,Salida,_,_,_,_):- 
        \+ member(PalabraGenerada,Lista),
        Salida = [PalabraGenerada|Lista].

compara(_,_,Salida,Alfabeto,TamMin,TamMax,Iterador):-
            NuevoIterador is Iterador + 1,
            generadorPalabras(Alfabeto,TamMin,TamMax,Lista,NuevoIterador),
            Salida = Lista.



%alfabeto(Letras), generaPalabra(Letras,5,Palabra).
generaPalabra(_,0,[]). % Caso en el cual la lista y el tamaño es 0, por lo cual ya termino su cometido.
generaPalabra(Alfabeto,Tam,Salida):- 
            Tam > 0, % Si TAM es menor que 0, significa que ya acabo.
            NuevoTam is Tam-1, % Hacemos que el contador disminuya.
            random_member(Letra,Alfabeto),% Me va a otorgar una elemento en este caso una letra al Azar.
            Salida = [Letra|Lista],
            generaPalabra(Alfabeto,NuevoTam,Lista). % Volvemos a llamar a la funcion con el nuevo valor, recuerda que puedes abortar algo ciclado con 'a' de abortar.
