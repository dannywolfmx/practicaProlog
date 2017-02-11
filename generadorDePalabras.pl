%Daniel Menchaca Luna


%alfabeto(Letras), generadorPalabras(Letras,10,15,Palabras,1000),ordenaListaPalabras(Palabras,Z),imprimePalabras(Palabras),listaArbol(Palabras,Arbol,0),write(Arbol).


alfabeto([a,b,c,d,e,f,g,h,i,j,k,l,m,n,ñ,o,p,q,r,s,t,u,v,w,x,y,z]).
%alfabeto([a,e,i,o,u]). % alfabeto mas pequeño para pruebas

% Le indicamos el caso en que la lista es Vacia, dado que si no lo colocamos no se cicla. 
% el 0 tecnicamente lo comprobamos en Tam > 0, pero no podemos usar una variable anonima, 
% dado que entraria en cualquier caso.


%alfabeto(Letras), generadorPalabras(Letras,5,15,Palabras,10).




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





insertaArbol(P,nulo, X, nodo(P,X, nulo, nulo)).

insertaArbol(P,Arbol, X, Arbol) :- 
        Arbol = nodo(P,X, _, _).

insertaArbol(P,nodo(R,Indice, Izquierda, Derecha), X,  nodo(R,Indice, IzquierdaNueva, Derecha)) :-
         X < Indice, 
         insertaArbol(P,Izquierda, X, IzquierdaNueva).

insertaArbol(P,nodo(R,Indice, Izquierda, Derecha), X , nodo(R,Indice, Izquierda, DerechaNueva)) :-
        X > Indice, 
        insertaArbol(P,Derecha, X, DerechaNueva).


listaArbol([],Arbol,_):-
	Arbol = nodo(0,0,nulo,nulo). %hijo

listaArbol([Palabra|Y],Arbol,Iterador):-
                NuevoIterador is Iterador + 1, 
                listaArbol(Y,NuevaRama,NuevoIterador),
                insertaArbol(Palabra,NuevaRama,NuevoIterador,H),
                Arbol = H.


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
