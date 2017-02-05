%alfabeto([a,b,c,d,e,f,g,h,i,j,k,l,m,n,ñ,o,p,q,r,s,t,u,v,w,x,y,z]).
alfabeto([a,e,i,o,u]). % alfabeto mas pequeño para pruebas



iterador([Head|Tail]) :- write("Head "),
                    write(Head),    % Cabeza
                    write('\n'),
                    iterador(Tail). % Resto

% Le indicamos el caso en que la lista es Vacia, dado que si no lo colocamos no se cicla. 
% el 0 tecnicamente lo comprobamos en Tam > 0, pero no podemos usar una variable anonima, 
% dado que entraria en cualquier caso.


%alfabeto(Letras), generadorPalabras(Letras,5,15,Palabras,10).
generadorPalabras(_,_,_,[],0).% Caso en el cual la lista y el tamaño es 0, por lo cual ya termino su cometido.
generadorPalabras(Alfabeto,TamMin,TamMax,Salida,Iterador):-
            Iterador > 0,% Si TAM es menor que 0, significa que ya acabo.
            random_between(TamMin,TamMax,TamPalabra),
            generaPalabra(Alfabeto,TamPalabra,PalabraGenerada),
            write(Lista),
            %Colocar aqui el member, (Ya que la lista este en uso).
            %\+ member(PalabraGenerada,Lista)
            NuevoIterador is Iterador-1,
            generadorPalabras(Alfabeto,TamMin,TamMax,Lista,NuevoIterador),
            compara(PalabraGenerada,Lista).
            Salida = [PalabraGenerada|Lista].


%alfabeto(Letras), generaPalabra(Letras,5,Palabra).
generaPalabra(_,0,[]). % Caso en el cual la lista y el tamaño es 0, por lo cual ya termino su cometido.
generaPalabra(Alfabeto,Tam,Salida):- 
            Tam > 0, % Si TAM es menor que 0, significa que ya acabo.
            NuevoTam is Tam-1, % Hacemos que el contador disminuya.
            random_member(Letra,Alfabeto),% Me va a otorgar una elemento en este caso una letra al Azar.
            Salida = [Letra|Lista],
            generaPalabra(Alfabeto,NuevoTam,Lista). % Volvemos a llamar a la funcion con el nuevo valor, recuerda que puedes abortar algo ciclado con 'a' de abortar.
