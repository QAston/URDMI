% ace engine settings
% file appended to the generated .s file

load(key).
tilde_mode(classify).
talking(4).
predict(pracownik(+idpracownika,+stanowisko,+pensja,+premia,+rokzatrudnienia,+iddzialu,-grupa)).

typed_language(yes).


type(pracownikpersonalia(idpracownika,imie,nazwisko,nip,ulica,nrdomu,nrlokalu,miejscowosc)).
type(dzial(iddzialu,nazwa,ulica,numerdomu,numerlokalu,miejscowosc)).
type(pracownikprodukcja(idtowaru,idpracownika,poczatek,koniec,stawka,liczbagodzin)).
type(towar(idtowaru,nazwa,model,rokprodukcji,ilosc,cena)).
type(produkcja(idtowaru,opis,poczatek,koniec,budzet)).
type(zamowienie(idzamowienia,idklienta,idpracownika,data,platnosc)).
type(zamowienieszczegoly(idzamowienia,idtowaru,ilosc,upust)).
type(klient(idklienta,imie,nazwisko,nip,ulica,nrdomu,nrlokalu,kod,miejscowosc)).

type(X=X).

rmode(pracownikpersonalia(+A,-B,-C,-D,-E,-F,-G,+-H)).

rmode(dzial(+A,#,-C,-D,-E,+-F)).
rmode(towar(+A,-B,-C,-D,-E,-F)).
rmode(towar(-A,+B,-C,-D,-E,-F)).
rmode(produkcja(+A,-B,-C,-D,-E)).
rmode(zamowienie(+A,-B,-C,-D,-E)).
rmode(zamowienie(-A,+B,-C,-D,-E)).
rmode(zamowienieszczegoly(+A,-B,-C,-D)).
rmode(klient(+A,-B,-C,-D,-E,-F,-G,-H, +-I)).

rmode(#(inf*inf*B: pracownik(A,B,C,D,E,F,G),+X=B)).
rmode(#(inf*inf*C: pracownik(A,B,C,D,E,F,G),+X=C)).
rmode(#(inf*inf*E: pracownik(A,B,C,D,E,F,G),+X=E)).
