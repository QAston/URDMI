%background knowledge and aleph settings 
% file appended to the generated .b file

:-set(interactive,true).
:-set(test_pos,'pracownik_test.f').
:-set(test_neg,'pracownik_test.n').


%DEFINICJA SKADOWYCH REGU

:-modeh(1,pracownik(+idpracownika,+stanowisko,+pensja,+premia,+rokzatrudnienia,+iddzialu)).

:-modeb(1,pracownikpersonalia(+idpracownika,-imie,-nazwisko,-nip,-ulica,-nrdomu,-nrlokalu,-miejscowosc)).
:-modeb(1,dzial(+iddzialu,-nazwadzialu,-ulica,-numerdomu,-numerlokalu,-miejscowosc)).
:-modeb(1,pracownikprodukcja(-idtowaru,+idpracownika,-poczatek,-koniec,-stawka,-liczbagodzin)).
:-modeb(1,towar(+idtowaru,-nazwatowaru,-model,-ilosc, -unk,-cena)).
:-modeb(1,produkcja(+idtowaru,-opis,-poczatek,-koniec,-budzet)).
:-modeb(1,zamowienie(-idzamowienia,-idklienta,+idpracownika,-data,-platnosc)).
:-modeb(1,zamowienieszczegoly(+idzamowienia,-idtowaru,-ilosc,-upust)).
:-modeb(1,klient(+idklienta,-imie,-nazwisko,-nip,-ulica,-nrdomu,-nrlokalu,-kodpocztowy,-miejscowosc)).

:-modeb(1,(+stanowisko) = (#stanowisko)).
:-modeb(1,(+miejscowosc) = (#miejscowosc)).
:-modeb(1,(+model) = (#model)).
:-modeb(1,(+stawka) > (#stawka)).
:-modeb(1,(+ilosc) > (#ilosc)).
:-modeb(1,(+upust) > (#upust)).
:-modeb(1,(+platnosc) = (#platnosc)).
:-modeb(1,(+cena) > (#cena)).
:-modeb(1,(+stawka) = (#stawka)).
:-modeb(1,(+pensja) = (#pensja)).
:-modeb(1,(+premia) = (#premia)).
:-modeb(1,(+rokzatrudnienia) > (#rokzatrudnienia)).
:-modeb(1,(+rokzatrudnienia) < (#rokzatrudnienia)).

%DEKLARCJA UYCIA RELACJI DO KONSTRUKCJI REGU

:-determination(pracownik/6,dzial/6).
:-determination(pracownik/6,pracownikpersonalia/8).
:-determination(pracownik/6,pracownikprodukcja/6).
:-determination(pracownik/6,produkcja/5).
:-determination(pracownik/6,zamowienie/5).
:-determination(pracownik/6,klient/9).
:-determination(pracownik/6,zamowienieszczegoly/4).
:-determination(pracownik/6,towar/6).
:-determination(pracownik/6,klient/9).

:-determination(pracownik/6,'='/2).
:-determination(pracownik/6,'>='/2).
