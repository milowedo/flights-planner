A simple app concerning management of flights in the airport's air traffic controll.

//polish desc

  Program składa się z trzech elementów: serwera, samolotu i wrappera. 
  Po uruchomieniu programu, użytkownik dodaje kolejne loty. Są one uruchamiane jakie osobne procesy, komunikujące się z serwerem, serwer dodaje je sobie do listy w celu śledzenia wszystkich lotów w czasie bieżącym. 
  Po odpowiedniej ilości czasu proces wysyła wiadomość o wystartowaniu samolotu, a następnie o jego wylądawaniu. Wtedy proces zwraca serwerowi, że skończył swoje zadanie, unicestwia się, a serwer usuwa go ze swojej listy lotów. 
  Możliwe jest również wycofanie danego lotu, co skutkuje przesłaniem do procesu wiadomości o jego anulowaniu, jego anulowanie
i powiadomienie o tym serwera, który usuwa dany lot ze swojej listy lotów, tj jednocześnie przestaje mieć do niego w ogóle dostęp 
i jakiekolwiek o nim informacje.


1.Skompiluj plane, myserver i wrapper –> c(program). c(myserver). c(wrapper).

2.Uruchom program –> wrapper:start().

3.Wybierz opcję działania podaną przez interfejs (dodanie lotu, anulowanie lotu, podgląd bieżących lotów)

4.Powtórz poprzedni krok lub wybierz opcję zakończenia działania programu.

