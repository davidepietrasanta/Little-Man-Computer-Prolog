# Little-Man-Computer-Prolog-
A "Little man computer" in Prolog. This implementation was written by Davide Pietrasanta and Veronica Orsanigo. 

You can find the specifics here 

Il progetto consiste nella creazione di un LMC (little man computer), che e' formato da:
- Memoria di 100 celle numerate da 0 a 99 contenenti numeri fra 0 e 999, senza distinzione fra numeri e istruzioni
- Registro accumulatore (0 all'inizio)
- Program counter: contiene l'indirizzo dell'istruzione da eseguire (0 all'inizio), se raggiunge 99 il valore successivo e' 0
- Coda input: contiene valori dati in input all'LMC, compresi fra 0 e 999
- Coda output: all'inizio e' vuota, poi contiene i valori di output dell'LMC, compresi fra 0 e 999
- Flag: bit che vale all'inizio 0 e 1 se l'ultima operazione aritmatica da' risultato maggiore di 999 o minore di 0
Al LMC si aggiunge anche la possibilita' di lettura ed esecuzione di file assembly.

Per il progetto in Prolog ci sentiamo di dover illustrare tali specifiche:
-Un file completamente vuoto viene considerato corretto e generera' una memoria di cento 0.
-Se in un file ci sono righe vuote esse non vengono considerate per il calcolo del valore delle labels.
-Se in un file ci sono righe contenenti solo spazi esse non vengono considerate per il calcolo del valore delle labels.
-Se in un file ci sono righe contenenti solo commenti esse non vengono considerate per il calcolo del valore delle labels.
-Se in una riga viene usata una label non presente nel resto del file il programma terminera' con false. 
-Se invece una label e' presente ma non viene usata (dunque e' prima di un'istruzione) il programma continua tranquillamente.
-Se in una riga e' presente solo una label, in assenza di istruzioni, il programma terminera' con false.
-In caso il file sia NON ben formato allora la lmc-load terminera' con false e cosi' anche la lmc-run.
-Sia le labels che le istruzioni sono case insensitive.
-Le labels accettate sono solo alfanumeriche, iniziano con una lettera e sono diverse da istruzioni
 (ES. A1b2, a1bcd2, ciao, ciao123, a123, aBcD)

Il file assembly passato e' messo in una lista di liste, ognuna delle quali corrisponde a una riga
Quindi sono eliminati commenti e righe vuote e vengono valutate le etichette e sostituite

Qui di seguito i predicati principali usati nella lmc_load\2, che riassumono i passaggi per creare la memoria a partire dalla lettura del file assembly

File assembly di esempio (file.txt):
add label
sub 1
label sub 2
sub 3

-? file_into_list_of_lists('file.txt', FileList).
FileList = [[[a, d, d], [l, a, b, e, l]], [[s, u, b], 1], [[l, a, b, e, l], [s, u, b], 2], [[s, u, b], 3]].

?- loop_label_to([[[a, d, d], [l, a, b, e, l]], [[s, u, b], 1], [[l, a, b, e, l], [s, u, b], 2], [[s, u, b], 3]], ListIstr, ListLabel).
ListIstr = [[[a, d, d], [l, a, b, e, l]], [[s, u, b], 1], [[s, u, b], 2], [[s, u, b], 3]], // lista istruzioni senza etichette a inizio riga
ListLabel = [0, 0, [l, a, b, e, l], 0]. // zeri nelle posizioni delle righe in cui non ci sono etichette a inizio riga

?- find_all_label([0, 0, [l, a, b, e, l], 0], 0, [[[a, d, d], [l, a, b, e, l]], [[s, u, b], 1], [[s, u, b], 2], [[s, u, b], 3]], ListIstr1).
ListIstr1 = [[[a, d, d], 2], [[s, u, b], 1], [[s, u, b], 2], [[s, u, b], 3]]. // etichette risolte

?- loop_list_of_type_flatten([[[a, d, d], 2], [[s, u, b], 1], [[s, u, b], 2], [[s, u, b], 3]], ListIstr2).
ListIstr2 = [[a, d, d, 2], [s, u, b, 1], [s, u, b, 2], [s, u, b, 3]]. // flatten delle liste istruzioni per passarle al parsing

?- loop_frase_value([[a, d, d, 2], [s, u, b, 1], [s, u, b, 2], [s, u, b, 3]], Mem1).
Mem1 = [102, 201, 202, 203].

?- crea_memoria_completa(4, [102, 201, 202, 203], Mem). // si passa il numero di el di Mem1 e si aggiungono zeri per completare la memoria
Mem = [102, 201, 202, 203, 0, 0, 0, 0, 0|...]. // mem di 100 el

Infine lmc_load, se non fallisce a causa della forma scorretta del file assembly, e' richiamata da lmc_run\3 per creare la memoria ed eseguire
le istruzioni richiamando execution_loop\2

In seguito esempi di richiamo a funzioni presenti nel progetto:

- one_instruction(state(0, 
		        0, 
		       [901, 103, 902, 10, 0, 0, 0, 0, 0, 0, 
			0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
			0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
			0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
			0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
			0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
			0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
			0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 	
			0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 0, 0, 0, 0, 0, 0, 0, 0], 
			[10], 
			[], 
			noflag), 
		NewState).

- execution_loop(state(0, 
		       0, 
		       [901, 103, 902, 10, 0, 0, 0, 0, 0, 0, 
			0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
			0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
			0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
			0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
			0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
			0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
			0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 	
			0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 0, 0, 0, 0, 0, 0, 0, 0], 
		       [10], 
	               [], 
		       noflag), 
		Out1).

- lmc_load('nome_file.txt', Mem).

- lmc_run('nome_file.txt', [901, 902, 705, 600, 0, 4, 5, 6, 7, 8, 9, 0], Output).


