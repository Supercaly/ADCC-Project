### APPLICAZIONI DISTRIBUITE E CLOUD COMPUTING A.A. 2021/2022

# Implementazione di uno spazio di tuple in Erlang

<p>
Realizzato da:</br>
<b>Lorenzo Calisti (307458)</br>
Luca Cinti (305772)</b>
</p>

## Quick start

Prima di clonare questa repository è importante specificare che l'intero applicativo è stato realizzato utilizzando la versione di **Erlang/OTP 19** pertanto non è garantito il suo funzionamento con versioni differenti.

Una volta importata la repository localmente è necessario compilare tutti i file .erl eseguendo i seguenti comandi nel terminale:

#### Linux/MacOS

```console
$ make compile
```

#### Windows

```console
> install.ps1 compile
```

Questo comando creerà la nuova cartella `ebin/` con al suo interno tutti i file .beam.

Una volta compilato il codice è possibile utilizzarlo avviando una o più istanze della shell Erlang:

```console
$ erl -pa ebin -sname name@host -secret SECRET
```

ed infine avviare l'applicazione su ogni nodo:

```console
(node@host)1> ts:start().
```
**Nota:** Prima di avviare l'applicazione sui nodi è consigliato connetterli fra loro utilizzando il comando `net_adm:ping(node@host).`. In caso contrario il nodo locale non sarà in grado di vedere i nodi aggiunti in seguito; per risolvere questo basterà riavviare l'applicazione sul singolo nodo.

## Specifiche del progetto e obiettivo

Prima di procedere alla presentazione dell’elaborato, è funzionale e necessario definire il concetto di *spazio di tuple*, e presentare brevemente cos’è *Erlang/OTP*.

Definiamo spazio di tuple un’implementazione del paradigma della memoria associativa per i sistemi distribuiti. Lo si può vedere come un insieme di array multi-dato, accessibile in maniera concorrente tramite pattern matching; nell’architettura distribuita i nodi distributori creano le tuple nello spazio, mentre i consumatori recuperano i dati che hanno la struttura della tupla che rispetta un pattern.

Con il termine Erlang/OTP vengono indicati tutti i concetti collegati allo sviluppo Erlang, a partire dal linguaggio di programmazione funzionale chiamato, per l’appunto, Erlang, alla sua macchina virtuale detta Beam, l’insieme di librerie e componenti scritti tutti in questo linguaggio, fino ai design principles che dettano come strutturare lo sviluppo di programmi Erlang.

Fatta questa panoramica possiamo passare a quello che è il focus dell’elaborato, ovvero implementare uno spazio di tuple (d’ora in avanti TS) che esponga le seguenti funzioni:

* `new(name)` → metodo per la creazione di un TS con nome *name*
* `in(TS, Pattern)` / `in(TS, Pattern, Timeout)` → metodo che ritorna e cancella una tupla dal TS  in caso di pattern matching verificato. L’overload del metodo con timeout evita che si verifichi una deadlock, ritornando un errore nel caso di mancato pattern matching
* `rd(TS, Pattern)` / `rd(TS, Pattern, Timeout)` → metodo analogo al precedente, ma che ritorna la tupla senza cancellarla dal TS
* `out(TS, Tuple)` → inserisce *Tuple* nel TS
* `addNode(TS, Node)` → aggiunge il nodo *Node* al TS, così che possa accedere a tutte le tuple
* `removeNode(TS, Node)` → rimuove il nodo *Node* dal TS
* `nodes(TS)` → ritorna i nodi dai quali il TS è visibile e replicato

Come si può ben vedere le funzioni *in* e *rd* hanno tra i parametri di input un **Pattern**, che è formato da una tupla composta da valori generici e/o dall’atomo *any*.
Any è la wildcard che rappresenta il matching con qualsiasi valore: prendiamo come esempio il pattern P = {any, 3, "Alice"} e le tuple A = {a, 3, "Alice"} e B = {b, 3, "Bob"}; vediamo che P fa matching con A, ma non con B.

## Dettagli implementativi

Il TS è stato implementato tramite un'applicazione Erlang (versione OTP19) chiamata **ts**, un componente che implementa una funzionalità specifica e può essere avviato e fermato come un blocco unico.
Al suo avvio viene lanciato il processo supervisor principale, chiamato *main_sup*, che a sua volta avvia i seguenti processi: logger, db_manager, ts_supervisor. Vediamoli più nel dettaglio:

**logger.erl** è la nostra implementazione di un logger adibito al *pretty-printing* dei messaggi; abbiamo deciso di creare una versione che rispettasse meglio le nostre necessità, utilizzando il behaviour *gen_server*. In presenza di un evento che richiede logging viene effettuata una chimata cast, con il messaggio come parametro, al processo logger che poi provvede a formattare e stampare sullo standard output.

**db_manager.erl** è il processo che si occupa della gestione del database, è dunque adibito alla sincronizzazione del db fra i diversi nodi del TS, gestendo l'aggiunta e rimozione dei nodi dallo stesso, e mantenendo una lista della loro distribuzione.
Strutturalmente db_manager non è altro che un wrapper attorno al database distribuito *mnesia*, permettendo inoltre all'intera applicazione di accedere in maniera facilitata alle seguenti funzioni:

* `create_new_space(spacename)` → crea un nuovo TS salvato sul database locale del nodo. L’operazione fallisce nel caso in cui *spacename* sia già utilizzato localmente o in remoto
* `add_node_to_space(Node, TS)` → aggiunge un altro nodo al TS di modo che anch’esso possa accedere a tutte le funzionalità condivise nello spazio. Tale obiettivo viene raggiunto sfruttando la funzionalità di mnesia *"add_table_copy"*, che copia localmente la tabella contenente le tuple
* `remove_node_from_space(Node, TS)` → rimuove un nodo dal TS, cancellando anche la copia locale della tabella delle tuple da esso, di nuovo sfruttando una funzione di mnesia: *"del_table_copy"*
* `list_nodes_in_space(TS)` → ritorna la lista di tutti i nodi che hanno accesso al TS. Questa operazione fallisce nel caso in cui il nodo che richiede l'informazione non sia a sua volta all'interno dello spazio

db_manager è implementato utilizzando nuovamente il behaviour gen_server, come nel caso di logger.erl, dove tutte le funzioni viste in precedenza effettuano delle *call* sul gen_server locale (o su quello del nodo remoto nel caso di add_node_to_space/remove_node_from_space) passando i parametri necessari.

**ts_supervisor.erl** implementa un supervisor dal comportamento molto semplice, il cui scopo è quello di gestire un pool di processi di tipo *ts_manager* avviando e fermandone l’esecuzione. Internamente utilizza una strategia di supervisione chiamata *simple_one_for_one*, nella quale tutti i figli sono istanze, aggiunte dinamicamente, dello stesso processo.
La creazione o la rimozione di un processo ts_manager, viene decisa dal db_manager in fase di creazione di un nuovo TS, oppure all’aggiunta/rimozione di un nodo.

**ts_manager.erl** è un ulteriore componente fondamentale dell'applicazione, si occupa della gestione delle letture e scritture all'interno di un TS; ogni spazio di tuple a cui ha accesso il nodo locale, ha un corrispettivo processo ts_manager in esecuzione, registrato con lo stesso nome.
Anche in questo caso è stato implementato un gen_server, e vengono esposte all'applicazione le seguenti funzioni:

* `perform_out(TS, Tuple)` → esegue internamente una call al gen_server (passando come parametro *{write_tuple, Tuple}*), che è poi responsabile dell’inserimento di una nuova entry nella tabella corretta
* `perform_rd(TS, Pattern, Timeout)` e `perform_in(TS, Pattern, Timeout)` le quali eseguono entrambe una call al gen_server passando il parametro *{read_tuple, Pattern}* e ricevendo come risposta *{ok, Tuple}* in caso di matching corretto oppure *{error, no_tuples}* in caso contrario

Queste due ultime funzioni eseguono a loro volta la funzione interna *subscribe_for_pattern(Space, Pattern, Timeout)* che mette in attesa il chiamante, usando il construtto receive, di un messaggio da parte di mnesia in cui si indica l'inserimento di una nuova tupla nel TS. Se la nuova tupla rispetta il pattern, il chiamante viene sbloccato, altrimenti rimane in attesa ricorsivamente fino allo scadere del timeout.
Come da specifiche, la differenza tra le due funzioni sta nel fatto che, contrariamente alla *perform_rd*, la *perform_in* cancella la tupla dal TS una volta effettuata la lettura.

## Scelte progettuali

Di seguito parleremo delle decisioni implementative effettuate da noi nella fase di progettazione e sviluppo, di questo applicativo.

Abbiamo scelto di realizzare un’applicazione Erlang per racchiudere nel modo più pulito e conciso tutti i moduli necessari al suo funzionamento; questo ha reso possibile ad ogni nodo l’avvio dell'intero sistema attraverso la singola chiamata di `ts:start()`, e conseguentemente l’arresto chiamando la funzione `ts:stop()`.

Come già detto in precedenza, main_supervisor è il processo padre di tutta l'applicazione. Si è scelto di utilizzare un supervisor a questo livello per poter sfruttare appieno il pattern OTP e garantire la costante esecuzione dei processi figli in ogni momento, anche in caso di crash; il supervisor, infatti, essendo di tipo *one_for_one*, monitora ogni processo e lo riavvia autonomamente in caso di terminazione improvvisa. Anche ts_supervisor è stato creato con il medesimo scopo, ovvero garantire la costante esecuzione dei processi figli; in questo caso però, avendo a che fare con un numero di processi dinamico, abbiamo scelto di usare il *simple_one_for_one* supervisor.

Nella progettazione e creazione del database distribuito, si è scelto di utilizzare mnesia, adattandolo alle nostre esigenze. 
Il suo funzionamento è il seguente: ogni nodo in fase di avvio stabilisce una connessione ad un cluster mnesia con tutti i nodi a sua conoscenza. Va detto che purtroppo, a causa delle limitazioni di mnesia, ogni nodo può essere inserito in un solo cluster per volta; ne consegue che se la topologia della rete è densa si viene a creare un unico grande cluster.

I nodi possiedono tutti una copia della tabella nodes, che è di tipo bag (ogni chiave può avere più valori collegati) e associa ad ogni nome di TS una lista con tutti i nodi che sono al suo interno. 
Abbiamo deciso di distribuire la tabella nodes su ogni nodo per garantirne la persistenza anche in caso di fallimento di alcuni di essi.
Questa scelta di progetto, benché comporti un mantenimento del sistema sicuramente oneroso, in termini di performance globali, è stata intrapresa perché garantisce sicurezza e stabilità.

In fase di creazione di un nuovo TS da parte di un nodo, nel database viene creata una nuova tabella con lo stesso nome, una nuova entry è inserita all'interno della tabella nodes, e viene lanciato il relativo processo locale di ts_manager.
Le tabelle dei TS sono, anche in questo caso, di tipo bag, dove le chiavi sono la lunghezza della tupla e i valori sono tutte le tuple inserite di quella data lunghezza.
Se un nodo è aggiunto ad uno spazio già esistente, il suo nome è inserito alla tabella nodes e una copia della relativa tabella con le tuple è copiata nel suo ambiente locale. Analogamente in fase di rimozione, la copia della tabella è cancellata dall’host e il suo nome è ovviamente rimosso dalla tabella nodes. 
Queste scelte sono state fatte tenendo sempre a mente la persistenza e ridondanza dei dati in caso di fallimento di nodi “nevralgici”.

## Metriche

In questa sezione analizzeremo alcune statistiche ricavate eseguendo l'applicazione in un cluster composto da diversi nodi.

Per facilitare il monitoraggio di queste tempistiche ci siamo serviti di un *escript*, ovvero programma erlang che può essere lanciato come un comune script; per eseguirlo basta lanciare il seguente comando da terminale:

```console
$ escript avg_times.escript
```

L'esecuzione dello script impiega circa 15 minuti e al termine stampa a video i seguenti risultati:

| Operazione | Tempo medio |
| - | - |
| in | 5.79 ms |
| rd | 6.77 ms |
| out | 0.22 ms |
| node recover | 673.04 ms |

Come possiamo ben vedere le operazioni di lettura e scrittura impiegano un tempo trascurabile.
Anche il tempo di recover dei nodi, sotto la soglia del secondo, non è preoccupante, sopratutto se si considera che la stessa applicazione implementata in un linguaggio di programmazione "tradizionale" avrebbe richiesto l'intervento di un tecnico e additittura causato il crash dell'intero sistema.

## Conclusioni

vediamo dopo le metriche 😉