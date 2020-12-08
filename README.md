# Decision Trees a Haskell

Pràctica de l'assignatura de LP de la tardor de 2020. Construcció i classificació del conjunt de dades [mushroom](https://archive.ics.uci.edu/ml/datasets/Mushroom). 

### Implementació

La generació dels decision trees, segueix l'algorisme [ID3](https://en.wikipedia.org/wiki/ID3_algorithm), en el que el paràmetre de separació és l'infomation gain generat a partir de les entropies del conjunt d'elements que s'està tractant. Un cop generat el decision tree del conjunt donat, el sistema fa preguntes a l'usuari per a tal de classificar un possible nou element.

### Instalació i execució

Per a l'instalació del programa cal tenir Haskell instal·lat a l'equip. Compilar usant la següent comanda.

```
ghc dts.hs
```

Un cop compilat, executar el fitxer executable generat.

### Ús

Un cop generat l'arbre de decisió del conjunt, el sistema pregunta les caràcteristiques dels diferents atributs, i l'usuari ha de respondre amb els valors adequats. Com a exemple:

```
>>System: Which odor?
>>User: none
>>System: Which spore-print-color?
>>User: black
>>System: Prediction: edible
```

Si l'usuari tria un valor que no existeix a l'arbre de decisió, el programa acaba. D'altra manera, el progrmaa no acaba fins que es fa una predicció.