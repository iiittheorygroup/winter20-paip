# winter20-paip

Materials used and code written for the winter 2020 summer school conducted on
the book *Paradigms of Artificial Intelligence Programming* by Peter Norvig.

## eliza

ELIZA is an early natural language processing computer program that uses pattern
matching and substitution to give an illusion of understanding. Here is a run of
eliza (on [latest commit](https://github.com/iiittheorygroup/winter20-paip/tree/459f7427cf4aab85d945bd1e8c8c9256d8816e72))

```
> (eliza '(hello eliza))
'(How do you do. Please state your problem)
> (eliza '(i want to go back to college))
'(Suppose you got to go back to college soon)
> (eliza '(i can go to jc if i get back to college))
'(Really-- if you get back to college)
> (eliza '(no not really))
'(Why not?)
> (eliza '(i was being sarcastic))
'(Why do you tell me you were being sarcastic now?)
> (eliza '(i feel bored))
'(Do you often feel bored ?)
> (eliza '(i felt bored yesterday))
'(What other feelings do you have?)
```

See [test cases](./eliza/tests.rkt) to get a feel of how various components of
the program work. To run all tests, do

```
raco test tests.rkt
```
