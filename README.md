# winter20-paip

Materials used and code written for the winter 2020 summer school conducted on
the book *Paradigms of Artificial Intelligence Programming* by Peter Norvig.

## GPS

[GPS](https://en.wikipedia.org/wiki/General_Problem_Solver) is a computer program intended to work as a universal problem solver machine (exaggerated) . It's works with `means-end-analysis` (**We deliberate not about ends, but about means**). Refer to Chapter 4 in the book provided for more info.

 Here's a run of GPS for the school problem:

```bash
python gps.py school.json
> Do you want to output a stack trace?[y/n] n
Executing look up number
Executing telephone shop
Executing tell shop problem
Executing give shop money
Executing shop installs battery
Executing drive son to school
```

### How-To use the [Template](GPS/template.json):

1. Refer to [this file](GPS/gps.rkt) for understanding how to formulate a problem (if possible) for the GPS, and checkout [this example](GPS/monkaay.json) for specifics.

Here's a link to the original [GPS paper](http://bitsavers.informatik.uni-stuttgart.de/pdf/rand/ipl/P-1584_Report_On_A_General_Problem-Solving_Program_Feb59.pdf).

## eliza

[ELIZA](https://en.wikipedia.org/wiki/ELIZA) is an early natural language
processing computer program that uses pattern matching and substitution to give
an illusion of understanding. Here is a run of eliza (on [latest commit](https://github.com/iiittheorygroup/winter20-paip/tree/459f7427cf4aab85d945bd1e8c8c9256d8816e72))

```lisp
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

Here's a link to the original [Eliza paper](https://web.stanford.edu/class/linguist238/p36-weizenabaum.pdf).

See [test cases](./eliza/tests.rkt) to get a feel of how various components of
the program work. To run all tests, do

```
raco test tests.rkt
```
