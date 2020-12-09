# Symbolic `meth`


Simplification : 
    Vague concept, depends on use-case scenario.
    Ex:- Is $x^2 + 3x + 2$ simplified or $(x+1)(x+2)$ ? Depends whether we want to integrate or find roots.

## Infix -> Prefix

- Write each exp. in infix form but store them in prefix form.

Properties of infix->prefix and `______`:
    1. Both leave atoms unchanged
    2. ""   transform the 3-element list by swapping `exp-op` and `exp-lhs`.

- We use `rule-based translator` and `pat-match` to implement this.


