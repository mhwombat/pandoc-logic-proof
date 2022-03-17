# pandoc-logic-proof
Logic proofs for pandoc.


```
  ```{.logicproof}
  10 | | $p \land \lnot p$ | Assumption
  20 | | $p$ | And Elimination (@10)
  30 | $p \land \lnot p \implies p$ |Implication Introduction (@10), (@20)
  40 | | $p \land \lnot p$ | Assumption
  50 | | $\lnot p$ | And Elimination (@40)
  60 | $p \land \lnot p \implies \lnot p$ |Implication Introduction (@40), (@50)
  70 | $\lnot (p \land \lnot p)$ |Negation Introduction (@30), (@60)
  ```
```

pandoc --filter=pandoc-logic-proof README.md --output=temp.pdf
