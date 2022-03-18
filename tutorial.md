---
title: Tutorial pandoc-logic-proof
...

# Usage

Use this filter by adding `--filter=pandoc-logic-proof` to your pandoc command.
For example, this tutorial was generated using the command below.

    pandoc --filter=pandoc-logic-proof tutorial.md --output=tutorial.pdf

# Basic structure

This example of a very short proof, the idempotency law $p \lor p \equiv p$,
will demonstrate how to format a proof.

    ```{.logicproof}
    1 | $p$         |Premise
      | $p \land p$ |And Introduction (@1), (@1)
    ```

In PDF output, this would be displayed as shown below.

```{.logicproof}
1 | $p$         |Premise
  | $p \land p$ |And Introduction (@1), (@1)
```

The proof is contained within a verbatim (code) block.
Within that block, the "columns" of the proof are separated by vertical bars (`|`).
It is not necessary to align the columns, the filter align the proof
properly without it.
Of course, column alignment makes it easier to read the raw markdown.

The first column contains label for statements that you want to refer back to.
Labels do not need to be numbers; they can be arbitrary alphanumeric
strings.
Note that these labels are unrelated to the automatically generated
statement numbers that appear in the output.

The middle column contains the statements of the proof.
Typically these are Latex equations, but they can be arbitrary markdown.

The final column contains the justification for each statement.
You can reference earlier statements using the syntax `(@`*label*`)`.

In all but the first column, you can include arbitrary markdown.
For example, in the justification column I like to include a hypertext
link to an explanation of the rule being used.
For example,


    ```{.logicproof}
    1 | $[a] = [b]$ | Premise
    2 | $a \in [a]$ | [](#equivalence-class)
    3 | $a \in b$   | 1, 2
    ```

This will result in a hyperlink (if your output format supports it).
For example, if you hover over the words "equivalence class" in the
proof below, you will find that it is a link to a Wikipedia article.

```{.logicproof}
1 | $[a] = [b]$ | Premise
2 | $a \in [a]$ | [equivalence class](https://en.wikipedia.org/wiki/Equivalence_class)
3 | $a \in b$   | 1, 2
```

# Subproofs

Subproofs within your proof can be indicated using extra columns.
This is easier to show than to explain.

    ```{.logicproof}
    10  | $p \implies q$        |Premise
    20  | $r \implies s$        |Premise
    30  | $p \lor r$            |Premise
    40  | | $p$                 | Assumption
    50  | | $q$                 | Implication Elimination (@10), (@40)
    60  | | $q \lor s$          | Or Introduction (@50)
    70  | $p \implies q \lor s$ |Implication Introduction (@40), (@60)
    80  | | $r$                 | Assumption
    90  | | $s$                 | Implication Elimination (@20), (@80)
    100 | | $q \lor s$          | Or Introduction (@90)
    110 | $r \implies q \lor s$ | Implication Introduction (@80), (@100)
    110 | $q \lor s$            | Or Elimination (@30), (@70), (@110)
    ```

In the resulting output, the subproofs are indented.

```{.logicproof}
10  | $p \implies q$        |Premise
20  | $r \implies s$        |Premise
30  | $p \lor r$            |Premise
40  | | $p$                 | Assumption
50  | | $q$                 | Implication Elimination (@10), (@40)
60  | | $q \lor s$          | Or Introduction (@50)
70  | $p \implies q \lor s$ |Implication Introduction (@40), (@60)
80  | | $r$                 | Assumption
90  | | $s$                 | Implication Elimination (@20), (@80)
100 | | $q \lor s$          | Or Introduction (@90)
110 | $r \implies q \lor s$ | Implication Introduction (@80), (@100)
110 | $q \lor s$            | Or Elimination (@30), (@70), (@110)
```

Of course, you can have sub-subproofs, etc.

    ```{.logicproof}
     10 | | $A \subseteq B$                       | Assumption
     20 | | | $x \in A \cup B$                    | Assumption
     30 | | | $x \in A \lor x \in B$              | Def. of union, (@20)
     40 | | | $x \in A \implies x \in B$          | Def. of subset, (@10)
     50 | | | $x \in B$                           | Or elimination, (@40), (@30)
     60 | | $x \in A \cup B \implies x \in B$     | II, (@20), (@50)
     70 | | $A \cup B \subseteq B$                | Def. of subset, (@60)
     80 | | $B \subseteq A \cup B$                | Subset union rule
     90 | | $A \cup B = B$                        | Subset equality rule
    100 | $A \subseteq B \implies A \cup B = B$   | II, (@10), (@90)
    110 | | $A \cup B = B$                        | Assumption
    120 | | | $x \in A$                           | Assumption
    130 | | | $x \in A \lor x \in B$              | Or introduction, (@120)
    140 | | | $x \in A \cup B$                    | Def. of union, (@130)
    150 | | | $x \in B$                           | (@140), (@110)
    160 | | $x \in A \implies x \in B$            | II, (@120), (@150)
    170 | | $A \subseteq B$                       | Def. of subset, (@160)
    180 | $A \cup B = B \implies A \subseteq B$   | II, (@110), (@170)
    190 | $A \subseteq B \iff A \cup B = B$       | Mat. equiv., (@100), (@180)
    ```

The output:

```{.logicproof}
 10 | | $A \subseteq B$                       | Assumption
 20 | | | $x \in A \cup B$                    | Assumption
 30 | | | $x \in A \lor x \in B$              | Def. of union, (@20)
 40 | | | $x \in A \implies x \in B$          | Def. of subset, (@10)
 50 | | | $x \in B$                           | Or elimination, (@40), (@30)
 60 | | $x \in A \cup B \implies x \in B$     | II, (@20), (@50)
 70 | | $A \cup B \subseteq B$                | Def. of subset, (@60)
 80 | | $B \subseteq A \cup B$                | Subset union rule
 90 | | $A \cup B = B$                        | Subset equality rule
100 | $A \subseteq B \implies A \cup B = B$   | II, (@10), (@90)
110 | | $A \cup B = B$                        | Assumption
120 | | | $x \in A$                           | Assumption
130 | | | $x \in A \lor x \in B$              | Or introduction, (@120)
140 | | | $x \in A \cup B$                    | Def. of union, (@130)
150 | | | $x \in B$                           | (@140), (@110)
160 | | $x \in A \implies x \in B$            | II, (@120), (@150)
170 | | $A \subseteq B$                       | Def. of subset, (@160)
180 | $A \cup B = B \implies A \subseteq B$   | II, (@110), (@170)
190 | $A \subseteq B \iff A \cup B = B$       | Mat. equiv., (@100), (@180)
```
