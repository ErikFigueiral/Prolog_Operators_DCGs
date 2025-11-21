# LC_PUBLIC_Operators+DCGS

### Description
This repository try to make a easy example demonstrates of custom operators in **Prolog** and their equivalence with **DCGs (Definite Clause Grammars)**.  
For clarity, we start by "modifying" a simple predicate version of the Cartesian product.  
It includes examples of Cartesian product evaluation implemented both through operator definitions and DCG rules.

The focus is on exploiting **associativity** to design new operators. In Prolog, this can be achieved elegantly through unification and operator declarations. By contrast, in imperative paradigms, introducing new operators is far more complex: it requires modifications at the lexical, syntactic, and semantic levels of the language. Adding new lexical, syntactic, and semantic rules at runtime is not trivial, whereas Prolog allows this naturally and declaratively.

---

## Example: Cartesian Product in Prolog

```prolog
% Cartesian product: combine each element of L1 with each element of L2
cartesiano([], _, []).
cartesiano([Car|Cdr], C, R) :-
    linea(Car, C, L),
    cartesiano(Cdr, C, Resto),
    concatenar(L, Resto, R).

% Concatenate two lists
concatenar([], L, L).
concatenar([Car|Cdr], L, [Car|R]) :-
    concatenar(Cdr, L, R).

% Generate flat combinations
linea(_, [], []).
linea(Elemento, [Car|Cdr], [[Elemento|Car]|Resto]) :- % [Elemento|Car] =>[a,b,c] to solve [[a,b],c]
    linea(Elemento, Cdr, Resto).

```
## 1. Operator Definition Approach

We first define custom operators `x` and `igual` to express Cartesian products in a natural mathematical style.  
The operator `x` is defined with **higher precedence** so that chained products are grouped first, and `igual` is evaluated last to produce the final result.

```prolog
:- op(100, xfy, x).     % 'x' is associative to the right, higher priority
:- op(200, xfx, igual). % 'igual' has lower priority, evaluated last

L1 x L2 igual R :-
    cartesiano(L1, L2, R).

% Recursive case
L1 x Expr igual R :-
    Expr igual R2,
    cartesiano(L1, R2, R).

```
Example:
?- [1,2] x [a,b] x [foo,bar] igual R.


R = [[1,a,foo],[1,a,bar],[1,b,foo],[1,b,bar],
     [2,a,foo],[2,a,bar],[2,b,foo],[2,b,bar]].
     
Tree:
```text
                 igual
                /     \
               x       R
        ------------
        [1,2]    x
             ---------
           [a,b]   [foo,bar]
```
Other interesting form to do this and avoiding to add complexity making new operators and avoiding errors is use a predicate to help us:
```prolog
:- op(100, xfy, x).

eval(L1 x L2, R) :-
    cartesiano(L1, L2, R).

eval(L1 x Expr, R) :-
    eval(Expr, R2),
    cartesiano(L1, R2, R).
```
## 2. DCG Definition Approach

We can also express the same grammar using **Definite Clause Grammars (DCGs)**.  
A DCG is essentially a **grammar with explicit semantics**: it not only defines which sequences of tokens are valid (syntax), but also attaches computations (semantic actions) to those rules.  
This makes DCGs much more powerful than a pure grammar, because they can parse and evaluate at the same time.
```text
Sintax:
S → L x S
S → L x L
L → [lista]
Semantic:
⟦ L x L ⟧ = cartesiano(L1, L2)
⟦ L x S ⟧ = cartesiano(L1, ⟦S⟧)
```
```prolog
% Recognize a list token
lista(L) --> [L], {is_list(L)}.

% Recursive case: L x S
prod(R) --> lista(L1), [x], prod(R2), {cartesiano(L1, R2, R)}.

% Base case: L x L
prod(R) --> lista(L1), [x], lista(L2), {cartesiano(L1, L2, R)}.
```
Example:?- phrase(prod(R), [[1,2], x, [a,b], x, [foo,bar]]).

R = [[1, a|foo], [1, a|bar], [1, b|foo], [1, b|bar], [2, a|foo], [2, a|bar], [2, b|...], [2|...]] 


Prolog translates it into a Horn clause with extra arguments representing the input and output of the token stream:
```text
IN: the full list of tokens before parsing starts.

MID: intermediate state of the token list after consuming part of it.

OUT: the remaining tokens after parsing finishes.

So the DCG hides this mechanism, but internally it’s just passing lists around (In → Mid → Out) to keep track of what has been consumed.
```
```prolog
% Equivalent to: lista(L) --> [L], {is_list(L)}.
lista(L, [L|Rest], Rest) :-
    is_list(L).

% Grammar rules:
% S → L x S
% S → L x L
% L → [lista]

% Recursive case: L x S
expr(R, In, Out) :-
    lista(L1, In, Mid1),       
    Mid1 = [x|Mid2],       
    expr(R2, Mid2, Out),         
    cartesiano(L1, R2, R).    

% expr(R) --> lista(L1), [x], lista(L2), {cartesiano(L1, L2, R)}.
expr(R, In, Out) :-
    lista(L1, In, Mid1),         
    Mid1 = [x|Mid2],             
    lista(L2, Mid2, Out),       
    cartesiano(L1, L2, R).
```
?- expr(R, [[1,2], x, [a,b], x, [foo,bar]], []).


R = [[1,a,foo],[1,a,bar],[1,b,foo],[1,b,bar],
     [2,a,foo],[2,a,bar],[2,b,foo],[2,b,bar]].
