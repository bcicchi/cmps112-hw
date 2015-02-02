/**
 * Program: hw3.pl
 * Author: Brendan Cicchi - bcicchi - 1361334
 * HW3 Prolog - done solo
 */
 

father(al, bud).
father(al, kelly).
mother(peggy, kelly).
mother(peggy, bud).
mother(martha, peggy).

/** Problem 7 & 8 **/
grandma(X,Z) :- mother(Y,Z), mother(X,Y).

/** Problem 9 **/
descendants(X,Y):- child(X,Y).
descendants(X,Y):- 
		child(X,Z),
		descendants(Z,Y).

%helper function child		
child(C,P) :- father(P,C); mother(P,C).

/** Problem 10 **/
siblings(X,Y):- child(X,Z), child(Y,Z), X \= Y.

/** Problem 11 **/
% Transition relation:
transition(q0,q1,a).
transition(q1,q2,b).
transition(q0,q3,a).
transition(q3,q3,a).

% Accepting states:
accepting(q2).
accepting(q3).

accepts(State, []) :- accepting(State).
accepts(State,[H|T]) :- transition(State, State2, H),
						accepts(State2, T).