of_type(Variable, CompoundTerm):-
    Variable=CompoundTerm,
    CompoundTerm.

/*
Check each head of the program
if head contains a reference to of/2, typecheck that reference,

prepend a call to of_type in the body of the predicate.

check_type(Program).
*/
