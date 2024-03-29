% Write a program to calculate factorials.
% The name should be `factorial`, and the first parameter
% should be the value to get the factorial of, and the
% second parameter the answer.
%
% Recall that factorial can be recursively defined as such:
%
% 0! = 1
% n! = n * (n - 1)!, for n > 0

factorial(0, 1).
factorial(N, F) :-
    N > 0,
    N1 is N -1,
    factorial(N1, F1),
    F is N * F1.

% With your factorial routine in hand, the following queries should
% all succeed:

% QUERY 1: factorial(0, 1).
% QUERY 2: factorial(1, 1).
% QUERY 3: factorial(2, 2).
% QUERY 4: factorial(4, 24).

