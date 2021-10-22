% Task 2: Relational Data

% The line below imports the data
:- ['one.pl'].

% group(X,L) :- findall(Z,student(X,Z),L).

:- encoding(utf8).
%  Вариант представления: 1 (one.pl)
%  Вариант задания: 2

%  1) Напечатать средний балл для каждого предмета

%  подсчет общей суммы оценок по предмету
%  (список, сумма)
sum_grades([],0).
sum_grades([H|T], S) :-
	sum_grades(T, S1), 
	S = H + S1.	
	
%  рассчет среднего балла для каждого предмета
%  (список)
stud_lists([]).
stud_lists([H|T]) :- 
	findall(X, grade(_, H, X), MarksList),
	sum_grades(MarksList, Sum),
	length(MarksList, Len),
	Mark is Sum / Len,
	write(H),
	write(': '),
	write(Mark),
	write('\n'),
	stud_lists(T).	

%  выделение списка предметов и вызов основной функции	
average() :-
	findall(X, subject(X, _), Subs),
	stud_lists(Subs).
	
%  также, чтобы выводить средний балл по введенному вручную названию предмета,
%  можно воспользоваться следующим предикатом:
%  рассчет среднего балла по предмету
%  (предмет, средний балл)
average(Subj, Mark) :- 
	subject(EngSubj, Subj),
	findall(X, grade(_, EngSubj, X), MarksList),
	sum_grades(MarksList, Sum),
	length(MarksList, Len),
	Mark is Sum / Len,
	write(EngSubj),
	write(: ).
	
%  2) Для каждой группы найти количество несдавших студентов

%  проверка на пустоту списка с двойками
%  (список, флаг (0 - нет пересдач, 1 - есть пересдача (или пересдачи))
is_empty([], 0).
is_empty([_|_], 1).

%  выявление несдавших студентов
%  (список студентов, количество студентов)
has_bad_marks([], 0).
has_bad_marks([H|T], C) :-
	findall(X, grade(H, X, 2), Peresdachi),
	is_empty(Peresdachi, N),
	has_bad_marks(T, C1), C = N + C1.

%  рассчет количества несдавших по каждой группе
%  (список групп)
find_bad_students([]).
find_bad_students([H|T]) :-
	findall(X, student(H, X), StL),
	has_bad_marks(StL, Num),
	Count is Num,
	write(H),
	write(': '),
	write(Count),
	write('\n'),
	find_bad_students(T).

%  количество несдавших для каждой группы
fallens() :-
	findall(X, student(X, _), Groups),
	list_to_set(Groups, Gr),
	find_bad_students(Gr).
	
%  также, чтобы выводить количество несдавших по введенному вручную номеру группы,
%  можно воспользоваться следующим предикатом:
%  количество несдавших в группе
%  (группа, количество студентов)
fallen_number(Group, Count) :-
	findall(X, student(Group, X), StL),
	has_bad_marks(StL, Num),
	Count is Num.

%  3) Найти количество не сдавших студентов для каждого из предметов

%  подсчет количества двоек по предмету
%  (список оценок, счетчик)
count_falls([],0).
count_falls([H|T],N):- H < 3, !, count_falls(T,M), N is M + 1.
count_falls([_|T],N):- count_falls(T,N).

%  подсчет несдавших по конкретному предмету
%  (список предметов)
by_subject([]).
by_subject([H|T]) :-
	findall(X, grade(_, H, X), Marks),
	count_falls(Marks, Num),
	write(H),
	write(': '),
	write(Num),
	write('\n'),
	by_subject(T).
	
%  подсчет несдавших по каждому предмету
fallen_by_all_subjects() :-
	findall(X, subject(X, _), Subs),
	by_subject(Subs).

%  также, чтобы выводить количество несдавших по введенному вручную названию предмета,
%  можно воспользоваться следующим предикатом:
%  количество несдавших по предмету
%  (предмет, число несдавших)
fallen_by_subject(Subj, X) :-
	subject(EngSubj, Subj),
	findall(X, grade(_, EngSubj, X), MarksList),
	count_falls(MarksList, X).
