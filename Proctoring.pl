assign_proctors(AllTAs, Quizzes, TeachingSchedule, ProctoringSchedule) :-
    free_schedule(AllTAs, TeachingSchedule, FreeSchedule),
    assign_quizzes(Quizzes, FreeSchedule, ProctoringSchedule).



free_schedule(AllTAs, TeachingSchedule, FreeSchedule) :-
    each_day(AllTAs, TeachingSchedule, FreeSchedule).

each_day(_, [], []).
each_day(AllTAs, [day(DayName, Slots)|RestOfDays], [day(DayName, FreeSlots)|FreeRestOfDays]) :-
    each_slot(AllTAs, Slots, FreeSlots),
    each_day(AllTAs, RestOfDays, FreeRestOfDays).

each_slot(_, [], []).
each_slot(AllTAs, [Slot|RestOfSlots], [FreeTAs|RestOfFreeSlots]) :-
    free_ta(AllTAs, Slot, FreeTAs),
    each_slot(AllTAs, RestOfSlots, RestOfFreeSlots).

free_ta([], _, []).
free_ta([ta(TA, _)|RestTAs], Slot, FreeTAs) :-
    member(TA, Slot),
    !,
    free_ta(RestTAs, Slot, FreeTAs).
free_ta([ta(TA, DayOff)|RestTAs], Slot, [TA|RestOfFreeTAs]) :-
    DayOff \= Slot,
    free_ta(RestTAs, Slot, RestOfFreeTAs).

permutation_helper(_, [], 0).
permutation_helper(AllSlotTAs, [Name|RestOfNames], Count) :-
    Count > 0,
    select(Name, AllSlotTAs, Rest),
    NewCount is Count - 1,
    permutation_helper(Rest, RestOfNames, NewCount).


	
assign_quiz(quiz(Course, DayName, SlotNo, Count), FreeSchedule, AssignedTAs) :-
    member(day(DayName, FreeSlots), FreeSchedule),
    nth1(SlotNo, FreeSlots, FreeTAs),
    length(FreeTAs, FreeTAsCount),
    FreeTAsCount >= Count,
    permutation_helper(FreeTAs, AssignedTAsList, Count),
    AssignedTAs = proctors(quiz(Course, DayName, SlotNo, Count), AssignedTAsList).

assign_quizzes([], _, []).
assign_quizzes([Quiz|RestQuizzes], FreeSchedule, [AssignedTAs|RestProctors]) :-
    assign_quiz(Quiz, FreeSchedule, AssignedTAs),
    assign_quizzes(RestQuizzes, FreeSchedule, RestProctors).


assign_quiz_helper(_, _, [], []).
assign_quiz_helper(quiz(Course, DayName, SlotNo, Count), DayName, [Slot|RestSlots], AssignedTAs) :-
    assign_TAs(Count, Slot, AssignedTAs),
    assign_quiz_helper(quiz(Course, DayName, SlotNo, Count), DayName, RestSlots, AssignedTAs).


assign_TAs(Count, FreeTAs, AssignedTAs) :-
    length(FreeTAs, FreeTAsCount),
    length(AssignedTAs, Count),
    subset(AssignedTAs, FreeTAs),
    length(AssignedTAs, AssignedCount),
    AssignedCount =< FreeTAsCount,
    permutation(FreeTAs, AssignedTAs).



free_schedule(AllTAs, TeachingSchedule, FreeSchedule) :-
    maplist(free_schedule_day(AllTAs), TeachingSchedule, FreeSchedule).


free_schedule_day(AllTAs, day(DayName, Slots), day(DayName, FreeSlots)) :-
    maplist(free_ta(AllTAs, DayName), Slots, FreeSlots).


free_ta(AllTAs, DayName, Slot, FreeTAs) :-
    findall(TA, (member(ta(TA, DayOff), AllTAs),
                 \+ (DayName == DayOff; member(TA, Slot))),
            FreeTAs).
			