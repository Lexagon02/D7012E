% Leo Rydeker Bergström
% Transform the input list into a list of tuples (Index, Value)
index_list([], _, []).
index_list([Head|Tail], Index, [(Index, Head)|IndexedTail]) :-
    NextIndex is Index + 1,
    index_list(Tail, NextIndex, IndexedTail).

recreate_with_indices(List, IndexedList) :-
    index_list(List, 1, IndexedList).

% Generate all subsets with sums for a list of tuples (Index, Value)
find_all_subsets_with_sums([], [([], 0)]).
find_all_subsets_with_sums([(Index, Value)|Tail], SubsetsWithSums) :- 
    find_all_subsets_with_sums(Tail, TailSubsetsWithSums), 
    maplist(prepend_with_sum((Index, Value)), TailSubsetsWithSums, NewSubsetsWithSums), 
    append(TailSubsetsWithSums, NewSubsetsWithSums, AllSubsetsWithSums),
    filter_valid_subsets(AllSubsetsWithSums, SubsetsWithSums).

% Filter subsets to ensure they are valid
filter_valid_subsets([], []).
filter_valid_subsets([((Subset, Sum))|Tail], [((Subset, Sum))|FilteredTail]) :-
    is_valid_subset(Subset),
    filter_valid_subsets(Tail, FilteredTail).
filter_valid_subsets([_|Tail], FilteredTail) :-
    filter_valid_subsets(Tail, FilteredTail).

% Check if a subset is valid
is_valid_subset([]).
is_valid_subset([_]).
is_valid_subset([(Index1, _), (Index2, _)|Tail]) :-
    Index2 =:= Index1 + 1, % Ensure indices are consecutive
    is_valid_subset([(Index2, _)|Tail]).

% Remove the first and last subsets from the list
remove_first_and_last(List, Filtered) :-
    append([_|Middle], [_], List),
    Filtered = Middle.

prepend_with_sum((Index, Value), (Subset, Sum), ([(Index, Value)|Subset], NewSum)) :-
    NewSum is Sum + Value.

% Sort subsets by their sums in ascending order using quicksort
sort_subsets_by_sum(SubsetsWithSums, SortedSubsets) :-
    quicksort(SubsetsWithSums, SortedSubsets).

quicksort([], []).
quicksort([Pivot|Tail], Sorted) :-
    partition(Pivot, Tail, Less, Greater),
    quicksort(Less, SortedLess),
    quicksort(Greater, SortedGreater),
    append(SortedLess, [Pivot|SortedGreater], Sorted).

partition((_, _), [], [], []).
partition((_, Sum1), [((Subset, Sum2))|Tail], [((Subset, Sum2))|Less], Greater) :-
    Sum2 =< Sum1, % Change comparison to <= for ascending order
    partition((_, Sum1), Tail, Less, Greater).
partition((_, Sum1), [((Subset, Sum2))|Tail], Less, [((Subset, Sum2))|Greater]) :-
    Sum2 > Sum1, % Change comparison to > for ascending order
    partition((_, Sum1), Tail, Less, Greater).

extract_indices_and_values([], _, _, []).
extract_indices_and_values([(Index, Value)], FirstIndex, LastIndex, [Value]) :-
    % For a single-element subset, set both FirstIndex and LastIndex to Index
    FirstIndex = Index,
    LastIndex = Index.
extract_indices_and_values([(Index, Value)|Tail], FirstIndex, LastIndex, [Value|ValuesTail]) :-
    ( var(FirstIndex) -> FirstIndex = Index ; true ), % Set FirstIndex only for the first element
    ( Tail = [] -> LastIndex = Index ; true ),        % If this is the last element, set LastIndex
    extract_indices_and_values(Tail, FirstIndex, LastIndex, ValuesTail).

print_n_smallest_subsets([], _).
print_n_smallest_subsets(_, 0).
print_n_smallest_subsets([((Subset, Sum))|Subsets], N) :-
    N > 0,
    extract_indices_and_values(Subset, FirstIndex, LastIndex, SubsetValues),
    format('~w, ~w, ~w, ~w', [Sum, FirstIndex, LastIndex, SubsetValues]), nl,
    N1 is N - 1,
    print_n_smallest_subsets(Subsets, N1).

% Queries [-1,2,-3,4,-5] k=3, [3,2,-4,3,2,-5,-2,2,3,-3,2,-5,6,-2,2,3] k=8, [24,-11,-34,42,-24,7,-19,21] k=6
?- recreate_with_indices([24,-11,-34,42,-24,7,-19,21], IndexedList),
   find_all_subsets_with_sums(IndexedList, SubsetsWithSums),
   remove_first_and_last(SubsetsWithSums, FilteredSubsetsWithSums),
   sort_subsets_by_sum(FilteredSubsetsWithSums, SortedSubsets),
   print_n_smallest_subsets(SortedSubsets, 6).
