% Interactive Test Script for talon-organteq Prolog
% =================================================
%
% Start by loading the system:
%   cd prolog && swipl -g "consult('main.pl')."
%
% Then paste these commands one at a time (or in small groups) to verify
% each subsystem works. Comments explain what to expect.


% ============================================================================
% 1. VERIFY MODULES LOADED
% ============================================================================

% Check that config modules are available
manual(X).
% Should return: X = pedal ; X = choir ; X = great ; X = swell

family_of('Trompette', X).
% Should return: X = reed

aliases(X), member('Trumpet', X).
% Should return a list containing Trumpet, Trompette, Trommet


% ============================================================================
% 2. SET UP SOME TEST STATE
% ============================================================================

% Simulate a sync from Organteq - this is what Python would send
% Let's create a minimal organ with a few stops per division

execute("sync('Test Organ', [
    element(great, 1, 'Montre 8\\'', stop),
    element(great, 2, 'Bourdon 8\\'', stop),
    element(great, 3, 'Prestant 4\\'', stop),
    element(great, 4, 'Trompette 8\\'', stop),
    element(great, 5, 'Mixtura IV', stop),
    element(swell, 1, 'Gedact 8\\'', stop),
    element(swell, 2, 'Salicional 8\\'', stop),
    element(swell, 3, 'Voix Celeste 8\\'', stop),
    element(swell, 4, 'Hautbois 8\\'', stop),
    element(pedal, 1, 'Subbass 16\\'', stop),
    element(pedal, 2, 'Prinzipal 8\\'', stop),
    element(pedal, 3, 'Posaune 16\\'', stop),
    element(coupler, 1, 'Swell to Great', coupler),
    element(coupler, 2, 'Great to Pedal', coupler)
], [])", Actions, State).

% Should succeed with Actions = [], State showing empty engaged list
% Check the state was set:

current_preset(P).
% Should return: P = 'Test Organ'

findall(N-Name, element(great, N, Name, stop), Stops).
% Should show all 5 great stops


% ============================================================================
% 3. TEST CLASSIFICATION
% ============================================================================

element_family(great, 1, F).
% Should return: F = principal  (Montre is a principal)

element_family(great, 4, F).
% Should return: F = reed  (Trompette is a reed)

element_footage(great, 1, Ft).
% Should return: Ft = 8

element_footage(great, 3, Ft).
% Should return: Ft = 4


% ============================================================================
% 4. TEST SELECTORS
% ============================================================================

resolve_selector(great, numbers([1,2,3]), Els).
% Should return: Els = [1,2,3]

resolve_selector(great, family(principal), Els).
% Should return stops 1 and 3 (Montre and Prestant)

resolve_selector(great, family(principal, 8), Els).
% Should return just stop 1 (only 8' principal)

resolve_selector(great, family(reed), Els).
% Should return stop 4 (Trompette)

resolve_selector(great, all, Els).
% Should return [1,2,3,4,5]

resolve_selector(swell, family(string), Els).
% Should return stops 2 and 3 (Salicional and Voix Celeste)


% ============================================================================
% 5. TEST BASIC STOP OPERATIONS
% ============================================================================

execute("engage(great, numbers([1,2]))", Actions, State).
% Should return Actions with set_stop(great,1,1.0) and set_stop(great,2,1.0)
% State.engaged should show great/1 and great/2

engaged(great, X).
% Should return X = 1 ; X = 2

execute("toggle(great, numbers([2,3]))", Actions, State).
% Stop 2 was on, should turn off. Stop 3 was off, should turn on.
% Actions: set_stop(great,2,0.0), set_stop(great,3,1.0)

engaged(great, X).
% Should return X = 1 ; X = 3

execute("disengage(great, numbers([1]))", Actions, State).
% Should turn off stop 1

engaged(great, X).
% Should return just X = 3

execute("clear(great)", Actions, State).
% Should turn off everything on great

engaged(great, _).
% Should fail (nothing engaged)


% ============================================================================
% 6. TEST FAMILY-BASED OPERATIONS
% ============================================================================

execute("engage(great, family(principal))", Actions, State).
% Should engage stops 1 and 3 (the principals)

engaged(great, X).
% Should return X = 1 ; X = 3

execute("engage(swell, family(string))", Actions, State).
% Should engage swell stops 2 and 3

engaged(swell, X).
% Should return X = 2 ; X = 3

execute("disengage(great, family(principal, 4))", Actions, State).
% Should disengage only 4' principal (stop 3)

engaged(great, X).
% Should return just X = 1


% ============================================================================
% 7. TEST COMPOUND SELECTORS
% ============================================================================

execute("clear_all", _, _).
% Clean slate

execute("engage(great, union([family(reed), family(mixture)]))", Actions, State).
% Should engage stops 4 (reed) and 5 (mixture)

engaged(great, X).
% Should return X = 4 ; X = 5

execute("engage(great, numbers([1,2,3]))", _, _).
% Add some more stops

execute("disengage(great, intersection([family(principal), engaged]))", Actions, State).
% Should disengage engaged principals - that's stops 1 and 3

engaged(great, X).
% Should return X = 2 ; X = 4 ; X = 5


% ============================================================================
% 8. TEST SOLO OPERATION
% ============================================================================

execute("solo(great, family(flute))", Actions, State).
% Should engage only flutes (stop 2 = Bourdon), disengage everything else

engaged(great, X).
% Should return just X = 2


% ============================================================================
% 9. TEST UNDO/REDO
% ============================================================================

execute("clear_all", _, _).
execute("engage(great, numbers([1]))", _, _).
execute("engage(great, numbers([2]))", _, _).
execute("engage(great, numbers([3]))", _, _).

engaged(great, X).
% Should return X = 1 ; X = 2 ; X = 3

execute("undo", Actions, State).
% Should undo the last engage (stop 3)

engaged(great, X).
% Should return X = 1 ; X = 2

execute("undo", _, _).
engaged(great, X).
% Should return just X = 1

execute("redo", _, _).
engaged(great, X).
% Should return X = 1 ; X = 2


% ============================================================================
% 10. TEST RULES (if you have any defined)
% ============================================================================

% First, let's define a simple test rule inline:
assertz(state:rule(test_foundation, persistent)).
assertz(state:rule_selector(test_foundation, 1, great, family(principal, 8))).
assertz(state:rule_selector(test_foundation, 2, great, family(principal))).
assertz(state:rule_selector(test_foundation, 2, pedal, numbers([1,2]))).

execute("clear_all", _, _).

execute("up(test_foundation)", Actions, State).
% Should engage level 1: great 8' principals (stop 1)

engaged(great, X).
% Should return X = 1

get_rule_level(test_foundation, L).
% Should return L = 1

execute("up(test_foundation)", Actions, State).
% Should engage level 2: all principals + pedal stops

engaged(great, X).
% Should return X = 1 ; X = 3

engaged(pedal, X).
% Should return X = 1 ; X = 2

get_rule_level(test_foundation, L).
% Should return L = 2

execute("down(test_foundation)", Actions, State).
% Should go back to level 1

engaged(pedal, _).
% Should fail (pedal stops released)

execute("mute(test_foundation)", Actions, State).
% Should go to level 0

engaged(great, _).
% Should fail (everything released)


% ============================================================================
% 11. VERIFY STATE DICT OUTPUT
% ============================================================================

execute("get_state", [], State).
% State should be a dict with preset, engaged list, rule_levels


% ============================================================================
% CLEANUP
% ============================================================================

% Remove our test rule
retractall(state:rule(test_foundation, _)).
retractall(state:rule_selector(test_foundation, _, _)).
retractall(state:rule_selector(test_foundation, _, _, _)).


% ============================================================================
% SUCCESS!
% ============================================================================
% If all the above worked as described, your Prolog system is correctly set up.
% 
% To start the HTTP server:
%   server(5000).
%
% Then Python can POST to http://localhost:5000/execute with JSON like:
%   {"command": "engage(great, numbers([1,2,3]))"}
