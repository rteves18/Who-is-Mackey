% $Id: functions.pl,v 1.3 2016-11-08 15:04:13-08 - - $
/* Ryan Teves | rteves@ucsc.edu
   Ky Nguyen | kymnguye@ucsc.edu
   CS 112 Spring 2017 - Assignment 4 - Prolog */

haversine_radians(Lat1, Lon1, Lat2, Lon2, Distance ) :-
   Dlon is Lon2 - Lon1,
   Dlat is Lat2 - Lat1,
   A is sin( Dlat / 2 ) ** 2
      + cos( Lat1 ) * cos( Lat2 ) * sin( Dlon / 2 ) ** 2,
   Dist is 2 * atan2( sqrt( A ), sqrt( 1 - A )),
   Distance is Dist * 3961.

% Converts degrees and minutes to radians so we can compute distance
degmin_to_radians(degmin(Degrees, Minutes), Radians) :-
   Degs is Degrees + Minutes / 60,
   Radians is Degs * pi / 180.

% Calculates the distance between 2 airports
distance(From, To, Distance) :-
   airport(From, _, Lat1, Lon1),
   airport(To, _, Lat2, Lon2),
   degmin_to_radians(Lat1, Lat1R),
   degmin_to_radians(Lat2, Lat2R),
   degmin_to_radians(Lon1, Lon1R),
   degmin_to_radians(Lon2, Lon2R),
   haversine_radians(Lat1R, Lon1R, Lat2R, Lon2R, Distance).

% Computes the flight time from point A to B
flight_time(From, To, FlightTime) :-
   distance(From, To, Distance),
   FlightTime is Distance / 500.

% Convert 00:00 time format to hours
hm_to_hours( time(Hours, Mins), Ret) :-
   Ret is Hours + Mins / 60.

% Append 0 to the front if Time < 10
print_time_format(Time) :-
   Time < 10, print(0), print(Time).   
print_time_format(Time) :-
   Time >= 10, print(Time).

% Convert hours back to 00:00 time format and print it
print_time(Hours) :-
   HoursDecimal is Hours - floor(Hours),
   ReturnHours is floor(Hours),
   MinsDigits is round(HoursDecimal * 60),
   print_time_format(ReturnHours), 
   print(':'), 
   print_time_format(MinsDigits).

% Compute arrival time in 00:00 format
arrival_time(DepartureTimeInHours, FlightTime, ArrivalTime) :-
   ArrivalTime is DepartureTimeInHours + FlightTime.

% Compute the transfer time between flights
transfer_time(ArrivalTimeH, NextDepartureTimeH, TransferTime) :-
   TransferTime is NextDepartureTimeH - ArrivalTimeH - 0.5.

% Prolog version of not.
not( X ) :- X, !, fail.
not( _ ).

/* Writes the path out in a friendly format.
 * List passed in args has the following form:
 *    - [[From, DepartureTimeHours, ArrivalTimeHours], To]
 */
write_path([]) :-
   nl.

% If there's a direct flight between the two Airports   
write_path([[From, DTimeH, ATimeH], To]) :-
   airport(From, From_Name, _, _),
   airport(To, To_Name, _, _),
   write('  '), write('depart  '), write(From), write('  '),
   write(From_Name),
   print_time(DTimeH), nl,

   write('  '), write('arrive  '), write(To), write('  '),
   write(To_Name),
   print_time(ATimeH), nl,
   !.

% If there is not a direct flight between the two Airports
write_path([[From, DTimeH, ATimeH], [To, DTimeH2, ATimeH2] | Rest]) :-
   airport(From, From_Name, _, _),
   airport(To, To_Name, _, _),
   write('  '), write('depart  '), write(From), write('  '), 
   write(From_Name),
   print_time(DTimeH), nl,

   write('  '), write('arrive  '), write(To), write('  '),
   write(To_Name),
   print_time(ATimeH), nl,
   !, write_path([[To, DTimeH2, ATimeH2] | Rest]).


/* Searches for a path from departure to arrival location.
 * Pre-conditions:
 *  - flights must finish within the day (< 24 hours).
 *  - flights transfer time must be at least 0.5 hours.
 * List returned contains:
 *  - List of airports along with their departure & arrival times:
 *       [[From, DepartureTimeHours, ArrivalTimeHours], To]
 */
list_path(To, To, _, [To], _).

% If there is a directly connected path
list_path(From, To, Visited, [[From, DTimeH, ATimeH]|List], DTimeHM) :-
   flight(From, To, DTimeHM),
   hm_to_hours(DTimeHM, DTimeH),
   flight_time(From, To, FlightTime),
   arrival_time(DTimeH, FlightTime, ATimeH),
   list_path(To, To, [To | Visited], List, _).

% If there is a indirectly connected path
list_path(From, To, Visited, [[From, DTimeH, ATimeH]|List], DTimeHM) :-
   flight(From, Next, DTimeHM),
   not(member(Next, Visited)), % Check if Airport has been visited
   hm_to_hours(DTimeHM, DTimeH),
   flight_time(From, Next, FlightTime),
   arrival_time(DTimeH, FlightTime, ATimeH),
   ATimeH < 24.0, % Check if these flights are within the day
   flight(Next, _, NextDTimeHM),
   hm_to_hours(NextDTimeHM, NextDTimeH),
   transfer_time(ATimeH, NextDTimeH, TransferTime),
   TransferTime >= 0, % Make sure transfer time is at least 0.5 hours
   list_path(Next, To, [Next | Visited], List, NextDTimeHM).

/*list_path( Node, Node, _, [Node] ).
list_path( Node, End, Tried, [Node|List] ) :-
   flight( Node, Next, _ ),
   not( member( Next, Tried )),
   list_path( Next, End, [Next|Tried], List ).*/

% Fail if departure and arrival location are the same
fly(From, From) :-
   write('Woops! Cannot complete flight request.'), 
   write(' Either departure and arrival is in the same location.'),
   nl,
   write(' Or the location specified is invalid.'),
   nl, !, fail.

fly(From, To) :-
   /*airport(From, X, _, _),
   format('Flight from: ~w ~n', [X]),
   airport(To, Y, _, _),
   format('Flight to: ~w ~n', [Y]),
   distance(From, To, Distance),
   format('Distance between ~w & ~w is ~w miles ~n', 
   [From, To, Distance]),
   flight_time(From, To, FlightTime),
   format('Flight time between these 2 Airports is ~w ~n', 
   [FlightTime]),
   flight(From, To, DepartureTime),
   arrival_time(DepartureTime, FlightTime, ArrivalTime),
   format('Arrival time is ~w ~n', [ArrivalTime]), 
   nl.*/
   airport(From, _, _, _),
   airport(To, _, _, _),

   % Find a potential path between these 2 airports
   list_path(From, To, [From], List, _),
   !, nl, % Stop backtracking as soon as a path is found
   write_path(List).

% Fail if a flight path cannot be found
fly(From, To) :-
   airport(From, _, _, _),
   airport(To, _, _, _),
   write('Woops! Looks like a flight from *'), 
   write(From),
   write('* to *'), 
   write(To), 
   write('* is not available.'), !, fail.

% Fail if airport cannot be found in db
fly(_, _) :-
   write('Woops, invalid entry. Airport not found!'), nl, !, fail.







