% $Id: functions.pl,v 1.3 2016-11-08 15:04:13-08 - - $
/* Ryan Teves | rteves@ucsc.edu
   Ky Nguyen | kymnguye@ucsc.edu
   CS 112 Spring 2017 - Assignment 4 - Prolog */

/*
* Airport Database.
* For each airport:
* - three-letter airport code
* - name of city
* - north latitude: degrees and minutes
* - west longitude: degrees and minutes
* North latitudes and West longitudes are in degrees, minutes.
*/

airport( atl, 'Atlanta         ', degmin(  33,39 ), degmin(  84,25 ) ).
airport( bos, 'Boston-Logan    ', degmin(  42,22 ), degmin(  71, 2 ) ).
airport( chi, 'Chicago         ', degmin(  42, 0 ), degmin(  87,53 ) ).
airport( den, 'Denver-Stapleton', degmin(  39,45 ), degmin( 104,52 ) ).
airport( dfw, 'Dallas-Ft.Worth ', degmin(  32,54 ), degmin(  97, 2 ) ).
airport( lax, 'Los Angeles     ', degmin(  33,56 ), degmin( 118,24 ) ).
airport( mia, 'Miami           ', degmin(  25,49 ), degmin(  80,17 ) ).
airport( nyc, 'New York City   ', degmin(  40,46 ), degmin(  73,59 ) ).
airport( sea, 'Seattle-Tacoma  ', degmin(  47,27 ), degmin( 122,18 ) ).
airport( sfo, 'San Francisco   ', degmin(  37,37 ), degmin( 122,23 ) ).
airport( sjc, 'San Jose        ', degmin(  37,22 ), degmin( 121,56 ) ).

/*
* Flight schedule.
* Flight number, departure airport, destination airport,
* departure time in hours, minutes.
*/

flight( bos, nyc, time(  7,30 ) ).
flight( dfw, den, time(  8, 0 ) ).
flight( atl, lax, time(  8,30 ) ).
flight( chi, den, time(  8,30 ) ).
flight( mia, atl, time(  9, 0 ) ).
flight( sfo, lax, time(  9, 0 ) ).
flight( sea, den, time( 10, 0 ) ).
flight( nyc, chi, time( 11, 0 ) ).
flight( sea, lax, time( 11, 0 ) ).
flight( den, dfw, time( 11,15 ) ).
flight( sjc, lax, time( 11,15 ) ).
flight( atl, lax, time( 11,30 ) ).
flight( atl, mia, time( 11,30 ) ).
flight( chi, nyc, time( 12, 0 ) ).
flight( lax, atl, time( 12, 0 ) ).
flight( lax, sfo, time( 12, 0 ) ).
flight( lax, sjc, time( 12, 0 ) ).
flight( nyc, bos, time( 12,15 ) ).
flight( bos, nyc, time( 12,30 ) ).
flight( den, chi, time( 12,30 ) ).
flight( dfw, den, time( 12,30 ) ).
flight( mia, atl, time( 13, 0 ) ).
flight( sjc, lax, time( 13,15 ) ).
flight( lax, sea, time( 13,30 ) ).
flight( chi, den, time( 14, 0 ) ).
flight( lax, nyc, time( 14, 0 ) ).
flight( sfo, lax, time( 14, 0 ) ).
flight( atl, lax, time( 14,30 ) ).
flight( lax, atl, time( 15, 0 ) ).
flight( nyc, chi, time( 15, 0 ) ).
flight( nyc, lax, time( 15, 0 ) ).
flight( den, dfw, time( 15,15 ) ).
flight( lax, sjc, time( 15,30 ) ).
flight( chi, nyc, time( 18, 0 ) ).
flight( lax, atl, time( 18, 0 ) ).
flight( lax, sfo, time( 18, 0 ) ).
flight( nyc, bos, time( 18, 0 ) ).
flight( sfo, lax, time( 18, 0 ) ).
flight( sjc, lax, time( 18,15 ) ).
flight( atl, mia, time( 18,30 ) ).
flight( den, chi, time( 18,30 ) ).
flight( lax, sjc, time( 19,30 ) ).
flight( lax, sfo, time( 20, 0 ) ).
flight( lax, sea, time( 22,30 ) ).

mathfns( X, List ) :-
   S is sin( X ),
   C is cos( X ),
   Q is sqrt( X ),
   List = [S, C, Q].

constants( List ) :-
   Pi is pi,
   E is e,
   Epsilon is epsilon,
   List = [Pi, E, Epsilon].

sincos( X, Y ) :-
   Y is sin( X ) ** 2 + cos( X ) ** 2.

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
   % Get degmin from the 2 airports
   airport(From, _, Lat1, Lon1),
   airport(To, _, Lat2, Lon2),
   % Convert to radians
   degmin_to_radians(Lat1, Lat1R),
   degmin_to_radians(Lat2, Lat2R),
   degmin_to_radians(Lon1, Lon1R),
   degmin_to_radians(Lon2, Lon2R),
   % Use haversine formula to compute distance
   haversine_radians(Lat1R, Lon1R, Lat2R, Lon2R, Distance).

is_connected(From, To) :- flight(From, To, _), 
   format('~w -----> ~w', [From, To]).
is_connected(From, To) :- 
   flight(From, Buffer, _), 
   is_connected(Buffer, To).

fly(From, To) :-
   airport(From, X, _, _),
   format('Flight from: ~w ~n', [X]),
   airport(To, Y, _, _),
   format('Flight to: ~w ~n', [Y]),
   distance(From, To, Z),
   format('Distance between ~w and ~w is ~w miles', [From, To, Z]).