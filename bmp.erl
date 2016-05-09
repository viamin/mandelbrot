%% @author Bart Agapinan [http://www.mylifeinthevalley.com]
%% @copyright 2008 Bart Agapinan ;)
%% @doc This module contains some functions to help create simple, uncompressed bitmap images
%% in the windows BMP format. 
%% @version 1.0

-module(bmp).
-export([header/2, dib_header/3, color_table/1, filesize/3]).

-record(rgbquad, {
  blue = <<0:8>>,
  green = <<0:8>>,
  red = <<0:8>>,
  zilch = <<0:8>>
}).

%-define(debug, 3).

-ifdef(debug).
-define(FENTRY(X), io:format("Entering function ~p:~p ~p~n", [?MODULE, ?LINE, X])).
-define(TRACE(X), io:format("TRACE ~p:~p ~p~n", [?MODULE, ?LINE, X])).
-define(NTRACE(Name, X), io:format("TRACE ~p:~p ~p:~p~n", [?MODULE, ?LINE, Name, X])).
-else.
-define(FENTRY(X), void).
-define(TRACE(X), void).
-define(NTRACE(Name, X), void).
-endif.

%% This module will handle Windows style BMP images
%% originally created to output the Mandelbrot pixels

%% @proc header(Filesize, Depth) -> Bin generates the BMP header data
header(Filesize, Depth) ->
  ?FENTRY("header"),
  Size = bin_to_int(reverse_binary(Filesize, 32)),
  Offset = bin_to_int(reverse_binary((54 + round(math:pow(2,Depth) * 4)), 32)),
  <<"BM", Size:32, 0:16, 0:16, Offset:32>>.

%% @proc reverse_binary(Int, Size) -> Bin
reverse_binary(Int, Size) ->
  list_to_binary(lists:reverse(binary_to_list(<<Int:Size>>))).

%% @proc dib_header(Depth, Width, Height) -> Bin generates the DIB portion of the header
dib_header(Depth, W, H) -> 
  ?FENTRY("dib_header"),
  Size = bin_to_int(reverse_binary(40, 32)),
  Width = bin_to_int(reverse_binary(W, 32)),
  Height = bin_to_int(reverse_binary(H, 32)),
  One = bin_to_int(reverse_binary(1, 16)),
  D = bin_to_int(reverse_binary(Depth, 16)),
  <<Size:32, Width:32, Height:32, One:16, D:16, 0:32, 0:32, 0:32, 0:32, 0:32, 0:32>>.

%% @proc color_table(Depth)
color_table(Depth) -> 
  color_table(round(math:pow(2,Depth)), []).

color_table(0, Acc) ->
  Colors = lists:reverse(Acc),
  Colorbits = lists:map(fun(X) -> rgbquad_to_binary(X) end, Colors),
  list_to_binary(Colorbits);
color_table(Depth, []) -> 
  Black = #rgbquad{},
  White = #rgbquad{red = <<255:8>>, green = <<255:8>>, blue = <<255:8>>},
  color_table(Depth - 2, [Black, White]);
color_table(Depth, Acc) ->
  color_table(Depth - 1, [random_color()|Acc]).

%% @proc rgbquad_to_binary(RGBQUAD) -> Binary
rgbquad_to_binary(#rgbquad{red=Red, green=Green, blue=Blue}) ->
  RGBQuad_as_list = [Blue,Green,Red,0],
  list_to_binary(RGBQuad_as_list).

%% @proc random_color() returns a random rgbquad record
random_color() ->
  Rred = random:uniform(255),
  Rblue = random:uniform(255),
  Rgreen = random:uniform(255),
  #rgbquad{red = <<Rred:8>>, green = <<Rgreen:8>>, blue = <<Rblue:8>>}.

%% @proc filesize(Depth, Width, Height) -> Size calculates filesize for a given size
%% taken from the wikipedia page
filesize(Depth, W, H) -> 
  ?FENTRY("filesize"),
  Rowsize = 4 * round((((Depth * W) + 31) / 32 ) - 0.5),
  ?NTRACE("Rowsize", Rowsize),
  54 + (4 * (1 bsl Depth)) + (Rowsize * H).

%% @proc bin_to_int(Bin) -> Int
bin_to_int(Bin) ->
  bin_to_int(lists:reverse(binary_to_list(Bin)), []).

bin_to_int([H|T], []) ->
  bin_to_int(T, [H]);
bin_to_int([], Acc) ->
  unroll_bin_list(Acc, 0);
bin_to_int([H|T], Acc) ->
  bin_to_int(T, [H|Acc]).

%% @proc unroll_bin_list(List, Int) -> Int
unroll_bin_list([], Int) ->
  Int;
unroll_bin_list([H|T], Int) ->
  unroll_bin_list(T, Int + (H bsl (length(T) * 8))).
