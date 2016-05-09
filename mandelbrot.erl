%% @author Bart Agapinan [http://www.mylifeinthevalley.com]
%% @copyright 2008 Bart Agapinan
%% @doc This file generates a BMP file of the Mandelbrot set at the bit depth and size
%% the user chooses. To create the BMP, use the command 'mandelbrot:start(BitDepth, Width, Height).'
%% @version 1.0

-module(mandelbrot).
-export([start/3, clear/0]).

-define(MIN_REAL, -2.0).
-define(MAX_REAL, 1.0).
-define(MIN_IM, -1.2).
-define(MAX_LOOPS, 30).
-define(FILE_PATH, "/tmp/mandelbrot.bmp").

%% Seems the best way to do this is to iterate over all of the pixels
%% and spawn a new process for each one. Calculate the value of that pixel
%% and return it to a collector with the coordinate of that pixel.
%% The collector will send the pixels to an image file.

%% Need a few processes: 1 will own the ets data, 
%% one will spawn calculation processes
%% one will wait for the ets data to be full and then dump pixel data to a file

%% @proc clear() unregisters all registered processes
clear() -> 
  unregister(bmp),
  unregister(file),
  unregister(pixels).

%% @proc setup(X, Y, width = W, height = H)
setup(1,1,W,H) ->
  calc(1,1,W,H);
setup(1, Y, W, H) ->
  calc(1,Y,W,H),
  setup(W, Y-1, W, H);
setup(X, Y, W, H) ->
  calc(X,Y,W,H),
  setup(X-1, Y, W, H).

%% @proc start(Depth, Width, Height)
start(Depth, W, H) ->
  TableId = ets:new(pixels, [ordered_set, public]),
  register(pixels, spawn(fun() -> ets_loop(TableId, Depth, W, H) end)),
  register(bmp, spawn(fun() -> create_bmp() end)),
  register(file, spawn(fun() -> create_file() end)),
  setup(W,H,W,H),
  ets:tab2list(TableId),
  pixels ! {write}.

%% @proc Im_max(Width, Height) -> Im_num
im_max(W, H) ->
  ?MIN_IM + ((?MAX_REAL - ?MIN_REAL) * (H/W)).

%% @proc Re_factor(Width) -> Real factor
re_factor(W) ->
  (?MAX_REAL - ?MIN_REAL) / (W - 1).

%% @proc Im_factor(Width, Height) -> Imaginary factor
im_factor(W, H) ->
  (im_max(W,H) - ?MIN_IM) / (H - 1).

%% @proc calc(x, y)
calc(X,Y,W,H) ->
  C_im = im_max(W,H) - (Y * im_factor(W, H)),
  C_re = ?MIN_REAL + (X * re_factor(W)),
  loop(X, Y, {C_re, C_im}, {C_re, C_im}, 0, true).

%% @proc loop(X, Y, C, Z, Iterations, Is_inside)
loop(X, Y, _C, _Z, ?MAX_LOOPS, Is_inside) -> 
  case Is_inside of
    false -> pixels ! {X, Y, blank};
    _     -> pixels ! {X, Y, black}
  end;
loop(X, Y, _C, _Z, Iter, false) ->
  Color = color_for_iter(Iter),
  pixels ! {X, Y, Color};
loop(X, Y, {C_re, C_im} = C, {Z_re, Z_im}, Iter, Is_inside) -> 
  Z_re2 = Z_re * Z_re,
  Z_im2 = Z_im * Z_im,
  if
    (Z_re2 + Z_im2) > 4 -> 
      loop(X, Y, C, {Z_re, Z_im}, Iter, false);
    true -> 
      loop(X, Y, C, {Z_re2 - Z_im2 + C_re, 2 * Z_re * Z_im + C_im}, Iter + 1, Is_inside)
  end.

%% @proc color_for_iter(Iter) returns the color table index for a given iteration
color_for_iter(Iter) ->
  Iter + 2.

%% @proc coord_list(X,Y) -> List with nice sortable name
coord_list(X,Y) ->
  Xint = integer_to_list(X),
  Yint = integer_to_list(Y),
  Xpad = pad_num(X),
  Ypad = pad_num(Y),
  lists:flatten([Ypad, Yint, Xpad, Xint]).

%% @proc pad_num(Int) -> a0{n}
pad_num(Int) -> 
  if
    Int > 9999 ->
      Pad = "a";
    Int > 999 ->
      Pad = "a0";
    Int > 99 ->
      Pad = "a00";
    Int > 9 ->
      Pad = "a000";
    true ->
      Pad = "a0000"
  end,
  Pad.


%% @proc ets_loop(TableId, Depth, Width, Height)
ets_loop(TableId, D, W, H) ->
  receive
    {X, Y, black} -> 
      L = coord_list(X,Y),
      Name = list_to_atom(L),
      ets:insert(TableId, {Name, black}),
      ets_loop(TableId, D, W, H);
    {X, Y, Color} ->
      L2 = coord_list(X,Y),
      Name2 = list_to_atom(L2),
      ets:insert(TableId, {Name2, Color}),
      ets_loop(TableId, D, W, H);
    {write} ->
      List = ets:tab2list(TableId),
      Pixels = lists:map(fun(X) -> get_pixel(X, D) end, List),
      bmp ! {data, D, W, H, concat_binary(Pixels)}
      %ets_loop(TableId, D, W, H)
  end.

%% @proc get_pixel(Pixel, Depth) -> Bin
get_pixel({_Pixel, Type}, Depth) ->
  Zero = 0,
  One = 1,
  case Type of
    black ->
      <<One:Depth>>;
    blank ->
      <<Zero:Depth>>;
    Color ->
      UseColor = Color rem math:pow(2,Depth),
      <<UseColor:Depth>>
  end.

%% @proc create_bmp() -> Bin
create_bmp() -> 
  receive
    {data, Depth, W, H, Pixel_data} -> 
      Filesize = bmp:filesize(Depth, W, H),
      Header = bmp:header(Filesize, Depth),
      Dib_header = bmp:dib_header(Depth, W, H),
      Color_table = bmp:color_table(Depth),
      Bmp = list_to_binary([Header, Dib_header, Color_table, Pixel_data]),
      file ! {write, Bmp}
      %create_bmp()
  end.

%% @proc create_file() -> File
create_file() ->
  receive
    {write, Data} -> 
      {ok, File} = file:open(?FILE_PATH, write),
      file:write(File, [Data]),
      file:close(File)
      %create_file()
  end.
