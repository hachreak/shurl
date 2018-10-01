%%% @author Leonardo Rossi <leonardo.rossi@studenti.unipr.it>
%%% @copyright (C) 2018 Leonardo Rossi
%%%
%%% This software is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This software is distributed in the hope that it will be useful, but
%%% WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this software; if not, write to the Free Software Foundation,
%%% Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
%%%
%%% @doc Shurl key generator - hexadecimal
%%% @end

-module(shurl_key_hex).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-export([new/1, new/2]).

new(Number) ->
  httpd_util:integer_to_hexlist(Number).

new(Number, Count) ->
  Hex = httpd_util:integer_to_hexlist(Number),
  Pad = lists:duplicate(Count - length(Hex), "0"),
  list_to_binary(Pad ++ Hex).
