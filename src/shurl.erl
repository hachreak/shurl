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
%%% @doc Shurl API
%%% @end

-module(shurl).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

%% API exports
-export([
  delete/1,
  init/1,
  register/1,
  resolve/1,
  update/2
]).

%%====================================================================
%% Behaviour callbacks
%%====================================================================

-callback handle_init(map()) -> {ok, map()} | {error, term()}.

-callback handle_register(binary(), binary()) -> {ok, map()} | {error, term()}.

-callback handle_update(binary(), binary()) -> {ok, map()} | {error, term()}.

-callback handle_delete(binary()) -> {ok, map()} | {error, term()}.

%%====================================================================
%% API functions
%%====================================================================

init(Config) ->
  ok.

register(Url) ->
  ok.

resolve(ShortUrl) ->
  ok.

update(ShortUrl, Url) ->
  ok.

delete(ShortUrl) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
