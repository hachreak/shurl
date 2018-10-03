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

-module(shurl_ets).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-behaviour(shurl_worker).

-include_lib("stdlib/include/ms_transform.hrl").

-export([
  handle_count/1,
  handle_delete/2,
  handle_drop/1,
  handle_init/1,
  handle_register/3,
  handle_resolve/2,
  handle_update/3
]).

%% API

-type ctx() :: shurl_worker:ctx().

-spec handle_init(map()) -> {ok, ctx()} | {error, term()}.

-spec handle_register(binary(), binary(), ctx()) ->
    {ok, {ctx(), binary()}} | {error, term()}.

-spec handle_resolve(binary(), ctx()) ->
    {ok, {ctx(), binary()}} | {error, term()}.

-spec handle_update(binary(), binary(), ctx()) ->
    {ok, ctx()} | {error, term()}.

-spec handle_delete(binary(), ctx()) -> {ok, ctx()} | {error, term()}.

-spec handle_drop(ctx()) -> {ok, ctx()} | {error, term()}.

-spec handle_count(ctx()) -> {ok, {ctx(), integer()}} | {error, term()}.

%% Implementation

handle_init(Config) ->
  Name = maps:get(name, Config, ?MODULE),
  Index = ets:new(Name, [
    set, private, {keypos, 1},
    {read_concurrency, true},
    {write_concurrency, false}
  ]),
  #{index => Index}.

handle_register(ShortUrl, Url, #{index := Index}=Ctx) ->
  case ets:insert(Index, {ShortUrl, Url}) of
    true -> {ok, {Ctx, ShortUrl}};
    false -> {error, create_error}
  end.

handle_resolve(ShortUrl, #{index := Index}=Ctx) ->
  case ets:lookup(Index, ShortUrl) of
    [] -> {error, not_found};
    [{_, Url}] -> {ok, {Ctx, Url}}
  end.

handle_update(ShortUrl, Url, #{index := Index}=Ctx) ->
  case ets:update_element(Index, ShortUrl, [{2, Url}]) of
    false -> {error, update_error};
    true -> {ok, Ctx}
  end.

handle_delete(ShortUrl, #{index := Index}=Ctx) ->
  ets:delete(Index, ShortUrl),
  {ok, Ctx}.

handle_drop(#{index := Index}=Ctx) ->
  ets:delete(Index),
  {ok, Ctx}.

handle_count(#{index := Index}=Ctx) ->
  Count = ets:select_count(Index, ets:fun2ms(fun(_X) -> true end)),
  {ok, {Ctx, Count}}.
