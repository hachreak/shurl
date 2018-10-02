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

-module(shurl_mongopool).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-behaviour(shurl).

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

-type ctx() :: shurl:ctx().

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
  Table = maps:get(table, Config, ?MODULE),
  Pool = maps:get(pool, Config),
  #{table => Table, pool => Pool}.

handle_register(ShortUrl, Url, #{table := Table, pool := Pool}=Ctx) ->
  mongopool_app:insert(Pool, Table, #{
    <<"_id">> => ShortUrl,
    <<"url">> => Url
  }),
  {ok, {Ctx, ShortUrl}}.

handle_resolve(ShortUrl, #{table := Table, pool := Pool}=Ctx) ->
  case mongopool_app:find_one(Pool, Table, #{<<"_id">> => ShortUrl}) of
    #{<<"_id">> := ShortUrl, <<"url">> := Url} -> {ok, {Ctx, Url}};
    _Rest -> {error, notfound}
  end.

handle_update(ShortUrl, Url, #{table := Table, pool := Pool}=Ctx) ->
  mongopool_app:update(
    Pool, Table, #{<<"_id">> => ShortUrl}, {<<"$set">>, #{
      <<"_id">> => ShortUrl,
      <<"url">> => Url
    }}),
  {ok, Ctx}.

handle_delete(ShortUrl, #{table := Table, pool := Pool}=Ctx) ->
  mongopool_app:delete(Pool, Table, #{<<"_id">> => ShortUrl}),
  {ok, Ctx}.

handle_drop(#{table := Table, pool := Pool}=Ctx) ->
  mongopool_app:delete(Pool, Table, #{}),
  {ok, Ctx}.

handle_count(#{table := Table, pool := Pool}=Ctx) ->
  Count = mongopool_app:count(Pool, Table, #{}),
  {ok, {Ctx, Count}}.
