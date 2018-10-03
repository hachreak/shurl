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
%%% @doc Shurl API worker
%%% @end

-module(shurl_worker).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-behaviour(gen_server).
-behaviour(poolboy_worker).

%% Poolboy API
-export([
  start_link/1
]).

%% GenServer API
-export([
  code_change/3,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  init/1,
  terminate/2
]).

%% API exports
-export([
  count/1,
  delete/2,
  register/2,
  resolve/2,
  update/3
]).

-export_type([ctx/0]).

%%====================================================================
%% Behaviour callbacks
%%====================================================================

-type ctx() :: map().

-callback handle_init(map()) -> {ok, ctx()} | {error, term()}.

-callback handle_register(binary(), binary(), ctx()) ->
    {ok, {ctx(), binary()}} | {error, term()}.

-callback handle_resolve(binary(), ctx()) ->
    {ok, {ctx(), binary()}} | {error, term()}.

-callback handle_update(binary(), binary(), ctx()) ->
    {ok, ctx()} | {error, term()}.

-callback handle_delete(binary(), ctx()) -> {ok, ctx()} | {error, term()}.

-callback handle_drop(ctx()) -> {ok, ctx()} | {error, term()}.

-callback handle_count(ctx()) -> {ok, {ctx(), integer()}} | {error, term()}.

%%====================================================================
%% API functions
%%====================================================================

count(Pid) ->
  gen_server:call(Pid, count).

register(Url, Pid) ->
  gen_server:call(Pid, {register, Url}).

resolve(ShortUrl, Pid) ->
  gen_server:call(Pid, {resolve, ShortUrl}).

update(ShortUrl, Url, Pid) ->
  gen_server:cast(Pid, {update, ShortUrl, Url}).

delete(ShortUrl, Pid) ->
  gen_server:cast(Pid, {delete, ShortUrl}).

%% Poolboy Callbacks

start_link(Config) ->
  gen_server:start_link(?MODULE, Config, []).

%% GenServer Callbacks

init(Cfg) ->
  {ok, config(Cfg)}.

handle_call({register, Url}, _,
            #{backend := {Backend, BCtx},
              idx := {Idx, Count},
              prefix := Prefix}=Ctx) ->
  {ok, {BCtx2, Number}} = Backend:handle_count(BCtx),
  Id = Idx:new(Number, Count),
  ShortUrl = << Prefix/binary, Id/binary >>,
  call(Backend:handle_register(ShortUrl, Url, BCtx2), Ctx);
handle_call({resolve, ShortUrl}, _, #{backend := {Backend, BCtx}}=Ctx) ->
  call(Backend:handle_resolve(ShortUrl, BCtx), Ctx);
handle_call(count, _, #{backend := {Backend, BCtx}}=Ctx) ->
  call(Backend:handle_count(BCtx), Ctx).

handle_cast({update, ShortUrl, Url}, #{backend := {Backend, BCtx}}=Ctx) ->
  cast(Backend:handle_update(ShortUrl, Url, BCtx), Ctx);
handle_cast({delete, ShortUrl}, #{backend := {Backend, BCtx}}=Ctx) ->
  cast(Backend:handle_delete(ShortUrl, BCtx), Ctx).

handle_info({'DOWN', _Ref, process, _Pid, _Reason},
            #{backend := {Backend, BCtx}}=Ctx) ->
  cast(Backend:handle_drop(BCtx), Ctx).

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.

%%====================================================================
%% Internal functions
%%====================================================================

config(#{backend := {Backend, Config}}=Cfg) ->
  Ctx = Backend:handle_init(Config),
  #{
    backend => {Backend, Ctx},
    idx => maps:get(idx, Cfg, {shurl_key_hex, 3}),
    prefix => maps:get(prefix, Cfg, <<"">>)
  }.

call({ok, {BCtx, Value}}, #{backend := {Backend, _}}=Ctx) ->
  {reply, Value, Ctx#{backend => {Backend, BCtx}}};
call({ok, BCtx}, #{backend := {Backend, _}}=Ctx) ->
  {reply, ok, Ctx#{backend => {Backend, BCtx}}};
call({error, _}=Err, Ctx) -> {reply, Err, Ctx}.

cast({ok, BCtx}, #{backend := {Backend, _}}=Ctx) ->
  {noreply, Ctx#{backend => {Backend, BCtx}}};
cast({error, _}, Ctx) -> {noreply, Ctx}.
