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
  count/1,
  delete/2,
  register/2,
  resolve/2,
  revert/2,
  update/3
]).

%%====================================================================
%% API functions
%%====================================================================

count(PoolName) ->
  poolboy:transaction(PoolName, fun(Worker) ->
    gen_server:call(Worker, count)
  end).

register(Url, PoolName) ->
  poolboy:transaction(PoolName, fun(Worker) ->
    gen_server:call(Worker, {register, Url})
  end).

resolve(ShortUrl, PoolName) ->
  poolboy:transaction(PoolName, fun(Worker) ->
    gen_server:call(Worker, {resolve, ShortUrl})
  end).

revert(Url, PoolName) ->
  poolboy:transaction(PoolName, fun(Worker) ->
    gen_server:call(Worker, {revert, Url})
  end).

update(ShortUrl, Url, PoolName) ->
  poolboy:transaction(PoolName, fun(Worker) ->
    gen_server:cast(Worker, {update, ShortUrl, Url})
  end).

delete(ShortUrl, PoolName) ->
  poolboy:transaction(PoolName, fun(Worker) ->
    gen_server:cast(Worker, {delete, ShortUrl})
  end).
