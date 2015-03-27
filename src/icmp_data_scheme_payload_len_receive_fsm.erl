-module(icmp_data_scheme_payload_len_receive_fsm).

-behaviour(gen_fsm).

%-export([start_link/1]).
-export([start/0]).
-export([init/1]).

-export([bit/1]).

-export([await_low_bit/3]).
-export([await_higher_bit/3]).

%-export([get_state/0]).
%-export([stop/0]).

-export([handle_event/3,
  handle_sync_event/4,
  handle_info/3,
  terminate/3,
  code_change/4]).

-include("icmp_data_scheme_seq_num_bits_receive.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(mod_state_seq_num_bits_receive_fsm, {
  current = 0,
  bit_num = 0
}).

%start_link(Code) ->
%  gen_fsm:start_link({local, ?MODULE}, ?MODULE, lists:reverse(Code), []).

start() ->
  gen_fsm:start({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
  {ok, await_low_bit, #mod_state_seq_num_bits_receive_fsm{}}.

bit(Bit) when is_record(Bit, bit) ->
  lager:debug("bit: ~p", [Bit]),
  gen_fsm:sync_send_event(?MODULE, Bit).

%%% Expecting first bit. Received first bit.
await_low_bit(#bit{is_first = true} = Bit, _From, _State) ->
  handle_expected_low_bit(Bit);

%%% Expecting first bit. Received non-first bit.
%%% Ignore bit (thus waiting for the next first bit).
await_low_bit(#bit{is_first = false}, _From, _State) ->
  handle_unexpected_low_bit().

%%% Expecting non-first bit, received non-first bit
await_higher_bit(#bit{is_first = false} = Bit, _From, State) ->
  handle_expected_higher_bit(Bit, State);

%%% Expecting non-first bit, received first bit
%%% Clear state and handle as if fsm state had been await_low_bit
await_higher_bit(#bit{is_first = true} = Bit, From, _State) ->
  handle_unexpected_higher_bit(Bit, From).

%%%
handle_expected_low_bit(Bit) ->
  {Reply, NextFsmState, State2} = process_bit(0, Bit#bit.val),
  {reply, Reply, NextFsmState, State2}.

handle_unexpected_low_bit() ->
  {Reply, NextFsmState, State2} = reset(),
  {reply, Reply, NextFsmState, State2}.

handle_expected_higher_bit(Bit, State) ->
  Current = apply_bit(Bit, State),
  #mod_state_seq_num_bits_receive_fsm{bit_num = BitNum} = State,
  {Reply, NextFsmState, State2} = process_bit(BitNum, Current),
  {reply, Reply, NextFsmState, State2}.

handle_unexpected_higher_bit(Bit, From) ->
  await_low_bit(Bit, From, #mod_state_seq_num_bits_receive_fsm{}).

%%% If the new high-order bit is 0, no change is needed for Current
apply_bit(
    #bit{val = BitVal},
    #mod_state_seq_num_bits_receive_fsm{
      current = Current}) when BitVal =:= 0 ->
  Current;

%%% If the new high-order bit is 1, apply it to Current
apply_bit(
    #bit{val = BitVal},
    #mod_state_seq_num_bits_receive_fsm{
      current = Current,
      bit_num = BitNum}) when BitVal =:= 1 ->
  Current bor (1 bsl BitNum).

%%%
reset() ->
  Reply = incomplete,
  NextFsmState = await_low_bit,
  State = #mod_state_seq_num_bits_receive_fsm{},
  {Reply, NextFsmState, State}.

%%%
process_bit(BitNum, Current) when BitNum < 7 ->
  Reply = incomplete,
  NextFsmState = await_higher_bit,
  State = #mod_state_seq_num_bits_receive_fsm{
    bit_num = BitNum + 1,
    current = Current
  },
  {Reply, NextFsmState, State};

process_bit(BitNum, Current) when BitNum =:= 7 ->
  Reply = {complete, [Current]},
  NextFsmState = await_low_bit,
  State = #mod_state_seq_num_bits_receive_fsm{},
  {Reply, NextFsmState, State}.

%%%
%get_state() ->
%  gen_fsm:sync_send_all_state_event(code_lock, get_state).

%stop() ->
%  gen_fsm:sync_send_all_state_event(code_lock, stop).

handle_event(Event, StateName, State) ->
  lager:info("handle_event. Event: ~p. StateName: ~p. State: ~p",
    [Event, StateName, State]),
  {next_state, StateName, State}.

%handle_sync_event(get_state, _From, StateName, State) ->
%  io:format("handle_sync_event. StateName: ~p. State: ~p~n", [StateName, State]),
%  {reply, {current_state, StateName, State}, StateName, State};

%handle_sync_event(stop, _From, StateName, State) ->
%  io:format("handle_sync_event. StateName: ~p. State: ~p~n", [StateName, State]),
%  {stop, stopped, State};

handle_sync_event(Event, _From, StateName, State) ->
  lager:info("handle_sync_event. Event: ~p. StateName: ~p. State: ~p",
    [Event, StateName, State]),
  Reply = ok,
  {reply, Reply, StateName, State}.

handle_info(Info, StateName, State) ->
  lager:info("handle_info. Info: ~p. StateName: ~p. State: ~p",
    [Info, StateName, State]),
  {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.


%%%


-ifdef(TEST).




%await_low_bit_test() ->
%  Bit = #bit{is_first = true, val = 1},
%  {reply, Reply, NextFsmState, _State} = await_low_bit(Bit, undefined, undefined),
%
%  ExpectedReply = {incomplete, no_state},
%  ExpectedNextFsmState = await_higher_bit,
%
%  ?assertEqual(ExpectedReply, Reply),
%  ?assertEqual(ExpectedNextFsmState, NextFsmState),
%
%  ok.


-endif.

