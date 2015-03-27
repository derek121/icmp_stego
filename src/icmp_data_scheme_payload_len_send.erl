-module(icmp_data_scheme_payload_len_send).

-export([init/1]).
-export([prepare_data/1]).
-export([log_echo_packet/2]).
-export([is_complete/1]).

-define(PAYLOAD_MIN_CHAR, $!).
-define(PAYLOAD_MAX_CHAR, $~ - 1). % TODO: why the -1

-define(PAYLOAD_MIN_LEN, 2).
-define(PAYLOAD_MAX_LEN_BASE, 4).

-include("icmp_data.hrl").

%%% Include for logging purposes
-include_lib("pkt/include/pkt.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%% TODO: use binary for data, and for incoming?
-record(mod_state_payload_len_send, {
  id,
  seq_num = 0,
  current,
  rest,
  bit_num = 0,
  byte_num = 0
  %seq_num_base_for_bit = 0,
  %seq_num_start_for_byte = 0
}).

%%% For each byte of the message, each bit is used to determine the id to use in a packet.
%%% Each byte has a starting id that will be used, and a base id for a given bit.
%%% For each bit, if it's 0, the base id is used. Else, the base id plus 1 is used.
%%% If there are bits left in the byte, the base id is then incremented by 2.
%%% Else, the base id and the starting id are set to the previous starting id plus
%%% ?SEQ_NUM_INC_PER_BYTE, and the next byte is processed.

init([H | T]) ->
  #mod_state_payload_len_send{
    id = random:uniform(1 bsl 16) - 1,
    current = H,
    rest = T
  }.

prepare_data(ModState) ->
  #mod_state_payload_len_send{
    id = Id,
    seq_num = SeqNum,
    current = Current
    } = ModState,

  Bit = Current band 1,
  Payload = create_payload(Bit),
  ModState2 = next_state(ModState),

  #send_prep{
    id      = Id,
    seq_num = SeqNum,
    payload = Payload,
    state   = ModState2#mod_state_payload_len_send{seq_num = SeqNum + 1}
  }.

%%% More bits left in Current
next_state(
    #mod_state_payload_len_send{
      bit_num = BitNum} = ModState) when BitNum < 7 ->
  ModState#mod_state_payload_len_send{
    current = ModState#mod_state_payload_len_send.current bsr 1,
    bit_num = BitNum + 1
  };

%%% Need to advance to next byte
next_state(
    #mod_state_payload_len_send{
      rest = [H | T],
      bit_num = BitNum} = ModState) when BitNum =:= 7 ->
  ModState#mod_state_payload_len_send{
    current = H,
    rest = T,
    bit_num = 0,
    byte_num = ModState#mod_state_payload_len_send.byte_num + 1
  };

%%% Data is complete
next_state(
    #mod_state_payload_len_send{
      rest = [],
      bit_num = BitNum} = ModState) when BitNum =:= 7 ->
  ModState#mod_state_payload_len_send{
    current = undefined
  }.

create_payload(Bit) ->
  [create_payload_char() || _N <- lists:seq(1, calc_payload_len(Bit))].

create_payload_char() ->
  random_between(?PAYLOAD_MIN_CHAR, ?PAYLOAD_MAX_CHAR).

random_between(Min, Max) ->
  RangeSize = Max - Min + 1,
  random:uniform(RangeSize) - 1 + Min.

calc_payload_len(Bit) ->
  BaseLen = random_between(?PAYLOAD_MIN_LEN, ?PAYLOAD_MAX_LEN_BASE),
  possibly_pad_payload_len(BaseLen, Bit).

possibly_pad_payload_len(BaseLen, Bit) when Bit =:= 0, BaseLen rem 2 =:= 0 -> BaseLen;
possibly_pad_payload_len(BaseLen, Bit) when Bit =:= 0, BaseLen rem 2 =:= 1 -> BaseLen + 1;
possibly_pad_payload_len(BaseLen, Bit) when Bit =:= 1, BaseLen rem 2 =:= 0 -> BaseLen + 1;
possibly_pad_payload_len(BaseLen, Bit) when Bit =:= 1, BaseLen rem 2 =:= 1 -> BaseLen.

log_echo_packet(
    Icmp,
    #mod_state_payload_len_send{
      bit_num = BitNum,
      byte_num = ByteNum}) ->
  [IcmpHdr, Payload] = pkt:decapsulate(icmp, Icmp),
  %Id = IcmpHdr#icmp.id,
  SeqNum = IcmpHdr#icmp.sequence,
  lager:info("Byte ~p, bit ~p. Packet seqnum: ~p. Payload: ~p",
    [ByteNum, BitNum, SeqNum, Payload]),
  ok.

is_complete(#mod_state_payload_len_send{current = undefined}) ->
  true;
is_complete(ModState) when is_record(ModState, mod_state_payload_len_send) ->
  false.




%%%


-ifdef(TEST).

create_payload_test() ->
  [create_payload_test2(0) || _N <- lists:seq(1, 20)],
  [create_payload_test2(1) || _N <- lists:seq(1, 20)],
  ok.

create_payload_test2(Bit) ->
  ?assert(length(create_payload(Bit)) rem 2 =:= Bit),
  ok.

%%% 0 1 / 2 3 / 4 5 / 6 7 / 8 9
prepare_data_test() ->
  io:format("prepare_data_test~n"),
  %% $a == 97 == 2#01100001
  %% $b == 98 == 2#01100010
  State = icmp_data_scheme_payload_len_send:init("ab"),

  %ExpectedSeqNums =
  %  [1, 2, 4, 6, 8, 11, 13, 14] ++
  %  [16, 19, 20, 22, 24, 27, 29, 30],
  ExpectedPayloadLenParities =
    [odd, even, even, even, even, odd, odd, even] ++
    [even, odd, even, even, even, odd, odd, even],

  test_data(State, ExpectedPayloadLenParities),
  ok.

test_data(_State, []) ->
  ok;
test_data(State, [ExpectedPayloadLenParity | T]) ->
  #send_prep{payload = Payload, state = State2} = icmp_data_scheme_payload_len_send:prepare_data(State),
  %?debugFmt("Expected parity: ~p. Payload: ~p", [ExpectedPayloadLenParity, Payload]),
  ?assertEqual(ExpectedPayloadLenParity, calc_len_parity(Payload)),

  ExpectedIsComplete = T =:= [],
  ?assertEqual(ExpectedIsComplete, icmp_data_scheme_payload_len_send:is_complete(State2)),

  test_data(State2, T).

calc_len_parity(L) when length(L) rem 2 =:= 0 -> even;
calc_len_parity(L) when length(L) rem 2 =:= 1 -> odd.


-endif.

