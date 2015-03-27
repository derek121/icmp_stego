-module(icmp_data_scheme_payload_send).

-export([init/1]).
-export([prepare_data/1]).
-export([log_echo_packet/2]).
-export([is_complete/1]).

-define(MAX_LEN, 512).

-define(DEFAULT_ID, 1).

-include("icmp_data.hrl").

%%% Include for logging purposes
-include_lib("pkt/include/pkt.hrl").

%%% TODO: use binary for data, and for incoming?
-record(mod_state_payload, {
  data = "",
  id,
  seq_num = 0
}).

init(Data) ->
  #mod_state_payload{
    data = Data,

    %% id field is 16 bits, so use random between 0 and 2^16 - 1
    id = random:uniform(1 bsl 16) - 1
  }.

prepare_data(ModState) ->
  %Data = ModState#mod_state_payload.data,
  #mod_state_payload{data = Data, id = Id, seq_num = SeqNum} = ModState,

  %% TODO: refactor
  {ToSend, Rest} = case length(Data) > ?MAX_LEN of
                     true -> lists:split(?MAX_LEN, Data);
                     _    -> {Data, []}
                   end,


  %{?DEFAULT_ID, ToSend, ModState#mod_state_payload{data = Rest, seq_num = SeqNum + 1}}.
  #send_prep{
    id      = Id,
    seq_num = SeqNum,
    payload = ToSend,
    state   = ModState#mod_state_payload{data = Rest, seq_num = SeqNum + 1}
  }.


log_echo_packet(Icmp, _State) ->
  [IcmpHdr, Payload] = pkt:decapsulate(icmp, Icmp),
  Id = IcmpHdr#icmp.id,
  lager:info("Id: ~p. Payload: ~p", [Id, Payload]),
  ok.

is_complete(#mod_state_payload{data = []}) ->
  true;
is_complete(ModState) when is_record(ModState, mod_state_payload) ->
  false.

