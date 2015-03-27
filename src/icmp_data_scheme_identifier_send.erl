-module(icmp_data_scheme_identifier_send).

-export([init/1]).
-export([prepare_data/1]).
-export([log_echo_packet/2]).
-export([is_complete/1]).

-export([extract_id/1]).

-define(DEFAULT_PAYLOAD, "abc").

-include("icmp_data.hrl").

%%% Include for logging purposes
-include_lib("pkt/include/pkt.hrl").

-record(mod_state_identifier, {
  data,
  seq_num = 0
  %source_data
}).

init(Data) ->
  #mod_state_identifier{data = Data}.

%%% TODO: refactor into 1 function
prepare_data(#mod_state_identifier{data = [M | []]} = ModState) when is_integer(M) ->
  Id = M bsl 8,
  %{Id, ?DEFAULT_PAYLOAD, ModState#mod_state_identifier{data = [], source_data = [M]}};
  #mod_state_identifier{seq_num = SeqNum} = ModState,

  #send_prep{
    id      = Id,
    seq_num = SeqNum,
    payload = ?DEFAULT_PAYLOAD,
    %state   = ModState#mod_state_identifier{data = [], seq_num = SeqNum + 1, source_data = [M]}
    state   = ModState#mod_state_identifier{data = [], seq_num = SeqNum + 1}
  };

prepare_data(#mod_state_identifier{data = [M | [N | Rest]]} = ModState) ->
  Id = (M bsl 8) bor N,
  %{Id, ?DEFAULT_PAYLOAD, ModState#mod_state_identifier{data = Rest, source_data = [M, N]}}.
  #mod_state_identifier{seq_num = SeqNum} = ModState,

  #send_prep{
    id      = Id,
    seq_num = SeqNum,
    payload = ?DEFAULT_PAYLOAD,
    %state   = ModState#mod_state_identifier{data = Rest, seq_num = SeqNum + 1, source_data = [M, N]}
    state   = ModState#mod_state_identifier{data = Rest, seq_num = SeqNum + 1}
  }.

log_echo_packet(Icmp, _State) ->
  [IcmpHdr, Payload] = pkt:decapsulate(icmp, Icmp),
  Id = IcmpHdr#icmp.id,
  lager:info("Id: ~p (~p). Payload: ~p", [Id, extract_id(<<Id:16>>), Payload]),
  ok.

is_complete(#mod_state_identifier{data = []}) ->
  true;
is_complete(ModState) when is_record(ModState, mod_state_identifier) ->
  false.

extract_id(<<M:8, N:8>>) when N =:= 0 ->
  %S = [N || <<N>> <= <<Id:16>>],
  %case lists:last(S) of
  %  0 -> lists:sublist(S, length(S) - 1);
  %  _ -> S
  %end.
  [M];
extract_id(<<M:8, N:8>>) ->
  %[M | [N | []]].
  [M, N].

