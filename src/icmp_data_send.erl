-module(icmp_data_send).

-export([send/1]).
-export([send/2]).
-export([send/3]).

%%% TODO: via config option
-define(DELAY_MS, 1000).

-include("icmp_data.hrl").

%%% Include for logging purposes
-include_lib("pkt/include/pkt.hrl").

send(Host) ->
  send(Host, payload).

send(Host, Scheme) ->
  send(Host, Scheme, lists:seq($A, $C)).

send(Host, Scheme, Data) ->
  {ok, Socket} = gen_icmp:open(),

  Mod = icmp_data:get_mod(send, Scheme),
  ModState = Mod:init(Data),

  send(Host, Mod, ModState, Socket),
  ok = gen_icmp:close(Socket).

send(Host, Mod, ModState, Socket) ->
  %{Identifier, PayloadPart, ModState2} = Mod:prepare_data(ModState),
  #send_prep{id = Id, seq_num = SeqNum, payload = PayloadRaw, state = ModState2}
    = Mod:prepare_data(ModState),

  {EchoPacket, Payload} = create_packet(Id, SeqNum, PayloadRaw),
  Mod:log_echo_packet(EchoPacket, ModState2),

  ok = gen_icmp:send(Socket, Host, EchoPacket),

  {ok, {_Addr, Packet}} = gen_icmp:recv(Socket, 500),
  validate_response(Payload, Packet),

  case Mod:is_complete(ModState2) of
    true ->
      ok;
    _ ->
      delay(?DELAY_MS),
      send(Host, Mod, ModState2, Socket)
  end.

create_packet(Identifier, SeqNum, PayloadBase) ->
  Payload = list_to_binary(PayloadBase),
  Echo = gen_icmp:echo(inet, Identifier, SeqNum, Payload),
  {Echo, Payload}.

validate_response(PayloadOut, PacketIn) ->
  [_Ipv4Hdr, _IcmpHdr, PayloadIn] = pkt:decapsulate(ipv4, PacketIn),
  %lager:info("Response IcmpHdr: ~p", [lager:pr(IcmpHdr, ?MODULE)]),

  case PayloadIn of
    PayloadOut -> ok;
    _ -> lager:error("Response payload doesn't match sent payload. Sent: ~p. Received: ~p",
      [PayloadOut, PayloadIn])
  end,
  ok.

delay(Ms) ->
  timer:sleep(Ms).


