-module(icmp_data_receive).

-export([recv/3]).

%%% Include for logging purposes
-include_lib("pkt/include/pkt.hrl").

recv(Mod, Socket, Timeout) ->
  case gen_icmp:recv(Socket, 500, Timeout) of
    {ok, {_Addr, Packet}} ->
      %lager:info("Addr: ~p. Packet: ~p", [Addr, Packet]),
      %log_recv_packet("Recv", Addr, Packet),
      Mod:process_packet(Packet);

    {error, timeout} ->
      %lager:info("Timeout in recv"),
      timeout;
    {error, Reason} ->
      lager:error("Error in recv: ~p", [Reason]),
      error;
    Any ->
      lager:error("Error response in recv: ~p", [Any]),
      error
  end.

%%% TODO: validate that it's a packet sent by us

%log_recv_packet(Msg, Addr, Ip) ->
%  lager:info("Recv. ~p. From ~p", [Msg, Addr]),
%
%  [Ipv4Hdr, IcmpHdr, Payload] = pkt:decapsulate(ipv4, Ip),
%  lager:info("Ipv4Hdr: ~p", [lager:pr(Ipv4Hdr, ?MODULE)]),
%  lager:info("IcmpHdr: ~p", [lager:pr(IcmpHdr, ?MODULE)]),
%  lager:info("Payload: ~p", [Payload]),
%  ok.

