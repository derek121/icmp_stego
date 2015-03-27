-module(icmp_data_scheme_payload_receive).

%-export([init_receive/0]).
-export([process_packet/1]).

%init_receive() ->
%  undefined.

process_packet(Ip) ->
  [_Ipv4Hdr, _IcmpHdr, Payload] = pkt:decapsulate(ipv4, Ip),
  %io:format("~s~n", [Payload]),
  {complete, erlang:binary_to_list(Payload)}.
