-module(icmp_data_scheme_identifier_receive).

%-export([init_receive/0]).
-export([process_packet/1]).

-include_lib("pkt/include/pkt.hrl").

%init_receive() ->
%  undefined.

process_packet(Ip) ->
  [_Ipv4Hdr, IcmpHdr, _Payload] = pkt:decapsulate(ipv4, Ip),
  Id = IcmpHdr#icmp.id,
  Data = icmp_data_scheme_identifier_send:extract_id(<<Id:16>>),
  %io:format("~s~n", [Data]),
  {complete, Data}.


