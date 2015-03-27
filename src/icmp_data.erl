-module(icmp_data).

-export([start/0]).
-export([get_mod/2]).

start() ->
  application:ensure_all_started(?MODULE),
  random:seed(erlang:now()),

  %% TODO: starting here
  icmp_data_receive_server:start(),
  icmp_data_scheme_payload_len_receive_fsm:start().

%%% TODO: interface for this?
get_mod(send, payload)      -> icmp_data_scheme_payload_send;
get_mod(recv, payload)      -> icmp_data_scheme_payload_receive;

get_mod(send, identifier)   -> icmp_data_scheme_identifier_send;
get_mod(recv, identifier)   -> icmp_data_scheme_identifier_receive;

%get_mod(send, seq_num_bits) -> icmp_data_scheme_seq_num_bits_send;
%get_mod(recv, seq_num_bits) -> icmp_data_scheme_seq_num_bits_receive.

get_mod(send, payload_len) -> icmp_data_scheme_payload_len_send;
get_mod(recv, payload_len) -> icmp_data_scheme_payload_len_receive.


