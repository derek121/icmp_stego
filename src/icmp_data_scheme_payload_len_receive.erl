-module(icmp_data_scheme_payload_len_receive).

-export([process_packet/1]).

-include("icmp_data.hrl").
-include("icmp_data_scheme_seq_num_bits_receive.hrl").

-include_lib("pkt/include/pkt.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


process_packet(Ip) ->
  [_Ipv4Hdr, IcmpHdr, Payload] = pkt:decapsulate(ipv4, Ip),
  SeqNum = IcmpHdr#icmp.sequence,
  Bit = extract_bit(SeqNum, erlang:binary_to_list(Payload)),
  handle_data(Bit).

extract_bit(SeqNum, Payload) ->
  IsFirst = (SeqNum rem 8) =:= 0,
  Val = length(Payload) rem 2,
  #bit{val = Val, is_first = IsFirst}.

handle_data(Bit) ->
  icmp_data_scheme_payload_len_receive_fsm:bit(Bit).



%%%



-ifdef(TEST).

extract_bit_test() ->
  Expected = [
    {0, "abcd", #bit{is_first = true, val = 0}},
    {1, "abc", #bit{is_first = false, val = 1}},
    {2, "abcd", #bit{is_first = false, val = 0}},
    {3, "abc", #bit{is_first = false, val = 1}},
    {4, "abcd", #bit{is_first = false, val = 0}},
    {5, "abc", #bit{is_first = false, val = 1}},
    {6, "abcd", #bit{is_first = false, val = 0}},
    {7, "abc", #bit{is_first = false, val = 1}},
    {8, "abcd", #bit{is_first = true, val = 0}},
    {9, "abc", #bit{is_first = false, val = 1}},
    {10, "abcd", #bit{is_first = false, val = 0}},
    {11, "abc", #bit{is_first = false, val = 1}},
    {12, "abcd", #bit{is_first = false, val = 0}},
    {13, "abc", #bit{is_first = false, val = 1}},
    {14, "abcd", #bit{is_first = false, val = 0}},
    {15, "abc", #bit{is_first = false, val = 1}},
    {16, "abcd", #bit{is_first = true, val = 0}},
    {17, "abc", #bit{is_first = false, val = 1}}
  ],
  Fun = fun({SeqNum, Payload, ExpectedRec}) ->
    ?assertEqual(ExpectedRec, extract_bit(SeqNum, Payload))
  end,
  lists:map(Fun, Expected).

handle_data_ok_test() ->
  %% $a == 97 == 2#01100001
  %% $b == 98 == 2#01100010
  %% [1, 2, 4, 6, 8, 11, 13, 14] ++
  %% [20, 23, 24, 26, 28, 31, 33, 34],

  %% TODO: use eunit fixtures to start and stop the fsm for each test here?
  icmp_data_scheme_payload_len_receive_fsm:start(),

  ?assertEqual(incomplete, handle_data(extract_bit(0, "a"))),
  ?assertEqual(incomplete, handle_data(extract_bit(1, "ab"))),
  ?assertEqual(incomplete, handle_data(extract_bit(2, "ab"))),
  ?assertEqual(incomplete, handle_data(extract_bit(3, "ab"))),
  ?assertEqual(incomplete, handle_data(extract_bit(4, "ab"))),
  ?assertEqual(incomplete, handle_data(extract_bit(5, "a"))),
  ?assertEqual(incomplete, handle_data(extract_bit(6, "a"))),
  ?assertEqual({complete, "a"}, handle_data(extract_bit(7, "ab"))),

  ?assertEqual(incomplete, handle_data(extract_bit(8, "ab"))),
  ?assertEqual(incomplete, handle_data(extract_bit(9, "a"))),
  ?assertEqual(incomplete, handle_data(extract_bit(10, "ab"))),
  ?assertEqual(incomplete, handle_data(extract_bit(11, "ab"))),
  ?assertEqual(incomplete, handle_data(extract_bit(12, "ab"))),
  ?assertEqual(incomplete, handle_data(extract_bit(13, "a"))),
  ?assertEqual(incomplete, handle_data(extract_bit(14, "a"))),
  ?assertEqual({complete, "b"}, handle_data(extract_bit(15, "ab"))),
  ok.

handle_data_exp_first_rec_non_test() ->
  %% $a == 97 == 2#01100001
  %% $b == 98 == 2#01100010
  %% [1, 2, 4, 6, 8, 11, 13, 14] ++
  %% [20, 23, 24, 26, 28, 31, 33, 34],

  ?assertEqual(incomplete, handle_data(extract_bit(0, "a"))),
  ?assertEqual(incomplete, handle_data(extract_bit(1, "ab"))),
  ?assertEqual(incomplete, handle_data(extract_bit(2, "ab"))),
  ?assertEqual(incomplete, handle_data(extract_bit(3, "ab"))),
  ?assertEqual(incomplete, handle_data(extract_bit(4, "ab"))),
  ?assertEqual(incomplete, handle_data(extract_bit(5, "a"))),
  ?assertEqual(incomplete, handle_data(extract_bit(6, "a"))),
  ?assertEqual({complete, "a"}, handle_data(extract_bit(7, "ab"))),

  %% Now send a non-first
  ?assertEqual(incomplete, handle_data(extract_bit(10, "a"))),

  %% Now go ahead with the next byte
  ?assertEqual(incomplete, handle_data(extract_bit(16, "ab"))),
  ?assertEqual(incomplete, handle_data(extract_bit(17, "a"))),
  ?assertEqual(incomplete, handle_data(extract_bit(18, "ab"))),
  ?assertEqual(incomplete, handle_data(extract_bit(19, "ab"))),
  ?assertEqual(incomplete, handle_data(extract_bit(20, "ab"))),
  ?assertEqual(incomplete, handle_data(extract_bit(21, "a"))),
  ?assertEqual(incomplete, handle_data(extract_bit(22, "a"))),
  ?assertEqual({complete, "b"}, handle_data(extract_bit(23, "ab"))),
  ok.

handle_data_exp_non_rec_first_test() ->
  %% $a == 97 == 2#01100001
  %% $b == 98 == 2#01100010
  %% [1, 2, 4, 6, 8, 11, 13, 14] ++
  %% [20, 23, 24, 26, 28, 31, 33, 34],

  ?assertEqual(incomplete, handle_data(extract_bit(0, "a"))),
  ?assertEqual(incomplete, handle_data(extract_bit(1, "ab"))),
  ?assertEqual(incomplete, handle_data(extract_bit(2, "ab"))),
  ?assertEqual(incomplete, handle_data(extract_bit(3, "ab"))),
  ?assertEqual(incomplete, handle_data(extract_bit(4, "ab"))),
  ?assertEqual(incomplete, handle_data(extract_bit(5, "a"))),
  ?assertEqual(incomplete, handle_data(extract_bit(6, "a"))),
  ?assertEqual({complete, "a"}, handle_data(extract_bit(7, "ab"))),

  %% Go ahead with the next byte
  ?assertEqual(incomplete, handle_data(extract_bit(8, "ab"))),
  ?assertEqual(incomplete, handle_data(extract_bit(9, "a"))),

  %% Now send a first bit, from the first sent above
  ?assertEqual(incomplete, handle_data(extract_bit(16, "ab"))),
  ?assertEqual(incomplete, handle_data(extract_bit(17, "a"))),
  ?assertEqual(incomplete, handle_data(extract_bit(18, "ab"))),
  ?assertEqual(incomplete, handle_data(extract_bit(19, "ab"))),
  ?assertEqual(incomplete, handle_data(extract_bit(20, "ab"))),
  ?assertEqual(incomplete, handle_data(extract_bit(21, "a"))),
  ?assertEqual(incomplete, handle_data(extract_bit(22, "a"))),
  ?assertEqual({complete, "b"}, handle_data(extract_bit(23, "ab"))),
  ok.


-endif.

