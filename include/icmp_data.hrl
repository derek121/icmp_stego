-record(send_prep, {
  id,
  seq_num,
  payload,
  state
}).

-define(SEQ_NUM_INC_PER_BYTE, 16).

