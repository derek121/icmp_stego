-module(icmp_data_receive_server).

-behaviour(gen_server).

-export([start/0]).

-export([test/0]).

-export([start_receive/1]).
-export([stop_receive/0]).
-export([get_data/0]).
-export([clear_data/0]).

%%% Only export to silence unused function warning
-export([do_start_receive/1]).
-export([data_received/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(TIMEOUT, 3000).

-record(state, {
  %socket
  child,
  data = ""
}).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
  gen_server:start({local, ?SERVER}, ?MODULE, [], []).

test() ->
  gen_server:call(?SERVER, test).

start_receive(payload) ->
  gen_server:cast(?SERVER, {start_receive, payload});

start_receive(identifier) ->
  gen_server:cast(?SERVER, {start_receive, identifier});

%start_receive(seq_num_bits) ->
%  gen_server:cast(?SERVER, {start_receive, seq_num_bits}).

start_receive(payload_len) ->
  gen_server:cast(?SERVER, {start_receive, payload_len}).


stop_receive() ->
  gen_server:cast(?SERVER, stop_receive).

get_data() ->
  gen_server:call(?SERVER, get_data).

clear_data() ->
  gen_server:cast(?SERVER, clear_data).

%%% Internal
data_received(Data) ->
  gen_server:cast(?SERVER, {data_received, Data}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  {ok, #state{}}.

%%%
handle_call(test, _From, State) ->
  {reply, ok, State};

handle_call(get_data, _From, State) ->
  Data = State#state.data,
  {reply, {data, Data}, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%%
%%% TODO: don't start if already running
handle_cast({start_receive, Scheme}, State) ->
  Pid = spawn(?MODULE, do_start_receive, [Scheme]),
  {noreply, State#state{child = Pid}};

handle_cast(stop_receive, State) ->
  Child = State#state.child,
  Child ! stop,
  {noreply, State#state{child = undefined}};

handle_cast({data_received, DataIn}, State) ->
  lager:info("~p", [DataIn]),
  NewData = State#state.data ++ DataIn,
  {noreply, State#state{data = NewData}};

handle_cast(clear_data, State) ->
  {noreply, State#state{data = ""}};

handle_cast(_Request, State) ->
  {noreply, State}.

%%%
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_start_receive(Scheme) ->
  %lager:info("Receive fun waiting to begin..."),
  %receive
  %  ready -> ok
  %end,
  Mod = icmp_data:get_mod(recv, Scheme),
  %ModState = Mod:init_receive(),

  {ok, Socket} = gen_icmp:open(),

  do_receive(Mod, Socket).

do_receive(Mod, Socket) ->
  Resp = icmp_data_receive:recv(Mod, Socket, ?TIMEOUT),
  {continue, Continue} = handle_resp(Resp),

  case Continue of
    true ->
      do_receive(Mod, Socket);
    _ ->
      do_close(Socket),
      stopped
  end.

handle_resp({complete, Data}) ->
  %Processed = process_data(Data, Scheme),
  Processed = Data,
  ?SERVER:data_received(Processed),
  {continue, not should_stop()};

handle_resp(incomplete) ->
  {continue, not should_stop()};

handle_resp(timeout) ->
  {continue, not should_stop()};

handle_resp({error, State}) ->
  {continue, false, State}.

%process_data(Data, payload) ->
%  Data;
%process_data(Data, identifier) ->
%  case lists:last(Data) of
%    0 -> lists:sublist(Data, length(Data) - 1);
%    _ -> Data
%  end.

should_stop() ->
  receive
    stop -> true
  after 0 -> false
  end.

do_close(Socket) ->
  ok = gen_icmp:close(Socket),
  lager:info("Closed socket"),
  ok.

