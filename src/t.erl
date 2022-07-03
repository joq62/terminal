%%% -------------------------------------------------------------------
%%% Author  : joqerlang
%%% Description :
%%% load,start,stop unload applications in the pods vm
%%% supports with services
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(t).  

-behaviour(gen_server).  

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("kernel/include/logger.hrl").

%% --------------------------------------------------------------------
-define(SERVER,?MODULE).



%% External exports
-export([

	 host_status/0,
	 appl_status/0,
	 
	 read_state/0,

	 appl_start/1,
	 ping/0
	]).


-export([
	 start/0,
	 stop/0
	]).


-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
		clusterid,
		hosts,
		k3_nodes_status,
		appl_status,
		applications,
		num_instances,
		start_time=undefined
	       }).

%% ====================================================================
%% External functions
%% ====================================================================
appl_start([DeploymentName])->
    io:format("DeploymentName ~p~n",[DeploymentName]),
    application:load(termnal),
    application:set_env([{terminal,[{deployment_name,DeploymentName}]}]),
    application:start(terminal).

%% ====================================================================
%% Server functions
%% ====================================================================
%% Gen server functions

start()-> gen_server:start_link({local, ?SERVER}, ?SERVER, [], []).
stop()-> gen_server:call(?SERVER, {stop},infinity).

%% ====================================================================
%% Application handling
%% ====================================================================


%% ====================================================================
%% Support functions
%
%%---------------------------------------------------------------
%% Function:host_status()
%% @doc: read the which hosts are running and not running      
%% @param: non 
%% @returns: Lists of running and missing nodes
%%
%%---------------------------------------------------------------
-spec host_status()-> term().
host_status()->
    gen_server:call(?SERVER, {host_status},infinity).

appl_status()->
    gen_server:call(?SERVER, {appl_status},infinity).

%% ====================================================================
%% Support functions
%
%%---------------------------------------------------------------
%% Function:read_state()
%% @doc: read theServer State variable      
%% @param: non 
%% @returns:State
%%
%%---------------------------------------------------------------
-spec read_state()-> term().
read_state()->
    gen_server:call(?SERVER, {read_state},infinity).
%% 
%% @doc:check if service is running
%% @param: non
%% @returns:{pong,node,module}|{badrpc,Reason}
%%
-spec ping()-> {atom(),node(),module()}|{atom(),term()}.
ping()-> 
    gen_server:call(?SERVER, {ping},infinity).

%% ====================================================================
%% Gen Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
 %% Start needed applications   

    ok=application:start(sd),
    ok=application:start(config),

    ok=etcd:appl_start([]),
    pong=etcd:ping(), 
    ok=etcd:dynamic_db_init([]),
    ok=db_host_spec:init_table(),
    ok=db_application_spec:init_table(),
    ok=db_deployment_info:init_table(),
    ok=db_deployments:init_table(),
    {ok,DeploymentName}=application:get_env(terminal,deployment_name),
    {ok,ClusterId}=db_deployments:read(name,DeploymentName),
    {ok,CookieStr}=db_deployments:read(cookie,DeploymentName),
    {ok,Hosts}=db_deployments:read(hosts,DeploymentName),
    {ok,DeploymentInfoSpecs}=db_deployments:read(deployments,DeploymentName),

 %   io:format("DEBUG: DeploymentInfoSpecs ~p~n",[DeploymentInfoSpecs]),
    [DeploymentInfoSpec|_]=DeploymentInfoSpecs,
    {ok,Applications}=db_deployment_info:read(appl_specs,DeploymentInfoSpec),
    {ok,NumInstances}=db_deployment_info:read(num_instances,DeploymentInfoSpec),
    true=erlang:set_cookie(node(),list_to_atom(CookieStr)),

    K3NodesStatus=t_lib:connect_hosts(ClusterId,Hosts),
    [{_HostName,K3Node}|_]=[{HostName,K3Node}||{HostName,K3Node,pong}<-K3NodesStatus],
    ApplStatus=t_lib:appl_status(K3Node,Applications,NumInstances),
    
  %  io:format("DEBUG: K3NodesStatus ~p~n",[K3NodesStatus]),
   % io:format("DEBUG: ApplStatus ~p~n",[ApplStatus]),
    
   

    {ok, #state{
	    clusterid=ClusterId,
	    hosts=Hosts,
	    k3_nodes_status=K3NodesStatus,
	    appl_status=ApplStatus,
	    applications=Applications,
	    num_instances=NumInstances,

	    start_time={date(),time()}
	   }
    }.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_call({host_status},_From, State) ->
    K3NodesStatus=t_lib:connect_hosts(State#state.clusterid,
				      State#state.hosts),
    Running=[{HostName,K3Node}||{HostName,K3Node,pong}<-K3NodesStatus],
    Missing=[{HostName,K3Node}||{HostName,K3Node,pang}<-K3NodesStatus],
    Reply=case Missing of
	      []->
		  {desired_state_fulfilled,
		   running,Running,missing,Missing};
	      _->
		  {desired_state_NOT_fulfilled,
		   running,Running,missing,Missing}
	  end,
    NewState=State#state{k3_nodes_status=K3NodesStatus},
    {reply, Reply, NewState};

handle_call({appl_status},_From, State) ->
    K3NodesStatus=t_lib:connect_hosts(State#state.clusterid,
				      State#state.hosts),
    Running=[{HostName,K3Node}||{HostName,K3Node,pong}<-K3NodesStatus],
    [{_HostName,K3Node}|_]=[{HostName,K3Node}||{HostName,K3Node}<-Running],
    ApplStatus=t_lib:appl_status(K3Node,
				 State#state.applications,
				 State#state.num_instances),
    RunningAppl=[{ApplId,Nodes}||{ApplId,0,Num,Nodes}<-ApplStatus],
    MissingAppl=[{ApplId,Nodes}||{ApplId,Diff,Num,Nodes}<-ApplStatus,
			     Diff>0],
    Reply=case MissingAppl of
	      []->
		  {desired_state_fulfilled,
		   running,RunningAppl,missing,MissingAppl};
	      _->
		  {desired_state_NOT_fulfilled,
		   running,RunningAppl,missing,MissingAppl}
	  end,
    NewState=State#state{appl_status=ApplStatus},
    {reply, Reply, NewState};

handle_call({read_state},_From, State) ->
    Reply=State,
    {reply, Reply, State};

handle_call({ping},_From, State) ->
    Reply=pong,
    {reply, Reply, State};

handle_call({stopped},_From, State) ->
    Reply=ok,
    {reply, Reply, State};


handle_call({not_implemented},_From, State) ->
    Reply=not_implemented,
    {reply, Reply, State};

handle_call({stop}, _From, State) ->
    {stop, normal, shutdown_ok, State};

handle_call(Request, From, State) ->
    %rpc:cast(node(),log,log,[?Log_ticket("unmatched call",[Request, From])]),
    Reply = {ticket,"unmatched call",Request, From},
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------


handle_cast(_Msg, State) ->
  %  rpc:cast(node(),log,log,[?Log_ticket("unmatched cast",[Msg])]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({nodedown,Node}, State) ->
    io:format(" ~p~n",[{?MODULE,?LINE,nodedown,Node}]),
    {noreply, State};

handle_info(Info, State) ->
    io:format("Info ~p~n",[{?MODULE,?LINE,Info}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

		  
