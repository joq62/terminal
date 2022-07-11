%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% Created :
%%% Node end point  
%%% Creates and deletes Pods
%%% 
%%% API-kube: Interface 
%%% Pod consits beams from all services, app and app and sup erl.
%%% The setup of envs is
%%% -------------------------------------------------------------------
-module(monkey).   
 
-export([start/0]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/logger.hrl").

-define(NumLoopsDesiredState,1).
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
 %   loop(KilledWorker,KilledK3Node,KilledHost),
    loop(?NumLoopsDesiredState),
  
%    create_controllers(),
 %   io:format(" sd:all() ~p~n",[ sd:all()]),
 %   timer:sleep(100),
 %   LeaderNodes=sd:get(leader),
 %   Leader=[{Node,rpc:call(Node,leader,who_is_leader,[],1000)}||{Node,_}<-LeaderNodes],
 %   io:format("Leader ~p~n",[Leader]),
 %   check_nodes(),
  
%    ok=start_math_monkey(),
 %   ok=calculator_test(),
   % [rpc:call(N,init,stop,[],1000)||N<-nodes()],
    
   %% test pod_lib
  

%    init:stop(),
    ok.




%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------

%loop(KilledWorker,KilledK3Node,KilledHost)->
loop(NumDesiredState)->
    {HostState,running,_,missing,_}=t:host_status(),
    {ApplState,running,_,missing,_}=t:appl_status(),    
    Leader=case t:leader() of
	       {error,Reason}->
		   io:format("error,Reason ~p~n",[{error,Reason,?MODULE,?LINE}]),
		   [];
		   
	       []->
		   io:format("No leader available  ~p~n",[{[],?MODULE,?LINE}]),
		   [];
	       Leader1->
		   print(Leader1,HostState,ApplState),
		   Leader1
	   end,
    NewDesiredState=case {HostState,ApplState} of
			{desired_state_fulfilled,desired_state_fulfilled}->
			    case NumDesiredState of
				0->
				    case Leader of
					[]->
					    io:format(" leader ~p~n",[{[],?MODULE,?LINE}]),
					    ?NumLoopsDesiredState;
					Leader->
					    io:format(" leader ~p~n",[{Leader,?MODULE,?LINE}]),
					   % K3Nodes=rpc:call(Leader,sd,get,[k3_node],2000),
					 %   io:format(" REBOOT leader ~p~n",[{Leader,?MODULE,?LINE}]),
					  % RebootResult=rpc:call(Leader,os,cmd,["sudo reboot"],5000),
					  %  io:format(" RebootResult ~p~n",[{RebootResult,?MODULE,?LINE}]),
					    
					    
					    io:format(" killed Leader ~p~n",[{Leader,?MODULE,?LINE}]),
					    rpc:call(Leader,init,stop,[],1000),
					    ?NumLoopsDesiredState
				    end;
				_->
				    NumDesiredState-1
			    end;
			_->
			    io:format(" ~p~n",[{desired_state_NOT_fulfilled,?MODULE,?LINE}]),
			    ?NumLoopsDesiredState
		    end,
    timer:sleep(10*1000),
    loop(NewDesiredState).


kill_leader()->
    ok.

kill_worker()->
    ok.


reboot_host()->
    
    ok.

kill_k3_node(Node)->
    rpc:call(Node,init,stop,[],1000),
    ok.


print(Leader,HostState,ApplState)->
    io:format(" leader ~p~n",[{ Leader,?MODULE,?LINE}]),
    io:format("k3_node ~p~n",[rpc:call(Leader,sd,get,[k3_node],2000)]),
    io:format("HostState ~p~n",[HostState]),	
    io:format("ApplState ~p~n",[ApplState]),	      
    io:format("test_math ~p~n",[rpc:call(Leader,sd,get,[test_math],2000)]),
    io:format("test_add ~p~n",[rpc:call(Leader,sd,get,[test_add],2000)]),
    io:format("test_divi ~p~n",[rpc:call(Leader,sd,get,[test_divi],2000)]),
    io:format("test_sub ~p~n",[rpc:call(Leader,sd,get,[test_sub],2000)]),
    ok.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
check_nodes()->
    io:format("nodes() ~p~n",[{time(),nodes()}]),
    LeaderNodes=sd:get(leader),
    Leader=[{rpc:call(Node,leader,who_is_leader,[],2000),Node}||{Node,_}<-LeaderNodes],
    io:format("Node thinks that X is Leader ~p~n",[Leader]),
    io:format("test_math ~p~n",[sd:get(test_math)]),
    io:format("test_add ~p~n",[sd:get(test_add)]),
    io:format("test_divi ~p~n",[sd:get(test_divi)]),
    io:format("test_sub ~p~n",[sd:get(test_sub)]),
    
 
    timer:sleep(20*1000),
    check_nodes().    

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
calculator_test()->
    case sd:get(test_math) of
	[]->
	    io:format("no nodes available ~n");
	[{N1,_}|_]->
	    io:format("20+22= ~p~n",[rpc:call(N1,test_math,add,[20,22],2000)])
    end,
    timer:sleep(5000),
    calculator_test().


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
start_math_monkey()->
    ok.




setup()->
  
    % Simulate host
    R=rpc:call(node(),test_nodes,start_nodes,[],2000),
%    [Vm1|_]=test_nodes:get_nodes(),

%    Ebin="ebin",
 %   true=rpc:call(Vm1,code,add_path,[Ebin],5000),
 
    R.
