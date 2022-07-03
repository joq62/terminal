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
-module(basic_eunit).   
 
-export([start/0]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/logger.hrl").
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
    ok=cluster:appl_start([]),
    timer:sleep(3000),
  
%    create_controllers(),
    io:format(" sd:all() ~p~n",[ sd:all()]),
    timer:sleep(100),
    LeaderNodes=sd:get(leader),
    Leader=[{Node,rpc:call(Node,leader,who_is_leader,[],1000)}||{Node,_}<-LeaderNodes],
    io:format("Leader ~p~n",[Leader]),
    check_nodes(),
  
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
