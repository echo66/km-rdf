:- module(ops,[
		op(1025,xfy,'>>')  %serial conjuction
        ,       op(1000,xfy,'and') %conjunction
        ,       op(1100,xfy,'or') %disjunction
        ,       op(1075,xfy,'#') %concurrent conjunction
        ,       op(1110,fx,'o') %modifier - atomic action (action must execute atomically)
	,	op(300,xfx,'=>') %bulk insert
	,	op(300,fx,'<=') %bulk delete
	,	op(300,xfy,'=>>') %for all (sequence)
	,	op(300,xfy,'=#>') %for all (concurrent conjunction)
	,	op(300,xfy,'=&>') %for all (conjunction)
	,	op(300,xfx,'rs') %repeat n times (sequence)
	,       op(300,xfx,'rc') %repeat n times (concurrent conjunction)
	]).
