#!/usr/local/bin/pl -g serql_welcome -L256m -G256m -T256m -s

:- [henry].

:- 
	use_module('dsp-builtins/aspl_builtins'), 
	use_module('dsp-builtins/vamp_builtins'),
	use_module('dsp-builtins/similarity_builtins'),
	use_module('dsp-builtins/musicutils').

source :- 
	n3_load('dsp-n3/decode.n3'),
	n3_load('dsp-n3/vamp.n3'),
	n3_load('dsp-n3/similarity.n3').

%:- source.
%:- compile_all.

