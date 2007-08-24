:- module(config,[
		max_retrieval_steps/1
	,	timeout/1
	]).


/**
 * Maximum number of retrieval steps
 *
 * max_retrieval_steps(-Number)
 */
max_retrieval_steps(0).


/**
 * Time Out (seconds)
 *
 * time_out(-TimeOut)
 */
timeout(2).

/**
 * Maximum number of threads
 */
max_threads(2).

