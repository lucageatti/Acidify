{application, acid, [ 
	{description, "An Erlang behaviour providing generic atomic transactions over distributed memories."},
	{vsn, "1.0.0"},
	{modules, [acid, gen_acid, acid_lexer, acid_parser, acid_manager, executor, multicast, riak_acid, amazon_s3_acid]},
	{registered, [acid]},
	{env, [{servers, ['server@192.168.0.27']}]},
	{applications, [kernel, stdlib, crypto, asn1, public_key, ssl, xmerl, inets, jsx, eini, base16, lhttpc, erlcloud]},
	{included_applications, [riakc]},
	{mod,{acid,[]}}
]}.
