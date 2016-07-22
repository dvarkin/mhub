{application, mhub, [
	{description, "Simple message hubN"},
	{vsn, "0.0.1"},
	{modules, ['mhub','mhub_app','mhub_queue','mhub_queue_register','mhub_queue_sup','mhub_sup','mhub_tcp','mhub_udp','mhub_udp_client']},
	{registered, [mhub_sup]},
	{applications, [kernel,stdlib,ranch,jiffy]},
	{mod, {mhub_app, []}}
]}.