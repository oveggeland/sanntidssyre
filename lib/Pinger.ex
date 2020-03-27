defmodule Cluster do
	use Task
	
	@broadcast_port 4444
	@broadcast_ip {255,255,255,255}


	def start_link([name]) do

		### Spawning all modules ###
		FSM_TEST.start_link()
		Distributor.start_link([])
		Orders.start_link()
		#Watchdog.start_link()


		get_my_node_name(name) |> Node.start()
		Node.set_cookie(:safari)
		Task.start_link(fn -> connect_to_cluster(name) end)
	end

	def connect_to_cluster(name) do
		options = [active: false, broadcast: true, reuseaddr: true]
		{:ok, socket} = :gen_udp.open(@broadcast_port, options)
		spawn_link(fn -> node_broadcast(socket, name) end)
		node_listener(socket)
	end

	def node_broadcast(socket, name) do
		:gen_udp.send(socket, @broadcast_ip, @broadcast_port, name)
	
		:timer.sleep(5000)
		node_broadcast(socket, name)
	end

	def node_listener(socket) do
		{:ok, {ip, _port, data}} = :gen_udp.recv(socket, 0)

		name = data |> to_string()
		node_name = name<>"@"<>(:inet.ntoa(ip) |> to_string())
		node_name |> String.to_atom() |> Node.ping()
	
		node_listener(socket)
	end	

	defp get_my_node_name(name) do
		{:ok, ip_tuple} = :inet.getif()
		ip = ip_tuple |> Enum.at(2) |> Tuple.to_list() |> Enum.at(0) |> :inet.ntoa() |> to_string()
		name <> "@" <> ip |> String.to_atom()
	end

end


