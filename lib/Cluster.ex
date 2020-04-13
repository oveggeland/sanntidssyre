defmodule Cluster do
	use Task, restart: :permanent

	@broadcast_port 4444
	@broadcast_ip {127,0,0,1}


	def start_link([name]) do
		get_node_name(name) |> Node.start(:longnames, 1000)
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
		name = to_string(data)
		ip = :inet.ntoa(ip) |> to_string() 
		get_node_name(name, ip) |> Node.ping()

		node_listener(socket)
	end

	defp get_node_name(name) do
		## Running on multiple computers ##
		#{:ok, ip_tuple} = :inet.getif()
		#ip = ip_tuple |> Enum.at(2) |> Tuple.to_list() |> Enum.at(0) |> :inet.ntoa() |> to_string()
		
		## Running localy ##
		ip = "127.0.0.1"
		
		get_node_name(name, ip)		
	end

	defp get_node_name(name, ip) do
		name <> "@" <> ip |> String.to_atom()	
	end

end


