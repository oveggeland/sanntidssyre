defmodule Distributor do
	use GenServer
	require Logger

	## Possible to name this globally? ##
	def start_link(node) do
		GenServer.start_link(__MODULE__, [], name: node)
	end

	def init([]) do
		{:ok, []}
	end


	## API ##

	def add_socket(node, socket) do
		GenServer.cast(node, {:add_socket, socket})
	end 

	def get_sockets(node) do
		GenServer.call(node, :get_sockets)
	end

	def new_order(order) do
		bids = GenServer.call(__MODULE__, {:get_bids, order})
		IO.inspect(bids)
	end	

	## Call handlers

	def handle_call(:get_sockets, _from, sockets) do
		{:reply, sockets, sockets}
	end

	def handle_call({:get_bids, order}, sockets) do
		Enum.each(sockets, fn(socket) -> send(socket, {:get_bid, self(), order}) end)
		:timer.sleep(1000)		
	end

	## Cast handlers

	def handle_cast({:add_socket, socket}, sockets) do
		sockets = [socket | sockets]
		{:noreply, sockets}
	end


	def send_bid({from, order}) do
		#Do some evaluation on the order and send bid back
		send(from, {:send_bid, order*4})
	end

end
