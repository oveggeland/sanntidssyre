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

	def new_order(order) do
		bids = GenServer.call(__MODULE__, {:get_bids, order})
		IO.inspect(bids)
	end	

	## Call handlers
	def handle_call({:get_bids, order}, sockets) do
		#Enum.each(sockets, fn(socket) -> send(socket, {:get_bid, self(), order}) end)
		IO.puts("get_bids not implemented yet")
		:timer.sleep(1000)		
	end

	## Cast handlers

end
