defmodule Distributor do
	use GenServer
	require Logger

	## Possible to name this globally? ##
	def start_link([]) do
		GenServer.start_link(__MODULE__, [], name: __MODULE__)
	end

	def init([]) do
		{:ok, nil}
	end


	## API ##

	def new_order(order) do
		{bids, _} = GenServer.multi_call(__MODULE__, {:get_bids, order})
		{lowest_bidder, _} = List.keysort(bids, 1) |> List.first()
		GenServer.call({Orders, lowest_bidder}, {:add_order, order})
		{watchdogs, _} = GenServer.multi_call(Watchdog, {:spawn_watchdog, order})
		if watchdogs == [] do
			IO.puts("Did not succesfully spawn any watchdogs!")
			### NO WATCHDOGS SPAWNED!!! ###
		end
	end	

	

	## Call handlers
	def handle_call({:get_bids, _order}, _from,  nil) do
		{:reply, FSM_TEST.get_state(), nil}
		#### IMPLEMENT A BETTER BID GENERATOR ####
	end

	## Cast handlers

end
