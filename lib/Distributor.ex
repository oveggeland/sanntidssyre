defmodule Distributor do
	use GenServer
	require Logger

	def start_link([]) do
		GenServer.start_link(__MODULE__, [], name: __MODULE__)
	end

	def init([]) do
		{:ok, nil}
	end


	## API ##

	def new_order(order) do
		#Finding optimal elevator to handle order
		{all_bids, _} = GenServer.multi_call(__MODULE__, {:get_bids, order})
		{lowest_bidder, _} = List.keysort(all_bids, 1) |> List.first()
		
		#Tell the relevant elevator to take order
		GenServer.call({Orders, lowest_bidder}, {:add_order, order})

		#Tell all watchdogs to watch order
		{_replies, _} = GenServer.multi_call(Watchdog, {:spawn_watchdog, order})


		#case replies do
		#	^[] ->
		#		IO.puts("Did not succesfully spawn any watchdogs!")
		#	_ ->
		#		IO.puts("Order lights on")
		#end

	end	

	def order_complete(order) do
		GenServer.abcast(Watchdog, {:kill_watchdog, order})
		### Skru av ordrelys ###
	end	


	## Call handlers
	def handle_call({:get_bids, _order}, _from,  nil) do
		{:reply, FSM_TEST.get_state(), nil}
	end

	## Cast handlers

end
