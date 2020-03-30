defmodule Distributor do
	use GenServer

	def start_link([]) do
		GenServer.start_link(__MODULE__, [], name: __MODULE__)
	end

	def init([]) do
		{:ok, nil}
	end


	## API ##

	def new_order(order) do
		IO.puts("Distributing")
		#Finding optimal elevator to handle order
		{all_bids, _} = GenServer.multi_call(__MODULE__, {:get_bids, order})
		{lowest_bidder, _} = List.keysort(all_bids, 1) |> List.first()
		
		#Tell the relevant elevator to take order, answer :order_added if true
		_answer = GenServer.call({Orders, lowest_bidder}, {:add_order, order})

		#Tell all watchdogs to watch order, returns list of replies
		{_replies, _} = GenServer.multi_call(Watchdog, {:spawn_watchdog, order})
		
		GenServer.abcast(Lights, {:set_order_light, order, :on})

		
		### TODO ###
		#Implement what should happen if reply from Watchdog call is empty? No responders?
		#Should action depend on whether or not the order is cab/call?
		#Turn on order_light if appropriate
	end	

	def order_complete(order) do
		### Casting order to kill watchdogs for all nodes
		GenServer.abcast(Watchdog, {:kill_watchdog, order})
		
		### Casting to all nodes that Orders should delete order
		GenServer.abcast(Orders, {:delete_order, order})

		GenServer.abcast(Lights, {:set_order_light, order, :off})
		

		### Skru av ordrelys ###	
	end		


	## Call handlers

	def handle_call({:get_bids, _order}, _from,  nil) do
		### TODO ###
		#Implement the cost-function routine
		{:reply, FSM_TEST.get_state(), nil}
	end
end
