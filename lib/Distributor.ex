defmodule Distributor do
	use GenServer, restart: :permanent

	def start_link([]) do
		GenServer.start_link(__MODULE__, [], name: __MODULE__)
	end

	def init([]) do
		{:ok, nil}
	end


	## API ##

	def new_order(order, node) do
		case order do
			{_, :cab} ->
				new_cab_order(order, node)
			_ -> 
				new_hall_order(order)
		end
	end

	def new_cab_order(order, node) do
		##What the hell should I do if new Cab Order?? ###
		GenServer.call(Orders, {:add_order, order})
		GenServer.multi_call(Watchdog, {:spawn_watchdog, order, node})
		GenServer.cast(Lights, {:set_order_light, order, :on})
	end

	def new_hall_order(order) do
		#Finding optimal elevator to handle order
		{all_bids, _} = GenServer.multi_call(__MODULE__, {:get_bids, order})
		{lowest_bidder, _} = List.keysort(all_bids, 1) |> List.first()
		
		#Tell the relevant elevator to take order, answer :order_added if true
		_answer = GenServer.call({Orders, lowest_bidder}, {:add_order, order})

		#Tell all watchdogs to watch order, returns list of replies
		{replies, _} = GenServer.multi_call(Watchdog, {:spawn_watchdog, order, lowest_bidder})
		
		if Enum.at(replies, 0) != nil do
			GenServer.abcast(Lights, {:set_order_light, order, :on})
		end	
	end	

	def order_complete(order, node) do
		### Casting order to kill watchdogs for all nodes
		IO.puts("Order Complete")
		GenServer.abcast(Watchdog, {:kill_watchdog, order, node})
		
		### Casting to node that Orders should delete order
		GenServer.cast({Orders, node}, {:delete_order, order})
		
		case order do
			{_, :cab} ->
				GenServer.cast(Lights, {:set_order_light, order, :off})
			_->
				GenServer.abcast(Lights, {:set_order_light, order, :off})
		end
	end		


	## Call handlers

	def handle_call({:get_bids, _order}, _from,  nil) do
		### TODO ###
		#Implement the cost-function routine
		{:reply, :rand.uniform(10), nil}
	end
end
