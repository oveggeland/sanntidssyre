defmodule Distributor do
	use GenServer, restart: :permanent
	require Logger

	def start_link([]) do
		GenServer.start_link(__MODULE__, [], name: __MODULE__)
	end

	def init([]) do
		{:ok, nil}
	end


	## API ##

	def new_order(order, node) do
		{floor, type} = order
		Logger.info("New order: {#{floor}, #{type}}")
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
		Logger.info(all_bids)
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
		{floor, type} = order
		Logger.info("Order complete: {#{floor}, #{type}}. Node: #{node}")

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

	def handle_call({:get_bids, order}, _from,  nil) do
		{:reply, calculate_cost(order), nil}
	end

        defp calculate_cost(order) do
                {order_floor, _} = order

                truth_map = %{true => 1, false => 0}

                #Retrieving state#
                orders = Orders.get_orders()

                already_taken = Enum.member?(orders, order)
                digit1 = truth_map[!already_taken]

                order_on_the_way =order_on_the_way?(order, FSM.get_state())
                digit2 = truth_map[!order_on_the_way]

                elevator_busy = List.first(orders) != nil
                digit3 = truth_map[elevator_busy]

                {state_floor, _, _, _, _} = FSM.get_state()
                distance_to_order = abs(order_floor - state_floor)
                digit4 = distance_to_order

                bid = Integer.undigits([digit1, digit2, digit3, digit4])
                Logger.info("My bid is #{bid}")
		bid
        end

        defp order_on_the_way?({order_floor, order_type},{state_floor, goal_floor, direction, _, _}) do
                if goal_floor != nil do
                        dir_map = %{:hall_up => :up, :hall_down => :down}
                        ((order_floor - state_floor)*(order_floor - goal_floor) < 0) && dir_map[order_type] == direction
                else
                        false
                end
        end
end

