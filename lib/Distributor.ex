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
		if type == :cab do
			new_cab_order(order, node)
		else 
			new_hall_order(order)
		end
	end

	def new_cab_order(order, node) do
		GenServer.cast({Orders, node}, {:add_order, order})
		{reply, _} = GenServer.multi_call(Watchdog, {:spawn_order_watchdog, order, node})
		if reply != [] do
			GenServer.cast({Lights, node}, {:set_order_light, order, :on})
		end
	end

	def new_hall_order(order) do
	
		#Finding optimal elevator to handle order
		{all_bids, _} = GenServer.multi_call(__MODULE__, {:get_bids, order})
		Logger.info(all_bids)
		{lowest_bidder, _} = List.keysort(all_bids, 1) |> List.first()
	
		#Tell optimal elevator to add order	
		GenServer.cast({Orders, lowest_bidder}, {:add_order, order})

		#Tell all nodes to watch order, returns list of replicants
		{reply, _} = GenServer.multi_call(Watchdog, {:spawn_order_watchdog, order, lowest_bidder})
		
		#If a watchdog is spawned, set hall_light
		if reply != [] do
			GenServer.abcast(Lights, {:set_order_light, order, :on})
		end	
	end	

	def order_complete(order, node) do
		{floor, type} = order
		Logger.info("Order complete: {#{floor}, #{type}}. Node: #{node}")

		GenServer.abcast(Watchdog, {:kill_order_watchdog, order, node})
		if type == :cab do
			GenServer.cast(Orders, {:delete_order, order})
			GenServer.cast(Lights, {:set_order_light, order, :off})
		else
			GenServer.abcast(Orders, {:delete_order, order})
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

