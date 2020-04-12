defmodule Distributor do
	use GenServer, restart: :permanent
	require Logger

	def start_link([]) do
		GenServer.start_link(__MODULE__, [], name: __MODULE__)
	end

	def init([]) do
		{:ok, nil}
	end


	def new_order(order, node) do
		{_floor, type} = order
	
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
		{lowest_bidder, _} = List.keysort(all_bids, 1) |> List.first()

		GenServer.cast({Orders, lowest_bidder}, {:add_order, order})

		{reply, _} = GenServer.multi_call(Watchdog, {:spawn_order_watchdog, order, lowest_bidder})
		if reply != [] do
			GenServer.abcast(Lights, {:set_order_light, order, :on})
		end
	end

	def order_complete(order, node) do
		{_floor, type} = order

		GenServer.abcast(Watchdog, {:kill_order_watchdog, order, node})
		if type == :cab do
			GenServer.cast(Orders, {:delete_order, order})
			GenServer.cast(Lights, {:set_order_light, order, :off})
		else
			GenServer.abcast(Orders, {:delete_order, order})
			GenServer.abcast(Lights, {:set_order_light, order, :off})
		end
	end



	def handle_call({:get_bids, order}, _from,  nil) do
		{:reply, calculate_cost(order), nil}
	end

        defp calculate_cost(new_order) do
		truth_map = %{true => 1, false => 0}

                {_,_,_,malfunction} = FSM.get_state()
                digit0 = truth_map[malfunction]

                orders = Orders.get_orders()
                already_taken = Enum.member?(orders, new_order)
                digit1 = truth_map[!already_taken]

                order_on_the_way =order_on_the_way?(new_order, FSM.get_state())
                digit2 = truth_map[!order_on_the_way]

                elevator_busy = List.first(orders) != nil
                digit3 = truth_map[elevator_busy]

                {new_order_floor, _} = new_order
                {state_floor, _, _, _} = FSM.get_state()
                distance_to_order = abs(new_order_floor - state_floor)
                digit4 = distance_to_order
		

                Integer.undigits([digit0, digit1, digit2, digit3, digit4])
        end

        defp order_on_the_way?({order_floor, order_type},{state_floor, goal_floor, _, _}) do
                if goal_floor != nil do
                        dir_map = %{:hall_up => :up, :hall_down => :down}
                        ((order_floor - state_floor)*(order_floor - goal_floor) < 0) && dir_map[order_type] == find_direction(state_floor, goal_floor)
                else
                        false
                end
        end

        defp find_direction(old_floor, new_floor) do
                cond do
                        old_floor == new_floor ->
                                :stop
                        old_floor > new_floor ->
                                :down
                        old_floor < new_floor ->
                                :up
                end
        end


end

