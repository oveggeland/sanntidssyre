defmodule Orders do
	use GenServer
	require Logger
	
	def start_link([]) do
		GenServer.start_link(__MODULE__, [], name: __MODULE__)
	end

	def init([]) do
		{:ok, [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]}
	end

	def add_order(new_order) do
		GenServer.cast(__MODULE__, {:add_order, new_order})
	end

	def delete_order(order) do
		GenServer.cast(__MODULE__, {:delete_order, order})
	end

	

	def get_orders() do
		GenServer.call(__MODULE__, :get_orders)
	end

	def check_orders(floor, direction) do
		GenServer.call(__MODULE__, {:chech_orders, floor, direction})
	end



	def handle_call(:get_orders, _from, orders) do
		{:reply, orders, orders}
	end

	def handle_call(:check_orders, _floor, _direction, _from, orders) do
		{:reply,"This is not implemented yet", orders}
	end
	
	def _get_index({floor, button_type} = _order) do
		button_type_map = %{:hall_up => 0, :hall_down => 1, :cab => 2}
		_index = 3*floor + Map.get(button_type_map, button_type)
	end

	def handle_cast({:add_order, new_order}, orders) do
		orders = List.replace_at(orders, _get_index(new_order), 1)
		{:noreply, orders}
	end

	def handle_cast({:delete_order, order}, orders) do
		orders = List.replace_at(orders, _get_index(order), 0)
		{:noreply, orders}
	end


end
