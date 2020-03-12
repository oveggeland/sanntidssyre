defmodule Orders do
	use GenServer
	require Logger
	
	def start_link([]) do
		GenServer.start_link(__MODULE__, [], name: __MODULE__)
	end

	def init([]) do
		{:ok, MapSet.new()}
	end


	### User Interface ###

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
		GenServer.call(__MODULE__, {:check_orders, floor, direction})
	end


	### Call handlers ###

	def handle_call(:get_orders, _from, orders) do
		{:reply, orders, orders}
	end

	def handle_call({:check_orders, floor, direction}, _from, orders) do
		direction_map = %{:up => :hall_up, :down => :hall_down}
		map_set_test = MapSet.new([{floor, :cab}, {floor, direction_map[direction]}])
		{:reply, !MapSet.disjoint?(map_set_test, orders), orders}
	end

	### Cast handlers ###

	def handle_cast({:add_order, new_order}, orders) do
		{floor, type} = new_order
		Logger.info("Order: {#{floor}, #{type}} added")
		orders = MapSet.put(orders, new_order)
		{:noreply, orders}
	end

	def handle_cast({:delete_order, order}, orders) do
		{floor, type} = order
		Logger.info("Order: {#{floor}, #{type}} added")
		orders = MapSet.delete(orders, order)
		{:noreply, orders}
	end


end
