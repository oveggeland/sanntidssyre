defmodule Orders do
	use GenServer
	require Logger
	
	def start_link([]) do
		GenServer.start_link(__MODULE__, [], name: __MODULE__)
	end

	def init([]) do
		{:ok, []}
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
		direction_test = Enum.filter(orders, fn order -> order == {floor, direction_map[direction]} end)
		cab_test = Enum.filter(orders, fn order -> order == {floor, :cab} end)
		{:reply, direction_test != [] or cab_test != [], orders}
	end

	### Cast handlers ###

	def handle_cast({:add_order, new_order}, orders) do
		orders = List.delete(orders, new_order)
		{floor, type} = new_order
		Logger.info("Order: {#{floor}, #{type}} added")
		orders = [new_order | orders]
		{:noreply, orders}
	end

	def handle_cast({:delete_order, order}, orders) do
		{floor, type} = order
		Logger.info("Order: {#{floor}, #{type}} added")
		orders = List.delete(orders, order)
		{:noreply, orders}
	end


end
