defmodule Orders do
	use GenServer
	require Logger
	
	def start_link([]) do
		GenServer.start_link(__MODULE__, [], name: __MODULE__)
	end

	def init([]) do
		{:ok, []}
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
		GenServer.call(__MODULE__, {:check_orders, floor, direction})
	end



	def handle_call(:get_orders, _from, orders) do
		{:reply, orders, orders}
	end

	def handle_call({:check_orders, floor, direction}, _from, orders) do
		direction_map = %{:up => :hall_up, :down => :hall_down}
		val1 = Enum.filter(orders, fn order -> order == {floor, direction_map[direction]} end)
		val2 = Enum.filter(orders, fn order -> order == {floor, :cab} end)
		{:reply, val1 != [] or val2 != [], orders}
	end

	def handle_cast({:add_order, new_order}, orders) do
		orders = [new_order | orders]
		{:noreply, orders}
	end

	def handle_cast({:delete_order, order}, orders) do
		orders = List.delete(orders, order)
		{:noreply, orders}
	end


end
