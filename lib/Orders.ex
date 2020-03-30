defmodule Orders do
	use GenServer
	require Logger

	def start_link([]) do
		GenServer.start_link(__MODULE__, [], name: __MODULE__)
	end

	def init([]) do
		{:ok,[]}
	end


	### Functions to be used by FSM: ###

	def get_next_order() do
		GenServer.call(__MODULE__, :get_next_order)
	end

	def check_orders(floor, direction) do
		GenServer.call(__MODULE__, {:check_orders, floor, direction})
	end



	### Test functions ###

	def add_order(new_order) do
		GenServer.call(__MODULE__, {:add_order, new_order})
	end

	def delete_order(order) do
		GenServer.cast(__MODULE__, {:delete_order, order})
	end

	def get_orders() do
		GenServer.call(__MODULE__, :get_orders)
	end


	### Call handlers ###

	def handle_call(:get_orders, _from, orders) do
		{:reply, orders, orders}
	end

	def handle_call(:get_next_order, _from, orders) do
		{:reply, List.last(orders), orders}
	end

	def handle_call({:check_orders, floor, direction}, _from, orders) do
		dir_map = %{:up => :hall_up, :down => :hall_down}
		disjoint_test = [{floor, :cab}, {floor, dir_map[direction]}] |> List.myers_difference(orders)
		{:reply, disjoint_test[:eq] != nil, orders}
	end

	def handle_call({:add_order, new_order}, _from, orders) do
		orders = [new_order | orders]
		{:reply, :order_added, orders}
	end


	### Cast handlers ###

	def handle_cast({:delete_order, order}, orders) do
		IO.inspect(order)
		orders = Enum.filter(orders, fn x -> x != order end)
		{:noreply, orders}
	end
end
