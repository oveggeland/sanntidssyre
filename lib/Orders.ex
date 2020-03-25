defmodule Orders do
	use GenServer
	require Logger
	
	def start_link() do
		GenServer.start_link(__MODULE__, [], name: __MODULE__)
	end

	def init([]) do
		{:ok,[]} 
	end


	### User Interface ###

	def add_order(new_order) do
		GenServer.cast(__MODULE__, {:add_order, new_order})
	end

	def delete_order(order) do
		GenServer.cast(__MODULE__, {:delete_order, order})
	end

	#### I believe this is a useless function, but it is nice for testing
	def get_orders() do
		GenServer.call(__MODULE__, :get_orders)
	end

	def get_next_order() do
		GenServer.call(__MODULE__, :get_next_order)
	end

	def check_orders(floor, direction) do
		GenServer.call(__MODULE__, {:check_orders, floor, direction})
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
		IO.inspect(disjoint_test)
		{:reply, disjoint_test[:eq] != nil, orders}
	end

	### Cast handlers ###

	def handle_call({:add_order, new_order}, _from, orders) do
		if List.delete(orders, new_order) == orders do
			{floor, type} = new_order
			Logger.info("Order: {#{floor}, #{type}} added")
			orders = [new_order | orders]
			{:reply, :order_added, orders}
		else
			{:reply, :order_added, orders}
		end
	end

	def handle_cast({:delete_order, order}, orders) do
		{floor, type} = order
		Logger.info("Order: {#{floor}, #{type}} deleted")
		orders = List.delete(orders, order)
		{:noreply, orders}
	end


end
