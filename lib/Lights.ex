defmodule Lights do
	use GenServer
	require Logger


	def start_link([elevPID]) do
		GenServer.start_link(__MODULE__, [elevPID], name: __MODULE__)
	end

	def init([elevPID]) do
                clear_all_order_lights(elevPID)
                Driver.set_door_open_light(elevPID, :off)
		{:ok, elevPID}
	end


	def set_order_light(order, state) do
		GenServer.cast(__MODULE__, {:set_order_light, order, state})
	end

	def set_door_open_light(state) do
		GenServer.cast(__MODULE__, {:set_door_open_light, state})
	end

        def update_floor_indicator(floor) do
                GenServer.cast(__MODULE__, {:update_floor_indicator, floor})
        end

        def update_door_open(state) do
                GenServer.cast(__MODULE__, {:update_door_open, state})
        end



	### Cast handlers ###
	def handle_cast({:set_order_light, {floor, button_type}, state}, elevPID) do
		Logger.info("Setting order light: {#{floor}, #{button_type}} #{state}")
		Driver.set_order_button_light(elevPID, button_type, floor, state)
		{:noreply, elevPID}
	end

	def handle_cast({:set_door_open_light, state}, elevPID) do
		Logger.info("Setting door-open light: #{state}")
		Driver.set_door_open_light(elevPID, state)
		{:noreply, elevPID}
	end


	def handle_cast({:update_floor_indicator, floor}, elevPID) do
		Driver.set_floor_indicator(elevPID, floor)
		{:noreply, elevPID}
	end

	def handle_cast({:update_door_open, state}, elevPID) do
                Driver.set_door_open_light(elevPID, state)
		{:noreply, elevPID}
	end

	defp clear_all_order_lights(elevPID) do
                for floors <- 0..3, type <- [:cab, :hall_up, :hall_down] do Driver.set_order_button_light(elevPID, type, floors, :off) end
	end

end
