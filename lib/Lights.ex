defmodule Lights do
	use GenServer
	require Logger


	def start_link([elevPID]) do
		GenServer.start_link(__MODULE__, [elevPID], name: __MODULE__)
	end

	def init([elevPID]) do
		{:ok, elevPID}
	end


	def set_order_light(order, state) do
		GenServer.cast(__MODULE__, {:set_order_light, order, state})
	end

	def set_door_open_light(state) do
		GenServer.cast(__MODULE__, {:set_door_open_light, state})
	end

        def clear_all_order_lights() do
                GenServer.cast(__MODULE__, :clear_lights)
        end


	### Cast handlers ###
	def handle_cast({:set_order_light, {floor, button_type}, state}, elevPID) do
		Driver.set_order_button_light(elevPID, button_type, floor, state)
		{:noreply, elevPID}
	end

	def handle_cast({:set_door_open_light, state}, elevPID) do
		Driver.set_door_open_light(elevPID, state)
		{:noreply, elevPID}
	end

	def handle_cast(:clear_lights, elevPID) do
                for floors <- 0..3, type <- [:cab, :hall_up, :hall_down] do Driver.set_order_button_light(elevPID, type, floors, :off) end
		{:noreply, elevPID}
	end


end
