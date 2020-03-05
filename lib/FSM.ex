defmodule FSM do
	use GenServer

	def start_link([]) do
		GenServer.start_link(__MODULE__, [], name: __MODULE__)
	end

	def init([]) do #structen vår er {floor(siste etasje), goal_floor, direction}
		{:ok, {nil, nil, :stop}}
	end


	def handle_call(:get_state, _from, state) do
		{:reply, state, state}
	end
	
	def handle_cast({:update, floor}, state) do
		{_dontcare, goal_floor, direction} = state
		{:noreply, {floor, goal_floor, direction}}
	end

	def get_state() do
		GenServer.call(__MODULE__, :get_state)
	end


	def floor_sensor(floor) do
		GenServer.cast(__MODULE__, {:update, floor})
	end



#Denne kan vi legge et annet sted helst:)
	def initialize() do
		{:ok, heisPID} = Driver.start
		Driver.set_motor_direction(heisPID, :down)
		{:ok, heisPID}
	end



end
