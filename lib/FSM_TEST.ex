defmodule FSM_TEST do
	use GenServer

	def start_link() do
		GenServer.start_link(__MODULE__, [], name: __MODULE__)
	end 

	def init([]) do
		{:ok,0}
	end

	def set_state(number) do
		GenServer.cast(__MODULE__, {:set_state, number})		
	end

	def handle_cast({:set_state, number}, state) do
		{:noreply, number}
	end

	def get_state() do
		GenServer.call(__MODULE__, :get_state)
	end
	
	def handle_call(:get_state, _from, state) do
		{:reply, state, state}
	end
end
