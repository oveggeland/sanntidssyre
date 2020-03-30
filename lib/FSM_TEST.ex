defmodule FSM_TEST do
	use GenServer

	def start_link([]) do
		GenServer.start_link(__MODULE__, [], name: __MODULE__)
	end

	def init([]) do
                IO.puts("### Initializing... ###")
                {:ok, elevatorpid} = Driver.start()
                Orders.start_link([])
                FSM.start_link([elevatorpid])
                Poller.start_link([elevatorpid])
                spawn(fn -> FSM.run_FSM end)
		{:ok,0}
	end



###SET_STATE###
	def set_state(number) do
		GenServer.cast(__MODULE__, {:set_state, number})
	end

	def handle_cast({:set_state, number}, _state) do
		{:noreply, number}
	end


###GET_STATE###
	def get_state() do
		GenServer.call(__MODULE__, :get_state)
	end

	def handle_call(:get_state, _from, state) do
		{:reply, state, state}
	end
end
