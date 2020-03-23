defmodule Distributor do
	use GenServer
	require Logger

	def message_box() do
		receive do
			{:get_bid, {from, order}}->
				send_bid({from, order})
		end
		message_box()
	end


	def start_link([]) do
		GenServer.start_link(__MODULE__, [], name: __MODULE__)
		mail = spawn_link(fn -> message_box() end)
		Process.register(mail, :mailbox)
	end

	def init([]) do
		Logger.info("Initialized Distributor with empty MapSet")
		{:ok, MapSet.new()}
	end

	### User Interface ###

	def new_distributor(PID) do
		GenServer.cast(__MODULE__, {:new_distributor, PID})
	end

	def new_order(button) do
		IO.puts("Placeholder")		
	end	

	### Cast handlers ###

	def handle_cast({:new_distributor, PID}, elevators) do
		elevators = MapSet.new(elevators, PID)
		{:noreply, elevators}
	end

	def send_bid({from, order}) do
		#Do some evaluation on the order and send bid back
		send(from, {:send_bid, order*4})
	end

end
