defmodule Watchdog do
	use GenServer
	require Logger
	
	#Constants
	@time_out 15000


	def start_link([]) do
		GenServer.start_link(__MODULE__, [], name: __MODULE__)
	end

	def init([]) do
		{:ok, []}
	end



	### User Interface ###

	def spawn_watchdog(order) do
		GenServer.cast(__MODULE__, {:spawn_watchdog, order})		
	end

	def order_complete(order) do
		GenServer.cast(__MODULE__, {:kill_watchdog, order})
	end


	### Call handlers ###

	def handle_call({:spawn_watchdog, order}, _from, watchdog_pids) do
		pid = spawn(fn -> watchdog(order) end)
		watchdog_pids = [pid | watchdog_pids]
		{:reply, :ok, watchdog_pids}
	end

	### Cast handlers ###

	def handle_cast({:kill_watchdog, completed_order}, watchdog_pids) do
		Enum.each(watchdog_pids, fn(pid) -> send(pid, {:kill, completed_order}) end)
		{:noreply, watchdog_pids}
	end

	def handle_cast({:delete_watchdog_pid, pid}, watchdog_pids) do
		watchdog_pids = List.delete(watchdog_pids, pid)
		{:noreply, watchdog_pids}
	end


	### THE watchdog ###	

	defp watchdog(watch_order) do
		#Avoiding race_conditions by adding a random offset to time_out_val	
		time_out_val = @time_out + :rand.uniform(100)
		receive do
			{:kill, ^watch_order} ->
				GenServer.cast(__MODULE__, {:delete_watchdog_pid, self()})

		after
			time_out_val ->
				GenServer.abcast(Watchdog, {:kill_watchdog, watch_order})
				Distributor.new_order(watch_order)
				Logger.info("Watchdog timed out")

		end
	end
end
