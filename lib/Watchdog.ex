defmodule Watchdog do
	use GenServer
	require Logger
	
	#Constants
	def order_time, do: 30000


	def start_link([]) do
		GenServer.start_link(__MODULE__, [], name: __MODULE__)
	end

	def init([]) do
		{:ok, MapSet.new()}
	end



	### User Interface ###

	def spawn_watchdog(new_order) do
		GenServer.cast(__MODULE__, {:spawn_watchdog, new_order})		
	end
	
	def get_timers() do
		GenServer.call(__MODULE__, :get_timers)
	end

	def order_complete(order) do
		GenServer.cast(__MODULE__, {:kill_watchdog, order})
	end


	### Call handlers ###

	def handle_call(:get_timers, _from, watchdog_timers) do
		{:reply, watchdog_timers, watchdog_timers}
	end


	### Cast handlers ###

	def handle_cast({:spawn_watchdog, new_order}, watchdog_pids) do
		watchdog_pid = spawn fn -> _watchdog(new_order) end
		watchdog_pids = MapSet.put(watchdog_pids, watchdog_pid)
		{:noreply, watchdog_pids}
	end

	def handle_cast({:kill_watchdog, completed_order}, watchdog_pids) do
		Enum.each(watchdog_pids, fn watchdog_pid -> send(watchdog_pid, {:kill, completed_order}) end)
		{:noreply, watchdog_pids}
	end

	def handle_cast({:delete_watchdog_pid, watchdog_pid}, watchdog_pids) do
		watchdog_pids = MapSet.delete(watchdog_pids, watchdog_pid)
		{:noreply, watchdog_pids}
	end


	### THE watchdog ###	

	def _watchdog(watch_order) do
		Logger.info("Watchdog succesfully spawned")
		receive do
			{:kill, ^watch_order} ->
				Logger.info("Process is commiting suicide")
		after
			30_000 -> 
				Logger.info("Watchdog timed out")
		end
		GenServer.cast(__MODULE__, {:delete_watchdog_pid, self()})
	end
end
