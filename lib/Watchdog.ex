defmodule Watchdog do
	use GenServer, restart: :permanent
	require Logger
	
	#Constants
	@time_out 15000


	def start_link([]) do
		GenServer.start_link(__MODULE__, [], name: __MODULE__)
	end

	def init([]) do
		{:ok, []}
	end


	### Call handlers ###

	def handle_call({:spawn_watchdog, order, node}, _from, watchdog_pids) do
		pid = spawn(fn -> watchdog(order, node) end)
		watchdog_pids = [pid | watchdog_pids]
		{:reply, :ok, watchdog_pids}
	end

	### Cast handlers ###

	def handle_cast({:kill_watchdog, completed_order, node}, watchdog_pids) do
		Enum.each(watchdog_pids, fn(pid) -> send(pid, {:kill, completed_order, node}) end)
		{:noreply, watchdog_pids}
	end

	def handle_cast({:delete_watchdog_pid, pid}, watchdog_pids) do
		watchdog_pids = List.delete(watchdog_pids, pid)
		{:noreply, watchdog_pids}
	end


	### THE watchdog ###	

	defp watchdog(watch_order, node) do
		#Avoiding race_conditions by adding a random offset to time_out_val	
		time_out_val = @time_out + :rand.uniform(100)

		{floor, type} = watch_order
		Logger.info("New watchdog, watching {#{floor}, #{type}} for node: #{node}")
	
		receive do
			{:kill, ^watch_order, ^node} ->
				GenServer.cast(__MODULE__, {:delete_watchdog_pid, self()})

		after
			time_out_val ->
				GenServer.abcast(Watchdog, {:kill_watchdog, watch_order, node})
				Distributor.new_order(watch_order, node)
				Logger.info("Watchdog {#{floor}, #{type}} timed out")

		end
	end
end
