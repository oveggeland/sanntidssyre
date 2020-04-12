defmodule Watchdog do
	use GenServer, restart: :permanent
	require Logger

	#Constants
	@order_time_out 15000
	@motor_time_out 5000

	def start_link([]) do
		GenServer.start_link(__MODULE__, [], name: __MODULE__)
	end

	def init([]) do
		{:ok, []}
	end


	### Call handlers ###

	def handle_call({:spawn_order_watchdog, order, node}, _from, watchdog_pids) do
		pid = spawn_link(fn -> order_watchdog(order, node) end)
		watchdog_pids = [pid | watchdog_pids]
		{:reply, :ok, watchdog_pids}
	end


	### Cast handlers ###

	def handle_cast(:spawn_motor_watchdog, watchdog_pids) do
		pid = spawn_link(fn -> motor_watchdog() end)
		watchdog_pids = [pid | watchdog_pids]
		{:noreply, watchdog_pids}
	end

	def handle_cast(:kill_motor_watchdog, watchdog_pids) do
		Enum.each(watchdog_pids, fn(pid) -> send(pid, :kill) end)
		{:noreply, watchdog_pids}
	end


	def handle_cast({:kill_order_watchdog, completed_order, node}, watchdog_pids) do
		Enum.each(watchdog_pids, fn(pid) -> send(pid, {:kill, completed_order, node}) end)
		{:noreply, watchdog_pids}
	end


	def handle_cast({:delete_watchdog_pid, pid}, watchdog_pids) do
		watchdog_pids = List.delete(watchdog_pids, pid)
		{:noreply, watchdog_pids}
	end


	### THE watchdogs ###

	defp motor_watchdog() do

		receive do
			:kill ->
				GenServer.cast(__MODULE__, {:delete_watchdog_pid, self()})
                                FSM.update_malfunction(false)
		after
			@motor_time_out ->
                                FSM.update_malfunction(true)
		end
	end

	defp order_watchdog(watch_order, node) do
		#Adding a random offset to time_out_val, preventing multiple watchdogs finishing at the exact same time
		time_out_val = @order_time_out + :rand.uniform(100)

		receive do
			{:kill, ^watch_order, ^node} ->
				GenServer.cast(__MODULE__, {:delete_watchdog_pid, self()})

		after
			time_out_val ->
				GenServer.abcast(Watchdog, {:kill_order_watchdog, watch_order, node})
				Distributor.new_order(watch_order, node)

		end
	end
end
