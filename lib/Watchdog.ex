defmodule Watchdog do
	use GenServer
	require Logger
	
	#Constants
	def order_time, do: 30000


	def start_link([]) do
		GenServer.start_link(__MODULE__, [], name: __MODULE__)
	end

	def init([]) do
		{:ok, []}
	end



	### User Interface ###

	def set_order_timer(new_order) do
		GenServer.cast(__MODULE__, {:set_order_timer, new_order})		
	end
	
	def get_timers() do
		GenServer.call(__MODULE__, :get_timers)
	end

	def order_complete(order) do
		GenServer.cast(__MODULE__, {:order_complete, order})
	end

	### Call handlers ###

	def handle_call(:get_timers, _from, orders) do
		{:reply, orders, orders}
	end

	### Cast handlers ###



	def handle_cast({:set_order_timer, new_order}, timers) do
		watchdog_pid = spawn fn -> _watchdog(new_order) end
		IO.puts("ferdig spawnet")
		timers = [watchdog_pid | timers]
		{:noreply, timers}
	end

	def handle_cast({:order_complete, completed_order}, timers) do
		#Regner med det er en mer elegant funksjon enn filter som kan brukes :) 
		Enum.filter(timers, fn timer -> send(timer, {:kill, completed_order}) end)
		{:noreply, timers}
	end

	def handle_cast({:delete_watchdog_pid, pid}, timers) do
		timers = List.delete(timers, pid)
		{:noreply, timers}
	end

	### Help functions ###	

	def _watchdog(watch_order) do
		Logger.info("Watchdog succesfully spawned")
		receive do
			{:kill, order} ->
				if order == watch_order do
					Logger.info("Process is commiting suicide")
					GenServer.cast(__MODULE__, {:delete_watchdog_pid, self()})
					Process.exit(self(), :kill)
				end
		after
			30_000 -> 
				Logger.info("Watchdog timed out")
				Process.exit(self(), :kill)
		end
	end
end


#Her er planen, add_order spawner en watchdog og legger PIDen dens inn i GenServeren. 

#Hver watchdog venter på en beskjed om kill, receive :kill -> commit suicide. 
