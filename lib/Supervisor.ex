defmodule HEIS.Supervisor do
	use Supervisor

	def start_link([node_name, port]) do
		Supervisor.start_link(__MODULE__, [node_name, port], name: __MODULE__)
	end	

	def init([node_name, port]) do
		IO.puts(port)
		{:ok, elevPID} = Driver.start({127, 0, 0, 1}, port)
		children = [
			{PollerSupervisor, [elevPID]},
			{Lights, [elevPID]},
			{FSM, [elevPID]},
			{Cluster, [node_name]},
			Orders,
			Watchdog,
			Distributor	
		]
		Supervisor.init(children, strategy: :one_for_one)
	end

end

defmodule Heis do
	def start(name, port) do
		HEIS.Supervisor.start_link([name, port])
	end
end	
