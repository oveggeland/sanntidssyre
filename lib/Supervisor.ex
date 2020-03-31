defmodule HEIS.Supervisor do
	use Supervisor

	def start_link([node_name]) do
		Supervisor.start_link(__MODULE__, [node_name], name: __MODULE__)
	end	

	def init([node_name]) do
		{:ok, elevPID} = Driver.start()
		children = [
			%{	id: Poller,
				start: {Poller, :start_link, [[elevPID]]},
				restart: :permanent
			},
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
	def start(name) do
		HEIS.Supervisor.start_link([name])
	end
end	
