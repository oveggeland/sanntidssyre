defmodule HEIS.Suppervisor do
	use Supervisor

	def start_link([]) do
		Supervisor.start_link(__MODULE__, [], name: __MODULE__)
	end

	def init([]) do
		children = [
			Counter
		]
		
		Supervisor.init(children, strategy: :one_for_one)
	end
end

defmodule Counter do
	use Task, restart: :permanent
	
	def start_link([]) do
		Task.start_link(__MODULE__, :shalom, [])
	end


        def shalom() do
                starting_val = GenServer.call(Server, :get_starting_val)
                count(starting_val)
        end

        def count(starting_val) do
                val = starting_val + 1
                IO.puts(val)
		:timer.sleep(500)
                GenServer.cast(Server, {:set_val, val, self()})
                count(val)
        end

end


defmodule Server do
	use GenServer

	def start_link([]) do
		GenServer.start_link(__MODULE__, [], name: __MODULE__)
	end

	def init([]) do
		{:ok, {0, nil}}
	end

	def handle_cast({:set_val, val, pid}, _server_val) do
		{:noreply, {val, pid}}
	end

	def handle_call(:get_starting_val, _from, {val, pid}) do
		{:reply, val, {val, pid}}
	end
	
	def handle_call(:get_pid, _from, {val, pid}) do
		{:reply, pid, {val, pid}}
	end

	
end
