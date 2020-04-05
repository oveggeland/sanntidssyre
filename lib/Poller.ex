defmodule PollerSupervisor do
	use Supervisor
	require Logger

	@n_floors 4

	def start_link([elevPID]) do
		Supervisor.start_link(__MODULE__, [elevPID], name: __MODULE__)
	end

	def init([elevPID]) do
		children = get_all_buttons() |> Enum.map_every(1,fn(btn)->%{id: btn, start: {ButtonPoller, :start_link, [elevPID, btn]}} end)
		children = [{FloorPoller, [elevPID]} | children]

		Supervisor.init(children, strategy: :one_for_one)
	end

	defp get_all_buttons() do
	 	for floors <- 0..(@n_floors-1), type <- [:cab, :hall_up, :hall_down] do {floors, type} end |> List.delete({0, :hall_down}) |> List.delete({@n_floors, :hall_up})
	end
end


defmodule ButtonPoller do
	use Task, restart: :permanent
	def start_link(elevPID, btn) do
		Task.start_link(__MODULE__, :button_poller, [elevPID, btn])
	end

	def button_poller(elevPID, {floor, type}) do
		:timer.sleep(50)
		
		button_push = Driver.get_order_button_state(elevPID, floor, type)
		if button_push == 1 do
			Distributor.new_order({floor, type}, Node.self())
			:timer.sleep(250)
		end
	
		button_poller(elevPID, {floor, type})
	end
end

defmodule FloorPoller do 
	use Task, restart: :permanent
	def start_link([elevPID]) do
		Task.start_link(__MODULE__, :floor_poller, [elevPID])
	end

	def floor_poller(elevPID) do
		:timer.sleep(200)

		floor_sensor = Driver.get_floor_sensor_state(elevPID)
		if floor_sensor != :between_floors do
			#Logger.info("At floor #{floor_sensor}")	
	               	FSM.update_floor(floor_sensor)
		end

		floor_poller(elevPID)
	end
end
