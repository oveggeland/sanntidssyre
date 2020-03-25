defmodule Poller do
	use Task
	require Orders

	def start_link([heisPID]) do
		buttons = get_all_buttons()
		Enum.each(buttons, fn(button)-> 
			Task.start_link(fn -> button_poller(heisPID, button) end) 
						end)
		Task.start_link(fn -> floor_poller(heisPID) end)
	end
	

	def button_poller(heisPID, {floor, type}) do
		:timer.sleep(50)
		button_push = Driver.get_order_button_state(heisPID, floor, type)
		if button_push == 1 do
			Orders.add_order({floor, type})
			#### Add whatever happens when button is pushed ####
		end
		button_poller(heisPID, {floor, type})
	end
	
	def floor_poller(heisPID) do
		:timer.sleep(50)
		floor_sensor = Driver.get_floor_sensor_state(heisPID)
		if floor_sensor != :between_floors do
			IO.puts("Floor: #{floor_sensor}")
			#### Add whatever happens when floor sensor goes off! ####
		end
		floor_poller(heisPID)	
	end	

	defp get_all_buttons() do 
	 	for floors <- 0..3, type <- [:cab, :hall_up, :hall_down] do {floors, type} end |> List.delete({0, :hall_down}) |> List.delete({3, :hall_up})
	end

end
