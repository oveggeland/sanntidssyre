defmodule FSM do
	def check_button_press(heisPID, floors, button_types) do
		IO.puts("Puta")
		cond do 
			floors == [] ->
				{nil, :no_orders}

			button_types == [] ->
				[_head | tail] = floors
				{_floor_pressed, _button_type_pressed} = check_button_press(heisPID, tail, [:hall_up, :hall_down, :cab])

			true ->
				[order_head | order_tail] = button_types
				[floor_head | _floor_tail] = floors
				order = Driver.get_order_button_state(heisPID,floor_head, order_head)
				if order != 1 do
					IO.puts("recursion same floor")
					{_floor_pressed, _button_type_pressed} = check_button_press(heisPID, floors, order_tail)
				else
					{floor_head, order_head}
				end
		end
	end

	#def fsm(state) do
	#	case state do
	#		:idle ->
	#			floors = [0, 1, 2, 3]
	#			button_types = [:hall_up, :hall_down, :cab]		
	#	end
	#end
end
