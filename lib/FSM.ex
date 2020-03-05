defmodule FSM do
	def _check_all_buttons(heisPID, floors, button_types) do
		cond do 
			floors == [] ->
				{nil, :no_orders}

			button_types == [] ->
				[_head | tail] = floors
				{_floor_pressed, _button_type_pressed} = _check_all_buttons(heisPID, tail, [:hall_up, :hall_down, :cab])

			true ->
				[order_head | order_tail] = button_types
				[floor_head | _floor_tail] = floors
				order = Driver.get_order_button_state(heisPID,floor_head, order_head)
				if order != 1 do
					{_floor_pressed, _button_type_pressed} = _check_all_buttons(heisPID, floors, order_tail)
				else
					{floor_head, order_head}
				end
		end
	end

	def get_orders(heisPID) do
		floors = [0, 1, 2, 3]
		order_types = [:hall_up, :hall_down, :cab]
		{floor, order_type} = _check_all_buttons(heisPID, floors, order_types)
		if floor != nil do
			IO.inspect({floor, order_type})
		end
		get_orders(heisPID)
	end

	def fsm(state) do
		case state do
			:idle ->
				floors = [0, 1, 2, 3]
				button_types = [:hall_up, :hall_down, :cab]		
				
		end
	end
end
