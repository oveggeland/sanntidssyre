defmodule FSMtest do
	use ExUnit.Case
	doctest FSM

	test "ButtonPress"(heisPID) do
		floors = [0, 1, 2, 3]
		btns = [:hall_up, :hall_down, :cab]
		assert {floor, type} = FSM.check_button_press(heisPID, floors, btns)
		{floor, type}
	end
end
