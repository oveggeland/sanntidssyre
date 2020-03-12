defmodule HEIS do
	def run heis do
		# Driver.set_motor_direction heis, :down
		floor = Driver.get_floor_sensor_state heis
		case floor do
			3 ->
				Driver.set_motor_direction heis, :down
			0 ->
				Driver.set_motor_direction heis, :up
			_ ->	
				# Default case	
		end
		HEIS.run heis
	end

	def update_floor_lights heisPID do
		floor = Driver.get_floor_sensor_state heisPID
		case floor do
			:between_floors ->
						Driver.set_door_open_light heisPID, :on
						# Do nothing
			_ 		->
						Driver.set_floor_indicator heisPID, floor
		end
		update_floor_lights heisPID
	end

	def call_elevator heisPID
			
	end
	
	def _go_to_floor heisPID, floor do
		position = Driver.get_floor_sensor_state heisPID
		case position do
			position > floor ->
				Driver.set_motor_direction heisPID, :down
			position < floor ->
				Driver.set_motor_direction heisPID, :up
			position == floor ->
				Driver.set_motor_direction heisPID, :stop
			_ ->
				:between_floors
		end
end
