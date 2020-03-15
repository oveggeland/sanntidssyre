defmodule TEST do
	def kill do
		:timer.sleep(1000)
		IO.puts("Ett sekund")
		TEST.kill
	end
end
