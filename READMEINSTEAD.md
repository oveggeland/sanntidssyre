HEllo, this is our elevator. I hope you enjoy riding with us! :)

To run several elevators, please follow these instructions:

For every elevator you need to run two terminals. 

Open the first terminal and go to sanntidssyre/simulator. 
Type ./SimElevatorServer --port #PORT_NUMBER. You can choose your own #PORT_NUMBER.
On the other terminal, navigate to sanntidssyre and type iex -S mix. In the iex> shell type Heis.start("some_unique_name", #PORT_NUMBER)
Make sure that it is the same port number as for the simulator. "Some_unique_name" can be any string as long as you don't have the same name for several elevators.

Repeat this for as many elevators as you would like, connection between them should commence automaticly when calling Heis.start.

### Some computers might have trouble defining nodes in-module. Try calling epmd -daemon in the terminal window before you start your elevators. ###

