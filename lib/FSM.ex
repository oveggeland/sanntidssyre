defmodule FSM do
  use GenServer

  #Constants
  def door_open_time, do: 3000


  #User API
  def start_link([elevatorpid]) do
    GenServer.start_link(__MODULE__, [elevatorpid], name: __MODULE__)
  end

  def init([elevatorpid]) do #structen er {floor(siste etasje), goal_floor, direction, elevatorpid}
    {:ok, {nil, nil, :stop, elevatorpid}}
  end

  def get_state() do
    GenServer.call(__MODULE__, :get_state)
  end

  def drive_elevator(direction) do
    GenServer.cast(__MODULE__, {:drive_elevator, direction})
  end

  def open_and_close_door(elevatorpid) do
    Driver.set_door_open_light(elevatorpid, :on)
    spawn(fn -> :timer.sleep(door_open_time()); close_door() end)
  end

  def update_floor(floor) do
    GenServer.cast(__MODULE__, {:update, floor})
    #Returning state to check if all good
    GenServer.call(__MODULE__, :get_state)
  end

  def update_goal_floor(floor) do
    GenServer.cast(__MODULE__, {:update_goal, floor})
    #Returning state to check if all good
    GenServer.call(__MODULE__, :get_state)
  end

  #Call handles
  def handle_call(:get_state, _from, state) do
    {:reply, state, state}
  end

  #Cast handles
  def handle_cast({:update, floor}, state) do
    {old_floor, old_goal_floor, old_direction, elevatorpid} = state
   # {_dontcare, goal_floor, direction, elevatorpid} = state
    Driver.set_floor_indicator(elevatorpid, floor);
    new_state = cond do
      old_goal_floor == floor ->
        IO.puts("At goal floor")
        drive_elevator(:stop)
        open_and_close_door(elevatorpid)
        #_clear_watchdog(order)
        {floor, :nil, :stop, elevatorpid}
      true ->
        IO.puts("Default")
        {floor, old_goal_floor, old_direction, elevatorpid}
      end
    #{:noreply, {floor, goal_floor, direction, elevatorpid}}
    {:noreply, new_state}
  end

  def handle_cast({:update_goal, goal_floor}, state) do
    {floor, _dontcare, direction, elevatorpid} = state
    {:noreply, {floor, goal_floor, direction, elevatorpid}}
  end

  def handle_cast({:drive_elevator, direction}, state) do
    {floor, goal_floor,__, elevatorpid} = state
    Driver.set_motor_direction(elevatorpid, direction)
    {:noreply, {floor, goal_floor, direction, elevatorpid}}
  end

  def handle_cast({:update_goal, goal_floor}, state) do
    {floor, _dontcare, direction, elevatorpid} = state
    {:noreply, {floor, goal_floor, direction, elevatorpid}}
  end

  def _clear_watchdog(order) do
    #Function is called when order is completed
    #Need to call a call to watchdog GenServer.
    #
    #orderComplete(self(), order)
  end

  def close_door do
    {_,_,_,elevatorpid} = get_state()
    Driver.set_door_open_light(elevatorpid, :off)
  end





#Denne kan vi legge et annet sted helst:)
  def initialize() do
    {:ok, heisPID} = Driver.start
    Driver.set_motor_direction(heisPID, :down)
    {:ok, heisPID}
  end
end
