defmodule FSM do
  use GenServer

  #Constants
  def door_open_time, do: 3000


  #User API
  def start_link([heispid]) do
    GenServer.start_link(__MODULE__, [heispid], name: __MODULE__)
  end

  def init([heispid]) do #structen vår er {floor(siste etasje), goal_floor, direction, heispid}
    {:ok, {nil, nil, :stop, heispid}}
  end

  def get_state() do
    GenServer.call(__MODULE__, :get_state)
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
    {old_floor, old_goal_floor, old_direction, heispid} = state
   # {_dontcare, goal_floor, direction, heispid} = state
    Driver.set_floor_indicator(heispid, floor);
    new_state = cond do
      old_goal_floor == floor ->
        IO.puts("At goal floor")
        Driver.set_motor_direction(heispid, :stop)
        Driver.set_door_open_light(heispid, :on)
        spawn(fn -> :timer.sleep(door_open_time()); close_door() end)
        #_clear_watchdog(order)
        {floor, :nil, :stop, heispid}
      true ->
        IO.puts("Default")
        {floor, old_goal_floor, old_direction, heispid}
      end
    #{:noreply, {floor, goal_floor, direction, heispid}}
    {:noreply, new_state}
  end

  def handle_cast({:update_goal, goal_floor}, state) do
    {floor, _dontcare, direction, heispid} = state
    {:noreply, {floor, goal_floor, direction, heispid}}
  end

  def _clear_watchdog(order) do
    #Function is called when order is completed
    #Need to call a call to watchdog GenServers.
    #orderComplete(self(), order)
  end

  def close_door do
    {_,_,_,heispid} = get_state()
    Driver.set_door_open_light(heispid, :off)
  end

  def drive(direction) do
    {_,_,_,heispid} = get_state()
    Driver.set_motor_direction(heispid, direction)
  end



#Denne kan vi legge et annet sted helst:)
  def initialize() do
    {:ok, heisPID} = Driver.start
    Driver.set_motor_direction(heisPID, :down)
    {:ok, heisPID}
  end
end
