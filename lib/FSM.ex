defmodule FSM do
  use GenServer
  require Logger

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

  def run_FSM() do
    GenServer.cast(__MODULE__, :run_FSM)
    Process.sleep(1000)
    Logger.info("Tic")
    run_FSM()
  end


  #Call handles
  def handle_call(:get_state, _from, state) do
    {:reply, state, state}
  end

  #Cast handles
  def handle_cast({:update, floor}, state) do
    {_, old_goal_floor, old_direction, elevatorpid} = state
    Driver.set_floor_indicator(elevatorpid, floor);
    new_state = cond do
      old_goal_floor == floor ->
        _reach_goal_floor(elevatorpid, floor)
      true ->
        Logger.info("Default")
        {floor, old_goal_floor, old_direction, elevatorpid}
      end
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

  def handle_cast(:run_FSM, state) do
    {current_floor, goal_floor, _, elevatorpid} = state
    new_state = cond do
      goal_floor == :nil -> #State is idle
        #new_order = get_order()
        #new_goal_floor = get_floor_from_order(order)
        new_goal_floor = 2
        new_direction =_handle_new_order(new_goal_floor, current_floor)
        {current_floor, new_goal_floor, new_direction, elevatorpid}
    end
    {:noreply, new_state}
  end

  def _clear_watchdog(order) do
    #Function is called when order is completed
    #Need to call a call to watchdog GenServer.
    #
    #orderComplete(self(), order)
  end

  def _close_door do
    {_,_,_,elevatorpid} = get_state()
    Driver.set_door_open_light(elevatorpid, :off)
  end

  def _open_and_close_door(elevatorpid) do
    Driver.set_door_open_light(elevatorpid, :on)
    spawn(fn -> :timer.sleep(door_open_time()); _close_door() end)
  end

  def _reach_goal_floor(elevatorpid, floor) do
    Logger.info("At goal floor")
    drive_elevator(:stop)
    _open_and_close_door(elevatorpid)
    #_clear_watchdog(order)
    {floor, :nil, :stop, elevatorpid}
  end

  def _handle_new_order(new_goal_floor, current_floor) do
    cond do
      new_goal_floor > current_floor ->
        drive_elevator(:up)
        :up
      new_goal_floor < current_floor ->
        drive_elevator(:down)
        :down
      new_goal_floor == current_floor ->
        :stop
        #Do nothing
      true ->
        Logger.info("bu")
    end
  end


#Denne kan vi legge et annet sted helst:)
  def initialize() do
    {:ok, heisPID} = Driver.start
    Driver.set_motor_direction(heisPID, :down)
    {:ok, heisPID}
  end
end
