defmodule FSM do
  use GenServer
  require Logger

  #Constants
  def door_open_time, do: 3000


  #User API
  def start_link([elevatorpid]) do
    GenServer.start_link(__MODULE__, [elevatorpid], name: __MODULE__)
  end

  def init([elevatorpid]) do #structen er {floor, goal_floor, direction, elevatorpid, door_open}
    {:ok, {:nil, :nil, :stop, elevatorpid, false}}
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
    Process.sleep(100)
    {floor,goal,dir,_,door} = get_state()
    Logger.info("#{floor}, #{goal}, #{dir}, #{door}")
    run_FSM()
  end


  #Call handles
  def handle_call(:get_state, _from, state) do
    {:reply, state, state}
  end

  #Cast handles
  def handle_cast({:update, floor}, state) do
    {_, old_goal_floor, old_direction, elevatorpid, door_open} = state

    #Driver.set_floor_indicator(elevatorpid, floor);
    Lights.update_floor_indicator(floor)

    #if Orders.check_orders(floor, old_direction) do
    #  Logger.info("Floor med samme retning")
    #  _pick_up_passengers(floor, old_direction)
    #end

    new_state = cond do

      old_goal_floor == floor ->
        _reach_goal_floor(elevatorpid, floor, door_open, old_direction)

      Orders.check_orders(floor, old_direction) == true ->
        _pick_up_passengers(floor, old_direction)
        {floor, old_goal_floor, :stop, elevatorpid, door_open}
      true ->
        {floor, old_goal_floor, old_direction, elevatorpid, door_open}
      end

    {:noreply, new_state}
  end

  def handle_cast({:update_goal, goal_floor}, state) do
    {floor, _, direction, elevatorpid, door_open} = state
    {:noreply, {floor, goal_floor, direction, elevatorpid, door_open}}
  end

  def handle_cast({:drive_elevator, direction}, state) do
    {floor, goal_floor,__, elevatorpid, door_open} = state
    Driver.set_motor_direction(elevatorpid, direction)
    {:noreply, {floor, goal_floor, direction, elevatorpid, door_open}}
  end

  def handle_cast(:run_FSM, state) do
    {current_floor, goal_floor, dir, elevatorpid, door_open} = state
    new_state = cond do
      door_open == true -> #door open do nothing
        {current_floor, goal_floor, dir, elevatorpid, door_open}
      current_floor == :nil -> #Uninitialized
        Logger.info("Uninit")
        Distributor.new_order({0, :cab})
        {3, 0, :down, elevatorpid, door_open}
      goal_floor != :nil ->
        {current_floor, goal_floor,_handle_new_order(goal_floor, current_floor), elevatorpid, door_open}
      goal_floor == :nil -> #State is idle
        if Orders.get_next_order != :nil do
          {new_goal_floor, _order_type} = Orders.get_next_order()
          #Orders.delete_order({new_goal_floor, order_type})
          {current_floor, new_goal_floor, _handle_new_order(new_goal_floor, current_floor), elevatorpid, door_open}
        else
          {current_floor, :nil, :nil, elevatorpid, door_open}
        end
      true ->
        {current_floor, goal_floor, dir, elevatorpid, door_open}
    end

    {:noreply, new_state}
  end

  def handle_cast(:open_door, state) do
    {floor,goal_floor, direction, elevatorpid, _} = state
    #Driver.set_door_open_light(elevatorpid, :on)
    Lights.update_door_open(:on)
    {:noreply, {floor, goal_floor, direction, elevatorpid, true}}
  end

  def handle_cast(:close_door, state) do
    {floor,goal_floor, direction, elevatorpid, _} = state
    #Driver.set_door_open_light(elevatorpid, :off)
    Lights.update_door_open(:off)
    {:noreply, {floor, goal_floor, direction, elevatorpid, false}}
  end


  def _open_and_close_door() do
    GenServer.cast(__MODULE__, :open_door)
    spawn(fn -> :timer.sleep(door_open_time()); GenServer.cast(__MODULE__, :close_door) end)
  end

  def _reach_goal_floor(elevatorpid, floor, door_open, direction) do
    Logger.info("At goal floor")
    drive_elevator(:stop)
    _open_and_close_door()
    Distributor.order_complete({floor, :cab})
    cond do
      direction == :stop ->
        Distributor.order_complete({floor, :hall_up})
        Distributor.order_complete({floor, :hall_down})
      direction == :up ->
        Distributor.order_complete({floor, :hall_up})
      true ->
        Distributor.order_complete({floor, :hall_down})
    end
    {floor, :nil, :stop, elevatorpid, door_open}
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
        Logger.info("Error")
    end
  end

  def _pick_up_passengers(floor, direction) do
    _open_and_close_door()
    drive_elevator(:stop)
    Distributor.order_complete({floor, :cab})
    cond do
      direction == :up ->
        Distributor.order_complete({floor, :hall_up})
      true ->
        Distributor.order_complete({floor, :hall_down})
    end
  end


#Denne kan vi legge et annet sted helst:)
  def initialize() do
    {:ok, heisPID} = Driver.start
    Driver.set_motor_direction(heisPID, :down)
    {:ok, heisPID}
  end
end
