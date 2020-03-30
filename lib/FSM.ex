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
    {:ok, {:nil, :nil, :stop, elevatorpid}}
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
    {floor,goal,dir,_} = get_state()
    Logger.info("#{floor}, #{goal}, #{dir}")
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

    if Orders.check_orders(floor, old_direction) do
      Logger.info("Floor med samme retning")
      _pick_up_passengers(elevatorpid, floor, old_direction)
    end

    new_state = cond do
      old_goal_floor == floor ->
        _reach_goal_floor(elevatorpid, floor)
      #Orders.check_orders(floor, old_direction) == true ->
        #IO.puts("Kom til floor med ordre i samme retning")
      true ->
        {floor, old_goal_floor, old_direction, elevatorpid}
      end

    {:noreply, new_state}
  end

  def handle_cast({:update_goal, goal_floor}, state) do
    {floor, _, direction, elevatorpid} = state
    {:noreply, {floor, goal_floor, direction, elevatorpid}}
  end

  def handle_cast({:drive_elevator, direction}, state) do
    {floor, goal_floor,__, elevatorpid} = state
    Driver.set_motor_direction(elevatorpid, direction)
    {:noreply, {floor, goal_floor, direction, elevatorpid}}
  end

  def handle_cast(:run_FSM, state) do
    {current_floor, goal_floor, dir, elevatorpid} = state
    new_state = cond do
      current_floor == :nil -> #Uninitialized
        Logger.info("Uninit")
        {:nil, 0, _handle_new_order(0, 3), elevatorpid}
      goal_floor == :nil -> #State is idle
        if Orders.get_next_order != :nil do
          {new_goal_floor, order_type} = Orders.get_next_order()
          Orders.delete_order({new_goal_floor, order_type})
          {current_floor, new_goal_floor, _handle_new_order(new_goal_floor, current_floor), elevatorpid}
        else
          {current_floor, :nil, :nil, elevatorpid}
        end
      true ->
        :timer.sleep(200)
        {current_floor, goal_floor, dir, elevatorpid}
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

  def _pick_up_passengers(elevatorpid, floor, direction) do
    _open_and_close_door(elevatorpid)
    Orders.delete_order({floor, :cab})
    if direction == :up do
      Orders.delete_order({floor, :hall_up})
    else
     Orders.delete_order({floor, :hall_down})
    end
  end


#Denne kan vi legge et annet sted helst:)
  def initialize() do
    {:ok, heisPID} = Driver.start
    Driver.set_motor_direction(heisPID, :down)
    {:ok, heisPID}
  end
end
