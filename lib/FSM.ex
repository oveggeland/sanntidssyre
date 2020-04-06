defmodule FSM do
  use GenServer
  require Logger

  #Constants
  def door_open_time, do: 3000


  #User API
  def start_link([elevatorpid]) do
    Process.register(elevatorpid, :elevpid)
    GenServer.start_link(__MODULE__, [:elevpid], name: __MODULE__)
  end

  def init([:elevpid]) do #structen er {floor, goal_floor, door_open}
    spawn_link(fn -> run_FSM() end)
    {:ok, {:nil, :nil, false}}
  end

  def get_state() do
    GenServer.call(__MODULE__, :get_state)
  end

  def drive_elevator(direction) do
    GenServer.cast(__MODULE__, {:drive_elevator, direction})
  end


  def update_floor(floor) do
    GenServer.cast(__MODULE__, {:update, floor})
  end

  def update_goal_floor(floor) do
    GenServer.cast(__MODULE__, {:update_goal, floor})
  end

  def run_FSM() do
    GenServer.cast(__MODULE__, :run_FSM)
    Process.sleep(100)
    run_FSM()
  end


  #Call handles
  def handle_call(:get_state, _from, state) do
    {:reply, state, state}
  end

  #Cast handles
  def handle_cast({:update, floor}, state) do
    {old_floor, old_goal_floor, door_open} = state

    Lights.update_floor_indicator(floor)

    direction = find_direction(old_floor, floor)
    new_state = cond do

      old_goal_floor == floor ->
        reach_goal_floor(:elevpid, floor, door_open, direction)

      Orders.check_orders(floor, direction) == true ->
        pick_up_passengers(floor, direction)
        {floor, old_goal_floor, door_open}
      true ->
        {floor, old_goal_floor, door_open}
      end

    {:noreply, new_state}
  end

  def handle_cast({:update_goal, goal_floor}, state) do
    {floor, _, door_open} = state
    {:noreply, {floor, goal_floor, door_open}}
  end

  def handle_cast({:drive_elevator, direction}, state) do
    {floor, goal_floor, door_open} = state
    Driver.set_motor_direction(:elevpid, direction)
    cond do
      direction == :up ->
        GenServer.cast(Watchdog, :spawn_motor_watchdog)
      direction == :down ->
        GenServer.cast(Watchdog, :spawn_motor_watchdog)
      direction == :stop ->
        GenServer.cast(Watchdog, :kill_motor_watchdog)
    end
    {:noreply, {floor, goal_floor, door_open}}
  end

  def handle_cast(:run_FSM, state) do
    {current_floor, goal_floor, door_open} = state
    new_state = cond do
      door_open == true -> #door open do nothing
        {current_floor, goal_floor, door_open}
      current_floor == :nil -> #Uninitialized
        Logger.info("Uninit")
        {3, 0, door_open}
      goal_floor != :nil ->
        handle_new_order(goal_floor, current_floor)
        {current_floor, goal_floor, door_open}
      goal_floor == :nil -> #State is idle
        if Orders.get_next_order != :nil do
          {new_goal_floor, _order_type} = Orders.get_next_order()
          handle_new_order(new_goal_floor, current_floor)
          {current_floor, new_goal_floor, door_open}
        else
          {current_floor, :nil, door_open}
        end
      true ->
        {current_floor, goal_floor, door_open}
    end

    {:noreply, new_state}
  end

  def handle_cast(:open_door, state) do
    {floor,goal_floor, _} = state
    Lights.update_door_open(:on)
    {:noreply, {floor, goal_floor, true}}
  end

  def handle_cast(:close_door, state) do
    {floor,goal_floor, _} = state
    Lights.update_door_open(:off)
    {:noreply, {floor, goal_floor, false}}
  end


  def _open_and_close_door() do
    GenServer.cast(__MODULE__, :open_door)
    spawn(fn -> :timer.sleep(door_open_time()); GenServer.cast(__MODULE__, :close_door) end)
  end

  defp reach_goal_floor(:elevpid, floor, door_open, direction) do
    Logger.info("At goal floor")
    drive_elevator(:stop)
    _open_and_close_door()
    Distributor.order_complete({floor, :cab}, Node.self())
    cond do
      direction == :stop ->
        Distributor.order_complete({floor, :hall_up}, Node.self())
        Distributor.order_complete({floor, :hall_down}, Node.self())
      direction == :up ->
        Distributor.order_complete({floor, :hall_up}, Node.self())
      true ->
        Distributor.order_complete({floor, :hall_down}, Node.self())
    end
    {floor, :nil, door_open}
  end

  defp handle_new_order(new_goal_floor, current_floor) do
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

  defp pick_up_passengers(floor, direction) do
    _open_and_close_door()
    drive_elevator(:stop)
    Distributor.order_complete({floor, :cab}, Node.self())
    cond do
      direction == :up ->
        Distributor.order_complete({floor, :hall_up}, Node.self())
      true ->
        Distributor.order_complete({floor, :hall_down}, Node.self())
    end
  end


  defp find_direction(old_floor, new_floor) do
    cond do
      old_floor == new_floor ->
        :stop
      old_floor > new_floor ->
        :down
      old_floor < new_floor ->
        :up
    end
  end
end
