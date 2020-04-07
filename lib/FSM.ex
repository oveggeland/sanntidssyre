defmodule FSM do
  use GenServer
  require Logger

  #Constants
  def door_open_time, do: 1000

  #User API
  def start_link([elevatorpid]) do
    Process.register(elevatorpid, :elevpid)
    GenServer.start_link(__MODULE__, [:elevpid], name: __MODULE__)
  end

  def init([:elevpid]) do #structen er {floor, goal_floor, door_open, malfunction}
    spawn_link(fn -> run_FSM() end)
    {:ok, {3, 0, false, false}}
  end

  def get_state() do
    GenServer.call(__MODULE__, :get_state)
  end

  def update_floor(floor) do
    GenServer.cast(__MODULE__, {:update, floor})
  end

  def update_malfunction(malfunction) do
    GenServer.cast(__MODULE__, {:malfunction, malfunction})
  end

  #Call handles
  def handle_call(:get_state, _from, state) do
    {:reply, state, state}
  end

  #Cast handles
  def handle_cast({:update, floor}, state) do
    {old_floor, goal_floor, door_open, mal} = state
    Lights.update_floor_indicator(floor)

    direction = find_direction(old_floor, floor)
    malfunction = cond do
      direction != :stop && mal ->
        false
      true ->
        mal
    end

    new_state = cond do

      goal_floor == floor ->
        stop_and_pick_up_passengers(floor)
        {floor, :nil, true, malfunction}

      Orders.check_orders(floor, direction) == true ->
        stop_and_pick_up_passengers(floor)
        {floor, goal_floor, true, malfunction}

      true ->
        {floor, goal_floor, door_open, malfunction}
      end
    {:noreply, new_state}
  end

  def handle_cast(:run_FSM, state) do
    {current_floor, goal_floor, door_open, malfunction} = state

    new_goal_floor = cond do
      door_open == true -> #door open do nothing
        goal_floor

      goal_floor != :nil ->
        drive_elevator(find_direction(current_floor, goal_floor))
        goal_floor

      goal_floor == :nil && Orders.get_next_order() != :nil -> #State is idle
        {new_goal_floor, _} = Orders.get_next_order()
        GenServer.cast(Watchdog, :spawn_motor_watchdog)
        new_goal_floor

      true ->
        goal_floor
    end

    {:noreply, {current_floor, new_goal_floor, door_open, malfunction}}
  end

  def handle_cast(:open_door, state) do
    {floor,goal_floor, _, malfunction} = state
    Lights.set_door_open_light(:on)
    {:noreply, {floor, goal_floor, true, malfunction}}
  end

  def handle_cast(:close_door, state) do
    {floor,goal_floor, _, malfunction} = state
    Lights.set_door_open_light(:off)
    {:noreply, {floor, goal_floor, false, malfunction}}
  end

  def handle_cast({:malfunction, malfunction}, state) do
    {floor, goal_floor, door_open, _} = state
    {:noreply, {floor, goal_floor, door_open, malfunction}}
  end

  defp stop_and_pick_up_passengers(floor) do

    drive_elevator(:stop)

    open_and_close_door()

    Distributor.order_complete({floor, :cab}, Node.self())
    Distributor.order_complete({floor, :hall_up}, Node.self())
    Distributor.order_complete({floor, :hall_down}, Node.self())

    GenServer.cast(Watchdog, :kill_motor_watchdog)

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

  defp drive_elevator(direction) do
    Driver.set_motor_direction(:elevpid, direction)
  end

  defp open_and_close_door() do
    GenServer.cast(__MODULE__, :open_door)
    spawn(fn -> :timer.sleep(door_open_time()); GenServer.cast(__MODULE__, :close_door) end)
  end

  defp run_FSM() do
    GenServer.cast(__MODULE__, :run_FSM)
    Process.sleep(100)
    run_FSM()
  end


end
