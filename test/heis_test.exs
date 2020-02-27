defmodule HEISTest do
  use ExUnit.Case
  doctest HEIS

  test "greets the world" do
    assert HEIS.hello() == :world
  end
end
