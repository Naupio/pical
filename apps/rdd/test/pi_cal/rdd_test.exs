defmodule PiCal.RDDTest do
  use ExUnit.Case
  doctest PiCal.RDD

  test "greets the world" do
    assert PiCal.RDD.hello() == :world
  end
end
