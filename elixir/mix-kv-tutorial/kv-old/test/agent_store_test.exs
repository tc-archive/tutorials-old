defmodule KV.AgentStoreTest do

  use ExUnit.Case, async: true

  setup do
    {:ok, a_store} = KV.AgentStore.start_link
    {:ok, a_store: a_store}
  end


  test "stores values by key" , %{a_store: a_store} do

    KV.AgentStore.get(a_store, "milk")
    assert KV.AgentStore.get(a_store, "milk") == nil

    KV.AgentStore.put(a_store, "milk", 3)
    assert KV.AgentStore.get(a_store, "milk") == 3

  end


  test "delete values by key" , %{a_store: a_store} do

    assert KV.AgentStore.get(a_store, "milk") == nil
    KV.AgentStore.put(a_store, "milk", 3)
    assert KV.AgentStore.get(a_store, "milk") == 3
    assert KV.AgentStore.delete(a_store, "milk") == 3
    assert KV.AgentStore.get(a_store, "milk") == nil
    
  end


end