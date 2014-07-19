defmodule KV.ProcessStoreTest do

  use ExUnit.Case, async: true

  setup do
    {:ok, p_store} = KV.ProcessStore.start
    {:ok, p_client} = KV.ProcessStoreClient.start
    {:ok, p_store: p_store, p_client: p_client}
  end


  test "stores values by key" , %{p_store: p_store, p_client: p_client} do

    # assert KV.ProcessStoreClient.get(p_client,p_store, "milk") == nil
    # KV.ProcessStoreClient.put(p_client,p_store, "milk", 3)
    # assert KV.ProcessStoreClient.get(p_client, p_store, "milk") == 3

  end


  test "delete values by key" , %{p_store: p_store, p_client: p_client} do

    # assert KV.ProcessStoreClient.get(p_client, p_store, "milk") == nil
    # KV.ProcessStore.put(self(), p_store, "milk", 3)
    # assert KV.ProcessStoreClient.get(p_client, p_store, "milk") == 3
    # assert KV.ProcessStoreClient.delete(p_client, p_store, "milk") == 3
    # assert KV.v.get(p_client, p_store, "milk") == nil
    
  end


end