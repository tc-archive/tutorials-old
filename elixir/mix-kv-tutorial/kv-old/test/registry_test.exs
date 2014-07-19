defmodule KV.RegistryTest do

  use ExUnit.Case, async: true

  setup do
    {:ok, registry} = KV.Registry.start_link
    {:ok, registry: registry}
  end

  test "spawns buckets", %{registry: registry} do

    assert KV.Registry.lookup(registry, "shopping") == :error

    KV.Registry.create(registry, "shopping")
    assert {:ok, bucket} = KV.Registry.lookup(registry, "shopping")

    KV.AgentStore.put(bucket, "milk", 1)
    assert KV.AgentStore.get(bucket, "milk") == 1

    assert KV.Registry.lookup(registry, "shopping") == :error

    # assert KV.Registry.stop(registry) == :ok

  end


  test "removes buckets on exit", %{registry: registry} do

    # KV.Registry.create(registry, "shopping")
    # {:ok, bucket} = KV.Registry.lookup(registry, "shopping")
    # Agent.stop(bucket)
    # assert KV.Registry.lookup(registry, "shopping") == :error
    
  end

end