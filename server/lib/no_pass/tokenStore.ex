defmodule NoPass.TokenStore do
  use GenServer

  ### API
  def start_link() do
    GenServer.start_link(__MODULE__, %{}, [name: __MODULE__])
  end

  def store(token, id) do
    GenServer.cast(__MODULE__, {:store, token, id})
  end

  def try_remove(token) do
    GenServer.call(__MODULE__, {:try_remove, token})
  end


  ### GenServer
  @polling_interval 21*1000 # ms
  @max_allowed_age 60 # s

  def init(state) do
    scheduleExpire()
    {:ok, state}
  end

  def handle_call({:try_remove, item}, _from, state) do
    {reply, newState} = Map.pop(state, item)
    {:reply, reply[:id], newState}
  end

  def handle_cast({:store, token, id}, state) do
    timestamp = DateTime.utc_now |> DateTime.to_unix
    {:noreply, Map.put(state, token, %{timestamp: timestamp, id: id})}
  end

  def handle_info(:expire_keys, state) do
    now = DateTime.utc_now |> DateTime.to_unix
    keys_to_drop = Enum.filter Map.keys(state), fn(ele) ->
      (now - Map.get(state, ele, %{timestamp: 0})[:timestamp]) >= @max_allowed_age
    end
    state = Map.drop(state, keys_to_drop)

    scheduleExpire()
    {:noreply, state}
  end

  defp scheduleExpire() do
    Process.send_after(self(), :expire_keys, @polling_interval)
  end

end


