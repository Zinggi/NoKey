defmodule NoPass.TokenStore do
  use GenServer

  ### API
  def start_link() do        
    GenServer.start_link(__MODULE__, %{}, [name: __MODULE__])
  end

  def store(token) do
    GenServer.cast(__MODULE__, {:store, token})
  end
  
  def try_remove(token) do
    GenServer.call(__MODULE__, {:try_remove, token})
  end


  ### GenServer
  @polling_interval 65*1000 # ms
  @max_allowed_age 60 # s

  def init(state) do
    scheduleExpire()
    {:ok, state}
  end

  def handle_call({:try_remove, item}, _from, state) do
    {reply, newState} = Map.pop(state, item)
    {:reply, reply, newState}
  end

  def handle_cast({:store, item}, state) do
    timestamp = DateTime.utc_now |> DateTime.to_unix 
    {:noreply, Map.put(state, item, timestamp)}
  end

  def handle_info(:expire_keys, state) do
    now = DateTime.utc_now |> DateTime.to_unix 
    drop_keys = Enum.filter(Map.keys(state), &( (now - Map.get(state, &1, 0)) >= @max_allowed_age))
    state = Map.drop(state, drop_keys)

    scheduleExpire()
    {:noreply, state}
  end

  defp scheduleExpire() do
    Process.send_after(self(), :expire_keys, @polling_interval)
  end

end


