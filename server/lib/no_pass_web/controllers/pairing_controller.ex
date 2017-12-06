defmodule NoPassWeb.PairingController do
  use NoPassWeb, :controller

  def random_string(length) do
    :crypto.strong_rand_bytes(length) |> Base.url_encode64 |> binary_part(0, length)
  end

  def init_pairing(conn, %{"deviceId" => dev_id, "syncData" => sync_data}) do
    token = MnemonicSlugs.generate_slug(4)
    NoPass.TokenStore.store(token, %{dev_id: dev_id, sync_data: sync_data } )
    json conn, %{ token: token }
  end

  def pair_with(conn, %{ "token" => token,"deviceId" => dev_id_a, "syncData" => sync_data_a }) do
    case NoPass.TokenStore.try_remove token do
      nil ->
        json conn, %{ error: "token expired" }

      %{:dev_id => dev_id_b, :sync_data => sync_data_b} ->
        # send to b the information of a
        NoPassWeb.Endpoint.broadcast!("private:" <> dev_id_b, "new_msg", %{type: "PairedWith", otherId: dev_id_a, syncData: sync_data_a})
        # send to a the info of b
        json conn, %{ otherId: dev_id_b, syncData: sync_data_b }
    end
  end

  def sync_with(conn, %{"syncData" => sync_data, "otherId" => other_id}) do
    NoPassWeb.Endpoint.broadcast!("private:" <> other_id, "new_msg", %{type: "SyncUpdate", syncData: sync_data})
    json conn, %{}
  end

end
