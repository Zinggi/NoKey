defmodule NoPassWeb.PairingController do
  use NoPassWeb, :controller

  def random_string(length) do
    :crypto.strong_rand_bytes(length) |> Base.url_encode64 |> binary_part(0, length)
  end

  def init_pairing(conn, %{"deviceId" => dev_id}) do
    token = MnemonicSlugs.generate_slug(4)
    NoPass.TokenStore.store(token, dev_id)
    json conn, %{ token: token }
  end

  def pair_with(conn, %{ "token" => token, "deviceId" => dev_id }) do
    case NoPass.TokenStore.try_remove token do
      nil ->
        json conn, %{ error: "token expired" }

      val ->
        json conn, %{ otherId: val }
    end
  end
end
