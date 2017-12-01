defmodule NoPassWeb.PairingController do
  use NoPassWeb, :controller

  def random_string(length) do
    :crypto.strong_rand_bytes(length) |> Base.url_encode64 |> binary_part(0, length)
  end

  def new_pairing_token(conn, _params) do
    token = MnemonicSlugs.generate_slug(4)
    NoPass.TokenStore.store(token)
    json conn, %{ token: token }
  end

  def pair_with(conn, %{ "token" => token }) do
    case NoPass.TokenStore.try_remove token do
      nil ->
        json conn, %{ error: "token expired" }

      val ->
        json conn, %{ success: val }
    end
  end
end
