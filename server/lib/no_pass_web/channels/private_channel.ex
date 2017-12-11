defmodule NoPassWeb.PrivateChannel do
  use Phoenix.Channel

  # a private channel to a certain device
  # TODO: this isn't private yet, as there is no encyption, all someone needs to join the channel is the device_id
  def join("private:" <> device_id, msg, socket) do
    sock = assign(socket, :device_id, msg["deviceId"])
    {:ok, sock}
    # {:error, %{reason: "unauthorized"}}
  end
end
