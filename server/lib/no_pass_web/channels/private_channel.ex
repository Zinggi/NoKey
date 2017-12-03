defmodule NoPassWeb.PrivateChannel do
  use Phoenix.Channel

  # the lobby is where clients will find each other for pairing
  # def join("private:lobby", msg, socket) do
  #   sock = assign(socket, :uuid, msg["uuid"])
  #   {:ok, sock}
  # end

  # a private channel to a certain device
  # TODO: this isn't private yet, as there is no encyption, all someone needs to join the channel is the device_id
  def join("private:" <> device_id, msg, socket) do
    sock = assign(socket, :device_id, msg["deviceId"])
    {:ok, sock}
    # {:error, %{reason: "unauthorized"}}
  end

  # def handle_in("new_msg", %{"body" => body}, socket) do
  #   broadcast! socket, "new_msg", %{body: body, uuid: socket.assigns[:uuid]}
  #   {:noreply, socket}
  # end

end
