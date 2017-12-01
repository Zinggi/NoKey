defmodule NoPassWeb.PrivateChannel do
  use Phoenix.Channel

  # the lobby is where clients will find each other for pairing
  def join("private:lobby", msg, socket) do
    sock = assign(socket, :uuid, msg["uuid"])
    {:ok, sock}
  end
  def join("private:" <> _private_room_id, _params, _socket) do
    {:error, %{reason: "unauthorized"}}
  end

  def handle_in("new_msg", %{"body" => body}, socket) do
    broadcast! socket, "new_msg", %{body: body, uuid: socket.assigns[:uuid]}
    {:noreply, socket}
  end

end
