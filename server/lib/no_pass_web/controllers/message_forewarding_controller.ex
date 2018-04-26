defmodule NoPassWeb.MessageForewardingController do
  use NoPassWeb, :controller

  def send_msg_to(conn, msg) do
    other_id = msg["otherId"]
    NoPassWeb.Endpoint.broadcast!("private:" <> other_id, "new_msg", Map.delete(msg, "otherId"))
    json(conn, %{status: "ok"})
  end
end
