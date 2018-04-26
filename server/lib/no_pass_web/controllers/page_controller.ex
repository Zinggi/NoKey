defmodule NoPassWeb.PageController do
  use NoPassWeb, :controller

  def index(conn, _params) do
    render(conn, "index.html")
  end

  def webApp(conn, _params) do
    redirect(conn, to: "/main.html")
    # conn
    # |> put_resp_header("content-type", "text/html; charset=utf-8")
    # |> Plug.Conn.send_file(200, Application.app_dir(:no_pass, "priv/static/main.html"))
  end
end
