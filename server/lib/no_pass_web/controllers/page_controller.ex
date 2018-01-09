defmodule NoPassWeb.PageController do
  use NoPassWeb, :controller

  def index(conn, _params) do
    render conn, "index.html"
  end

  def webApp(conn, _params) do
    render conn, "webApp.html"
  end
end
