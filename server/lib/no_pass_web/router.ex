defmodule NoPassWeb.Router do
  use NoPassWeb, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_flash
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/", NoPassWeb do
    pipe_through :browser # Use the default browser stack

    get "/", PageController, :index
  end

  # Other scopes may use custom stacks. 
  scope "/api", NoPassWeb do
    pipe_through :api

    post "/initPairing", PairingController, :init_pairing
    post "/sendMsgTo/:otherId", MessageForewardingController, :send_msg_to
    post "/pairWith", PairingController, :pair_with
  end
end
