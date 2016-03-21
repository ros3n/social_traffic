defmodule User do
  def start_link(friends, timeout) do
    spawn_link __MODULE__, :listen, [friends, timeout]
  end

  def listen(friends, timeout) do
    receive do
      {:add_friend, friend} ->
        listen [friend | friends], timeout
      {:message, msg} ->
        IO.puts "#{inspect self} received: #{msg}"
        react friends, msg
        listen friends, timeout
    after
      timeout ->
        create_content friends
        listen friends, timeout
    end
  end

  defp react(friends, msg) do
    mass_send friends, msg
  end

  defp create_content(friends) do
    mass_send friends, "hello from #{inspect self}"
  end

  defp mass_send(recipients, msg) do
    recipients |> Enum.filter(fn _el -> :random.uniform(100) > 60 end) |>
    Enum.map(&(send(&1, {:message, msg})))
  end
end
