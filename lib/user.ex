defmodule User do
  use GenServer

  def start_link(id, friends, logger, opts \\ []) do
    GenServer.start_link __MODULE__, {id, friends, logger}, opts
  end

  def init({id, friends, logger}) do
    setup_action(self())
    {:ok, %{id: id, friends: friends, logger: logger}}
  end

  def handle_cast({:add_friend, friend}, %{id: id, friends: friends, logger: logger}) do
    GenEvent.sync_notify(logger, {:log, "#{id}, F, #{inspect(friend)}"})
    {:noreply, %{id: id, friends: [friend|friends], logger: logger}}
  end

  def handle_cast({:message, msg, from, visited}, state) do
    GenEvent.sync_notify(state.logger, {:log, "#{state.id}, R, \"#{msg}\", from: #{from}"})
    react(msg, state.id, [self()|visited], state.friends, state.logger)
    {:noreply, state}
  end

  def handle_cast(:action, state) do
    GenEvent.sync_notify(state.logger, {:log, "#{state.id}, CC"})
    react("hello #{state.id}", state.id, [self()], state.friends, state.logger)
    setup_action(self())
    {:noreply, state}
  end

  def handle_call(:logs, _from, state) do
    logs = GenEvent.call(state.logger, LoggerHandler, :messages)
    {:reply, logs, state}
  end

  defp react(msg, from, visited, friends, logger) do
    recipients = friends
    |> Enum.filter(fn {_, pid} -> Enum.find_value(visited, &(&1 != pid)) end)
    |> Enum.filter(fn _el -> :random.uniform(100) > 60 end)   # pytanie: czy ten random nie wykona się tu tylko raz...
    broadcast {msg, from, visited}, recipients, logger
  end

  defp broadcast(message, recipients, logger) do
    recipients |> Enum.map(&(send_message(message, &1, logger)))
  end

  defp send_message({msg, from, visited}, {id, pid}, logger) do
    GenEvent.sync_notify(logger, {:log, "#{from}, S, \"#{msg}\", to: #{id}"})
    GenServer.cast(pid, {:message, msg, from, visited})
  end

  defp setup_action(parent) do
    timeout = :random.uniform(30) * 1000
    spawn_link(fn ->
      :timer.sleep(timeout)
      GenServer.cast(parent, :action)
    end)
  end
end
