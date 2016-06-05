defmodule User do
  use GenServer

  def start_link(id, friends, logger, opts \\ []) do
    GenServer.start_link __MODULE__, {id, friends, logger}, opts
  end

  def init({id, friends, logger}) do
    {:ok, %{id: id, friends: friends, logger: logger}}
  end

  def handle_cast({:add_friend, friend}, %{id: id, friends: friends, logger: logger}) do
    {:noreply, %{id: id, friends: [friend|friends], logger: logger}}
  end

  def handle_cast({:message, msg, from, visited}, state) do
    msg_qual = rand_msg_qual
    # GenEvent.notify(state.logger, {:log, chrono_log("#{state.id}, R, \"#{msg}\", from: #{from}")})
    setup_reaction(msg, msg_qual, state.id, [self()|visited], state.friends, state.logger)
    {:noreply, state}
  end

  def handle_cast(:action, state) do
    msg_qual = rand_msg_qual
    # GenEvent.notify(state.logger, {:log, chrono_log("#{state.id}, CC, qual: #{msg_qual}")})
    react("hello #{state.id}", msg_qual, state.id, [self()], state.friends, state.logger)
    setup_action(self())
    {:noreply, state}
  end

  def handle_cast(:start, state) do
    setup_action(self())
    {:noreply, state}
  end

  def handle_call(:logs, _from, state) do
    logs = GenEvent.call(state.logger, LoggerHandler, :messages)
    {:reply, logs, state}
  end

  defp react(msg, msg_qual, from, visited, friends, logger) do
    recipients = friends
      |> Enum.filter(fn {_, pid} -> !Enum.find_value(visited, &(&1 == pid)) end)
      |> Enum.filter(fn _el -> :random.uniform * msg_qual > 0.65 end)
    broadcast {msg, from, visited}, recipients, logger
  end

  defp broadcast(message, recipients, logger) do
    stamp = :erlang.system_time
    recipients |> Enum.map(&(send_message(message, &1, stamp)))
  end

  defp send_message({msg, from, visited}, {id, pid}, stamp) do
    # GenEvent.notify(logger, {:log, chrono_log("#{from}, S, \"#{msg}\", to: #{id}")})
    GenServer.cast(pid, {:message, msg, from, visited})
    stamp
  end

  defp setup_action(parent) do
    timeout = :random.uniform(30) * 1000
    spawn_link(fn ->
      :timer.sleep(timeout)
      GenServer.cast(parent, :action)
    end)
  end

  defp setup_reaction(msg, msg_qual, state_id, visited, friends, logger) do
    timeout = (1 + :random.uniform(4)) * 1000
    spawn_link(fn ->
      :timer.sleep(timeout)
      react(msg, msg_qual, state_id, visited, friends, logger)
    end)
  end

  defp chrono_log(log) do
    unix = "#{:erlang.system_time}"
    unix <> ", " <> log
  end

  defp rand_msg_qual do
    :random.uniform / 2 + 0.5
  end
end
