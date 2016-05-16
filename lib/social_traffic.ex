defmodule SocialTraffic do
  @logger_cap 10000

  def main(input_file) do
    start(input_file)
    receive do
      :exit -> "Exiting now!"
    end
  end

  def start(file) do
    {n, edges} = GraphReader.read(file)
              |> GraphReader.parse_input
    user_refs = start_users(n, %{})
    add_friendships(edges, user_refs)
    user_refs
  end

  def start_users(n, user_refs, logger_pid \\ nil)

  def start_users(0, user_refs, _logger_pid) do
    user_refs
  end

  def start_users(n, user_refs, logger_pid) do
    logger_pid = start_logger(n, logger_pid)
    {:ok, pid} = User.start_link(n - 1, [], logger_pid)
    # IO.puts "#{n - 1}: #{inspect(pid)}"
    start_users(n - 1, Map.put(user_refs, n - 1, pid), logger_pid)
  end

  def add_friendships(edges, user_refs) do
    Enum.each(edges, fn({x, y}) -> add_friendship(x, y, user_refs) end)
  end

  def add_friendship(x, y, user_refs) do
    ux = user_refs[x]
    uy = user_refs[y]

    GenServer.cast(ux, {:add_friend, {y, uy}})
    GenServer.cast(uy, {:add_friend, {x, ux}})
  end

  defp open_logfile(n) do
    log_name = Path.join(".logs", "#{n-1}.log")
    {:ok, logfile} = File.open log_name, [:write]

    logfile
  end

  defp start_logger(n, nil) do
    {:ok, pid} = GenEvent.start_link([])
    logfile = open_logfile n
    GenEvent.add_handler(pid, LoggerHandler, {[], logfile})
    pid
  end

  defp start_logger(n, _) when rem(n, @logger_cap) == 0 do
    start_logger(n, nil)
  end

  defp start_logger(_, pid) do
    pid
  end
end
