defmodule LoggerHandler do
  use GenEvent

  defp log_size, do: 100
  defp dump_size, do: 100

  def handle_event({:log, x}, {messages, file_handle}) do
    messages_upd = [x|messages]

    # dump messages to file if we exceed the limit:
    messages_final =
      if (length messages_upd) >= (dump_size + log_size) do
        # we want to keep the log_size messages available:
        {to_leave, to_dump} = Enum.split(messages_upd, log_size)
        dump_chunk = to_dump |> Enum.reverse
                             |> Enum.join("\n")
        IO.binwrite file_handle, dump_chunk

        to_leave
      else
        messages_upd
      end

    {:ok, {messages_final, file_handle}}
  end

  def handle_call(:messages, {messages, file_handle}) do
    {:ok, Enum.reverse(messages), {messages, file_handle}}
  end

end
