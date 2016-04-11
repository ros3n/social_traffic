defmodule LoggerHandler do
  use GenEvent

  def handle_event({:log, x}, messages) do
    {:ok, [x|messages]}
  end

  def handle_call(:messages, messages) do
    {:ok, Enum.reverse(messages), messages}
  end
end
