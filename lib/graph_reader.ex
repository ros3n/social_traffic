defmodule GraphReader do
  def read(file) do
    case File.read(file) do
      {:ok, body} -> body
      {:error, reason} -> ""
    end
  end

  def parse_input(data) do
    [n | edges] = String.split(data, "\n")
    edges_p = edges
              |> Enum.filter(&(&1 !== ""))
              |> Enum.map(&(convert_to_pair(&1)))
    {n_p, _} = Integer.parse(n)

    {n_p, edges_p}
  end

  def convert_to_pair(str) do
    [x, y | _] = String.split(str, " ")
    {x_p, _}   = Integer.parse(x)
    {y_p, _}   = Integer.parse(y)

    {x_p, y_p}
  end
end
