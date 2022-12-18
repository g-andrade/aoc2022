#!/usr/bin/env elixir

defmodule Solution do
  defmodule Matrix do
    @enforce_keys [:rows_and_cols, :height, :width]
    defstruct [:rows_and_cols, :height, :width]


    def from_text(text) do
      lines = String.split(text, ["\n", "\r"], _opts = [:trim])
      [first_line | _nth_lines] = lines
      rows_list = for line <- lines, line != "", do: line_to_row(line)
      rows_and_cols = List.to_tuple(rows_list)

      height = Kernel.tuple_size(rows_and_cols)
      width = String.length(first_line)

      %Matrix{
        rows_and_cols: rows_and_cols,
        height: height,
        width: width
      }
    end

    defp line_to_row(line) do
      List.to_tuple(Enum.map(
        String.to_charlist(line),
        fn char -> char - ?0 end))
    end

    defguardp is_within_bounds(value, min, max)
    when value >= min and value <= max

    defguardp are_coords_valid(row_idx, col_idx, height, width)
    when is_within_bounds(row_idx, 1, height)
      and is_within_bounds(col_idx, 1, width)


    def at(%Matrix{height: height, width: width} = matrix, row_idx, col_idx, _default)
      when are_coords_valid(row_idx, col_idx, height, width)
    do
      # IO.puts("got height #{height}, width #{width}, row_idx #{row_idx}, col_idx #{col_idx}")
      %Matrix{rows_and_cols: rows_and_cols} = matrix
      row = Kernel.elem(rows_and_cols, row_idx - 1)
      tree_height = Kernel.elem(row, col_idx - 1)
      # IO.puts("at row #{row_idx}, col #{col_idx}, tree height is #{tree_height}")
      tree_height
    end

    def at(%Matrix{}, _row_idx, _col_idx, default) do
      default
    end


    defimpl String.Chars, for: Matrix do
      def to_string(matrix) do
        inspect matrix
        # %Matrix{rows_and_cols: rows_and_cols} = matrix
        # rows_and_cols_list = Tuple.to_list(rows_and_cols)
        # Enum.map(rows_and_cols_list,
        #   fn row -> [List.to_string(Tuple.to_list(row)), "\n"] end)
      end
    end


	defimpl Enumerable, for: Matrix do
	  def count(%Matrix{height: height, width: width}) do
	    {:ok, width * height}
	  end

	  def member?(%Matrix{}, _tree_score) do
		{:error, __MODULE__}
	  end

	  def slice(%Matrix{}) do
        {:error, __MODULE__}
	  end

	  def reduce(%Matrix{rows_and_cols: rows_and_cols}, {:cont, acc}, fun) do
        rows_and_cols_list = Tuple.to_list(rows_and_cols)
        reduce_recur(rows_and_cols_list, _row_idx = 1, acc, fun)
	  end

      defp reduce_recur([row | next_rows], row_idx, acc, fun) do
        case reduce_row(row, row_idx, acc, fun) do
          {:cont, acc} ->
            reduce_recur(next_rows, row_idx + 1, acc, fun)
          other ->
            other
        end
      end

      defp reduce_recur([], _row_idx, acc, _fun) do
        {:done, acc}
      end

      defp reduce_row(row, row_idx, acc, fun) do
        cols_list = Tuple.to_list(row)
        reduce_row_recur(cols_list, row_idx, _col_idx = 1, acc, fun)
      end

      defp reduce_row_recur([tree_height | next], row_idx, col_idx, acc, fun) do
        case fun.({row_idx, col_idx, tree_height}, acc) do
          {:cont, acc} ->
            reduce_row_recur(next, row_idx, col_idx + 1, acc, fun)
          {:halt, acc} ->
            {:halted, acc}
        end
      end

      defp reduce_row_recur([], _row_idx, _col_Idx, acc, _fun) do
        {:cont, acc}
      end
	end
  end

  def run do
    text = IO.read :eof
    matrix = Matrix.from_text text
    IO.puts "matrix: #{matrix}"

    best_score = Enum.reduce(matrix, 0,
      fn {row_idx, col_idx, tree_height}, acc ->
        case calc_tree_scenic_score(row_idx, col_idx, tree_height, matrix) do
          better when better > acc ->
            better
          _ ->
            acc
        end
      end)

    IO.puts "max scenic score: #{best_score}"
  end

  def calc_tree_scenic_score(row_idx, col_idx, tree_height, matrix) do
    %Matrix{height: matrix_height, width: matrix_width} = matrix

    # IO.puts("\ncalculating for row #{row_idx}, col #{col_idx}, tree height #{tree_height}")
    up_range = (row_idx - 1)..min(1, row_idx - 1)
    down_range = (row_idx + 1)..max(matrix_height, row_idx + 1)
    left_range = (col_idx - 1)..min(1, col_idx - 1)
    right_range = (col_idx + 1)..max(matrix_width, col_idx + 1)

    horizontal_get = fn x -> Matrix.at(matrix, x, col_idx, _default = :out_of_bounds) end
    vertical_get = fn y -> Matrix.at(matrix, row_idx, y, _default = :out_of_bounds) end

    up_score = calc_tree_scenic_score_across_range(tree_height, up_range, horizontal_get)
    # IO.puts("up score: #{up_score}\n-----")

    down_score = calc_tree_scenic_score_across_range(tree_height, down_range, horizontal_get)
    # IO.puts("down score: #{down_score}\n------")

    left_score = calc_tree_scenic_score_across_range(tree_height, left_range, vertical_get)
    # IO.puts("left score: #{left_score}\n-----")

    right_score = calc_tree_scenic_score_across_range(tree_height, right_range, vertical_get)
    # IO.puts("right score: #{right_score}\n----")

    score = up_score * down_score * left_score * right_score
    # IO.puts("tree score: #{score}")
    score
  end

  defp calc_tree_scenic_score_across_range(min_height, range, get_fun) do
    Enum.reduce_while(range, _count_acc0 = 0,
      fn pos, count_acc ->
          # IO.puts("pos #{inspect pos}, acc #{inspect count_acc}")
        case get_fun.(pos) do
          :out_of_bounds ->
            {:halt, count_acc}
          lower when lower < min_height ->
              # IO.puts("got it")
            {:cont, count_acc + 1}
          same when same == min_height ->
              # IO.puts("blocked")
            {:halt, count_acc + 1}
          _higher ->
            {:halt, count_acc + 1}
        end
      end)
  end
end

Solution.run()
