defmodule Assertion do
  defmacro __using__(_options) do
    quote do
      import Assertion
      Module.register_attribute(__MODULE__, :tests, accumulate: true)
      @before_compile Assertion
    end
  end

  defmacro __before_compile__(_env) do
    quote do
      def run, do: Assertion.Test.run(Enum.reverse(@tests), __MODULE__)
    end
  end

  defmacro test(description, do: test_block) do
    test_fun = String.to_atom(description)

    quote do
      @tests unquote(test_fun)
      # cannot use bind_quoted here - see 'unquote fragments' doc
      def unquote(test_fun)(), do: unquote(test_block)
    end
  end

  defmacro assert({fun, _meta, [lhs, rhs]}) do
    etc = [Macro.escape(__CALLER__), Macro.to_string(lhs), Macro.to_string(rhs)]
    # or simply => quote bind_quoted: binding() do
    quote bind_quoted: [fun: fun, lhs: lhs, rhs: rhs, etc: etc] do
      Assertion.Test.assert(fun, lhs, rhs, etc)
    end
  end

  defmacro assert(ast) do
    etc = [Macro.escape(__CALLER__), Macro.to_string(ast)]
    # or simply => quote bind_quoted: binding() do
    quote bind_quoted: [ast: ast, etc: etc] do
      Assertion.Test.assert(ast, etc)
    end
  end
end

defmodule Assertion.Test do
  def run(tests, module) do
    Enum.each(tests, &apply(module, &1, []))
  end

  def assert(:==, lhs, rhs, _etc) when lhs == rhs do
    IO.write(".")
  end

  def assert(:==, lhs, rhs, [env, lhs_str, rhs_str]) do
    puts(
      {lhs, rhs},
      [env, lhs_str, rhs_str],
      {"Expected:      ", "to be equal to:"}
    )
  end

  def assert(:>, lhs, rhs, _etc) when lhs > rhs do
    IO.write(".")
  end

  def assert(:>, lhs, rhs, [env, lhs_str, rhs_str]) do
    puts(
      {lhs, rhs},
      [env, lhs_str, rhs_str],
      {"Expected:          ", "to be greater than:"}
    )
  end

  def assert(:is_function, fun, arity, _etc) when is_function(fun, arity) do
    IO.write(".")
  end

  def assert(:is_function, fun, arity, [env, fun_str, arity_str])
      when is_function(fun) do
    puts(
      {fun, arity},
      [env, fun_str, arity_str],
      {"Expected function:", "to be of arity:   "}
    )
  end

  def assert(:is_function, fun, arity, [env, fun_str, arity_str]) do
    puts(
      {fun, arity},
      [env, fun_str, arity_str],
      {"Expected expression:    ", "to be function of arity:"}
    )
  end

  def assert(ast, [env, ast_str]) when ast in [false, nil] do
    puts(
      {ast, nil},
      [env, ast_str, nil],
      {"Expected:   ", "to be truthy"}
    )
  end

  def assert(_ast, _etc) do
    IO.write(".")
  end

  defp puts({lhs, rhs}, [env, lhs_str, rhs_str], {lhs_intro, rhs_intro}) do
    {test_fun, _arity} = env.function

    lhs_ext =
      unless lhs_str == "#{inspect(lhs)}" or is_nil(lhs),
        do: " (= #{inspect(lhs)})"

    rhs_ext =
      unless rhs_str == "#{inspect(rhs)}" or is_nil(rhs),
        do: " (= #{inspect(rhs)})"

    failure_text = "FAILURE: #{test_fun}"

    IO.puts("""

    #{String.duplicate("=", String.length(failure_text))}
    failure_text
    #{String.duplicate("=", String.length(failure_text))}
    #{lhs_intro} #{lhs_str}#{lhs_ext}
    #{rhs_intro} #{rhs_str}#{rhs_ext}
    """)
  end
end

defmodule MathTest do
  use Assertion

  test "Tuple elements can be compared..." do
    tuple = {:foo, :bar, 3}
    assert elem(tuple, 1) == :bar
    nil_tuple = {nil}
    assert elem(nil_tuple, 0) |> is_nil()
  end
end

MathTest.run()
IO.puts("")
