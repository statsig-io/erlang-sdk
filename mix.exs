defmodule MyProject.MixProject do
  use Mix.Project

  def project() do
    [
      app: :statsig,
      version: "0.0.1",
      elixir: "~> 1.0",
      deps: deps(),
      description: description(),
      package: package(),
      name: "statsig",
      source_url: "https://github.com/statsig-io/erlang-sdk"
    ]
  end

  defp description() do
    "An erlang/elixir SDK for Statsig feature gates and experiments"
  end


  def application() do
    [
    ]
  end

  defp deps() do
    [
      {:hackney, "~> 1.18.1"},
      {:jiffy, "~> 1.1.1"},
      {:ex_doc, "~> 0.27", only: :dev, runtime: false},
    ]
  end

  defp package() do
    [
      # These are the default files included in the package
      files: ~w(mix.exs rebar.config LICENSE* README* src),
      licenses: ["ISC"],
      links: %{"GitHub" => "https://github.com/statsig-io/erlang-sdk"}
    ]
  end
end
