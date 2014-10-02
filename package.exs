defmodule Trie.Mixfile do
  use Mix.Project

  def project do
    [app: :trie,
     version: "1.3.3",
     description: description,
     package: package,
     deps: deps]
  end

  defp deps do
  end

  defp description do
    "Erlang Trie Implementation"
  end

  defp package do
    [files: ~w(src doc generate_docs.sh rebar.config README.markdown),
     contributors: ["Michael Truog"],
     licenses: ["BSD"],
     links: %{"GitHub" => "https://github.com/okeuday/trie"}]
   end
end
