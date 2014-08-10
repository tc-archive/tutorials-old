defmodule Issues.Mixfile do
  use Mix.Project


  # Starting Project 
  #
  # Mix:  iex -S mix
  # 
  def project do
    [app: :issues,
      version: "0.0.1",
      name: "Issues",
      source_url: "https://github.com/TempleCloud/tutorials/tree/master/elixir/programing-elixir/ch13-mix/issues",
      # elixir: "~> 0.14.3",
      elixir: "~>0.15.0-dev",
      escript: escript_config,
      deps: deps]
  end



  # Starting OTP Services
  # 
  # OTP is the framework that manages suites of running applications. The app function is used 
  # to configure the contents of these suites.
  # 
  # For example, here mix is informed to start 'HTTPotion' (instead of calling 'HTTPotion.start')
  # from somewhere in the code.
  #
  #

  # Configuration for the OTP application
  #
  # Type `mix help compile.app` for more information
  def application do
    [
      applications: [:httpotion]
    ]
  end


  # Finding and Adding Modules and Librarys
  #
  # 1) Elixir Libs    : http://elixir-lang.org/docs/stable
  # 2) Erlang Libs    : http://erlang.org/doc (Left-hand sidebar for Application Groups).
  # 3) Hex Packages   : https://hex.pm
  # 4) GitHub Repos   : https://github.com/
  # 5) Expm Packages  : http://expm (DEPRECATED)
  #
  # Add to 'deps' function and run 'mix deps'/'mix deps.get'/'mix deps.compile' etc.
  # If successful, the downloaded modules are stored in the 'deps' folder. 
  #
  # The 'mix.lock' file records the Git hash of each library. This means that at any point 
  # in the future you can get the exact version of the library used now.
  #
  # NB: 'rebar' installation may be requested.
  #
  #

  # Dependencies can be hex.pm packages:
  #
  #   {:mydep, "~> 0.3.0"}
  #
  # Or git/path repositories:
  #
  #   {:mydep, git: "https://github.com/elixir-lang/mydep.git", tag: "0.1"}
  #
  # Type `mix help deps` for more examples and options
  defp deps do
    [
      {:httpotion, github: "myfreeweb/httpotion"},  # https://github.com/myfreeweb/httpotion
      {:jsx, github: "talentdeficit/jsx"},
      {:ex_doc, github: "elixir-lang/ex_doc"},       # ExDoc dependency
      {:earmark, ">= 0.1.9"}
    ]
  end


  # Mix can package our code, along with its dependencies, into a single file that can be 
  # run on any Unix-based platform. This makes use of Erlang’s escript utility, which can run 
  # precompiled programs stored as a zip archive. In our case, the program will be run as 
  # issues.
  #
  # > mix escript.build (mix escriptize)
  #
  # Ensure main() method in Issues.CLI
  #
  defp escript_config do 
    [ main_module: Issues.CLI ]
  end

end
