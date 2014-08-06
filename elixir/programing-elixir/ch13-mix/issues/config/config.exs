# This file is responsible for configuring your application
# and its dependencies. The Mix.Config module provides functions
# to aid in doing so.
use Mix.Config

# Note this file is loaded before any dependency and is restricted
# to this project. If another project depends on this project, this
# file won't be loaded nor affect the parent project.

# Sample configuration:
#
#     config :my_dep,
#       key: :value,
#       limit: 42

# It is also possible to import configuration files, relative to this
# directory. For example, you can emulate configuration per environment
# by uncommenting the line below and defining dev.exs, test.exs and such.
# Configuration from the imported file will override the ones defined
# here (which is why it is important to import them last).
#
#     import_config "#{Mix.env}.exs"



# Each config line adds one or more key/value pairs to the given application’s _ environment‘. 
# If you have multiple lines for the same application they accu- mulate, with duplicate keys 
# in later lines overriding values from earlier ones.
#
# In our code, we use Application.get_env function to return a value from the envi- ronment.
#
#
config :issues, github_url: "https://api.github.com"