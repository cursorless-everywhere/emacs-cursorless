tag: user.command_client
-
simple test:
  print("in a command client")
  user.run_rpc_command_and_wait("butterfly")

where is command server:
  insert(user.command_server_directory())
