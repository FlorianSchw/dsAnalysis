#### for template builder


#### only possible in the testing workflow
#### see how the "D" object in the server-side look like
list_of_serverside_elements <- getDSLiteData(conns = conns,
                                             "D")

serverside_elements_single_study <- list_of_serverside_elements[[1]]
