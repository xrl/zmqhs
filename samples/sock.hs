sock <- ZC.connect_uri ext_uri typed_id
 sent <- ZC.send sock (Message typed_id [pack payload])
 putStrLn $ "Sent " ++ (show sent) ++ " bytes"