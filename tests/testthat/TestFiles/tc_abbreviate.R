expected <- structure("abbrvt", .Names = "abbreviate")
test(id=0, code={
  abbreviate(minlength=6,names.arg="abbreviate")
},  o = expected);

expected <- structure(c("abcd", "ef", "abce"), .Names = c("abcd", "efgh", 
                                                          "abce"))
test(id=1, code={
  abbreviate(minlength=2,names.arg=c("abcd", "efgh", "abce"))
},  o = expected);

expected <- structure(c("ab", "ef", "ab"), .Names = c("abcd", "efgh", "abce"
))
test(id=2, code={
  abbreviate(minlength=2,names.arg=c("abcd", "efgh", "abce"),strict=TRUE)
},  o = expected);

expected <- structure(c("Alb", "Als", "Arz", "Ark", "Clf", "Clr", "Cn", "Dl", 
                        "Fl", "Gr", "Hw", "Id", "Il", "In", "Iw", "Kns", "Knt", "Ls", 
                        "Man", "Mr", "Mssc", "Mc", "Mnn", "Msss", "Mssr", "Mnt", "Nb", 
                        "Nv", "NH", "NJ", "NM", "NY", "NC", "ND", "Oh", "Ok", "Or", "Pn", 
                        "RI", "SC", "SD", "Tn", "Tx", "Ut", "Vrm", "Vrg", "Wsh", "WV", 
                        "Wsc", "Wy"), .Names = c("Alabama", "Alaska", "Arizona", "Arkansas", 
                                                 "California", "Colorado", "Connecticut", "Delaware", "Florida", 
                                                 "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", 
                                                 "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", 
                                                 "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", 
                                                 "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", 
                                                 "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", 
                                                 "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", 
                                                 "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", 
                                                 "West Virginia", "Wisconsin", "Wyoming"))
test(id=3, code={
  abbreviate(minlength=2,names.arg=c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", 
                                     "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", 
                                     "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", 
                                     "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", 
                                     "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", 
                                     "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", 
                                     "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", 
                                     "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", 
                                     "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", 
                                     "Wyoming"))
},  o = expected);

expected <- structure(c("Alb", "Als", "Arz", "Ark", "Clf", "Clr", "Cnn", 
                        "Dlw", "Flr", "Grg", "Haw", "Idh", "Ill", "Ind", "Iow", "Kns", 
                        "Knt", "Lsn", "Man", "Mry", "Mss", "Mch", "Mnn", "Mss", "Mss", 
                        "Mnt", "Nbr", "Nvd", "NwH", "NwJ", "NwM", "NwY", "NrC", "NrD", 
                        "Ohi", "Okl", "Org", "Pnn", "RhI", "StC", "StD", "Tnn", "Txs", 
                        "Uth", "Vrm", "Vrg", "Wsh", "WsV", "Wsc", "Wym"), .Names = c("Alabama", 
                                                                                     "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", 
                                                                                     "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", 
                                                                                     "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", 
                                                                                     "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", 
                                                                                     "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", 
                                                                                     "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", 
                                                                                     "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", 
                                                                                     "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", 
                                                                                     "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", 
                                                                                     "Wyoming"))
test(id=4, code={
  abbreviate(minlength=3,names.arg=c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", 
                                     "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", 
                                     "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", 
                                     "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", 
                                     "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", 
                                     "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", 
                                     "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", 
                                     "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", 
                                     "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", 
                                     "Wyoming"),strict=TRUE)
},  o = expected);

expected <- structure(c("Al", "Aa", "Ar", "As", "Cl", "Co", "Cn", "Dl", "Fl", 
                        "Gr", "Hw", "Id", "Il", "In", "Iw", "Kn", "Ky", "Ls", "Mn", "Mr", 
                        "Ms", "Mc", "Mnn", "Mss", "Mri", "Mnt", "Nb", "Nv", "NH", "NJ", 
                        "NM", "NY", "NC", "ND", "Oh", "Ok", "Or", "Pn", "RI", "SC", "SD", 
                        "Tn", "Tx", "Ut", "Vr", "Va", "Ws", "WV", "Wn", "Wy"), .Names = c("Alabama", 
                                                                                          "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", 
                                                                                          "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", 
                                                                                          "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", 
                                                                                          "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", 
                                                                                          "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", 
                                                                                          "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", 
                                                                                          "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", 
                                                                                          "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", 
                                                                                          "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", 
                                                                                          "Wyoming"))
test(id=5, code={
  abbreviate(method="both",minlength=2,names.arg=c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", 
                                                   "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", 
                                                   "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", 
                                                   "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", 
                                                   "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", 
                                                   "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", 
                                                   "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", 
                                                   "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", 
                                                   "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", 
                                                   "Wyoming"))
},  o = expected);

expected <- structure("abbrvt", .Names = "abbreviate")
test(id=0, code={
  abbreviate(minlength=6,names.arg="abbreviate")
},  o = expected);

expected <- structure(c("abcd", "ef", "abce"), .Names = c("abcd", "efgh", 
                                                          "abce"))
test(id=1, code={
  abbreviate(minlength=2,names.arg=c("abcd", "efgh", "abce"))
},  o = expected);

expected <- structure(c("ab", "ef", "ab"), .Names = c("abcd", "efgh", "abce"
))
test(id=2, code={
  abbreviate(minlength=2,names.arg=c("abcd", "efgh", "abce"),strict=TRUE)
},  o = expected);

expected <- structure(c("Alb", "Als", "Arz", "Ark", "Clf", "Clr", "Cn", "Dl", 
                        "Fl", "Gr", "Hw", "Id", "Il", "In", "Iw", "Kns", "Knt", "Ls", 
                        "Man", "Mr", "Mssc", "Mc", "Mnn", "Msss", "Mssr", "Mnt", "Nb", 
                        "Nv", "NH", "NJ", "NM", "NY", "NC", "ND", "Oh", "Ok", "Or", "Pn", 
                        "RI", "SC", "SD", "Tn", "Tx", "Ut", "Vrm", "Vrg", "Wsh", "WV", 
                        "Wsc", "Wy"), .Names = c("Alabama", "Alaska", "Arizona", "Arkansas", 
                                                 "California", "Colorado", "Connecticut", "Delaware", "Florida", 
                                                 "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", 
                                                 "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", 
                                                 "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", 
                                                 "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", 
                                                 "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", 
                                                 "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", 
                                                 "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", 
                                                 "West Virginia", "Wisconsin", "Wyoming"))
test(id=3, code={
  abbreviate(minlength=2,names.arg=c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", 
                                     "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", 
                                     "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", 
                                     "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", 
                                     "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", 
                                     "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", 
                                     "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", 
                                     "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", 
                                     "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", 
                                     "Wyoming"))
},  o = expected);

expected <- structure(c("Alb", "Als", "Arz", "Ark", "Clf", "Clr", "Cnn", 
                        "Dlw", "Flr", "Grg", "Haw", "Idh", "Ill", "Ind", "Iow", "Kns", 
                        "Knt", "Lsn", "Man", "Mry", "Mss", "Mch", "Mnn", "Mss", "Mss", 
                        "Mnt", "Nbr", "Nvd", "NwH", "NwJ", "NwM", "NwY", "NrC", "NrD", 
                        "Ohi", "Okl", "Org", "Pnn", "RhI", "StC", "StD", "Tnn", "Txs", 
                        "Uth", "Vrm", "Vrg", "Wsh", "WsV", "Wsc", "Wym"), .Names = c("Alabama", 
                                                                                     "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", 
                                                                                     "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", 
                                                                                     "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", 
                                                                                     "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", 
                                                                                     "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", 
                                                                                     "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", 
                                                                                     "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", 
                                                                                     "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", 
                                                                                     "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", 
                                                                                     "Wyoming"))
test(id=4, code={
  abbreviate(minlength=3,names.arg=c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", 
                                     "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", 
                                     "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", 
                                     "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", 
                                     "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", 
                                     "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", 
                                     "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", 
                                     "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", 
                                     "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", 
                                     "Wyoming"),strict=TRUE)
},  o = expected);

expected <- structure(c("Al", "Aa", "Ar", "As", "Cl", "Co", "Cn", "Dl", "Fl", 
                        "Gr", "Hw", "Id", "Il", "In", "Iw", "Kn", "Ky", "Ls", "Mn", "Mr", 
                        "Ms", "Mc", "Mnn", "Mss", "Mri", "Mnt", "Nb", "Nv", "NH", "NJ", 
                        "NM", "NY", "NC", "ND", "Oh", "Ok", "Or", "Pn", "RI", "SC", "SD", 
                        "Tn", "Tx", "Ut", "Vr", "Va", "Ws", "WV", "Wn", "Wy"), .Names = c("Alabama", 
                                                                                          "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", 
                                                                                          "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", 
                                                                                          "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", 
                                                                                          "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", 
                                                                                          "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", 
                                                                                          "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", 
                                                                                          "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", 
                                                                                          "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", 
                                                                                          "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", 
                                                                                          "Wyoming"))
test(id=5, code={
  abbreviate(method="both",minlength=2,names.arg=c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", 
                                                   "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", 
                                                   "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", 
                                                   "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", 
                                                   "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", 
                                                   "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", 
                                                   "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", 
                                                   "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", 
                                                   "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", 
                                                   "Wyoming"))
},  o = expected);

expected <- structure("abbrvt", .Names = "abbreviate")
test(id=6, code={
  abbreviate(minlength=6,names.arg="abbreviate")
},  o = expected);

expected <- structure(c("abcd", "ef", "abce"), .Names = c("abcd", "efgh", 
                                                          "abce"))
test(id=7, code={
  abbreviate(minlength=2,names.arg=c("abcd", "efgh", "abce"))
},  o = expected);

expected <- structure(c("ab", "ef", "ab"), .Names = c("abcd", "efgh", "abce"
))
test(id=8, code={
  abbreviate(minlength=2,names.arg=c("abcd", "efgh", "abce"),strict=TRUE)
},  o = expected);

expected <- structure(c("Alb", "Als", "Arz", "Ark", "Clf", "Clr", "Cn", "Dl", 
                        "Fl", "Gr", "Hw", "Id", "Il", "In", "Iw", "Kns", "Knt", "Ls", 
                        "Man", "Mr", "Mssc", "Mc", "Mnn", "Msss", "Mssr", "Mnt", "Nb", 
                        "Nv", "NH", "NJ", "NM", "NY", "NC", "ND", "Oh", "Ok", "Or", "Pn", 
                        "RI", "SC", "SD", "Tn", "Tx", "Ut", "Vrm", "Vrg", "Wsh", "WV", 
                        "Wsc", "Wy"), .Names = c("Alabama", "Alaska", "Arizona", "Arkansas", 
                                                 "California", "Colorado", "Connecticut", "Delaware", "Florida", 
                                                 "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", 
                                                 "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", 
                                                 "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", 
                                                 "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", 
                                                 "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", 
                                                 "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", 
                                                 "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", 
                                                 "West Virginia", "Wisconsin", "Wyoming"))
test(id=9, code={
  abbreviate(minlength=2,names.arg=c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", 
                                     "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", 
                                     "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", 
                                     "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", 
                                     "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", 
                                     "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", 
                                     "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", 
                                     "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", 
                                     "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", 
                                     "Wyoming"))
},  o = expected);

expected <- structure(c("Alb", "Als", "Arz", "Ark", "Clf", "Clr", "Cnn", 
                        "Dlw", "Flr", "Grg", "Haw", "Idh", "Ill", "Ind", "Iow", "Kns", 
                        "Knt", "Lsn", "Man", "Mry", "Mss", "Mch", "Mnn", "Mss", "Mss", 
                        "Mnt", "Nbr", "Nvd", "NwH", "NwJ", "NwM", "NwY", "NrC", "NrD", 
                        "Ohi", "Okl", "Org", "Pnn", "RhI", "StC", "StD", "Tnn", "Txs", 
                        "Uth", "Vrm", "Vrg", "Wsh", "WsV", "Wsc", "Wym"), .Names = c("Alabama", 
                                                                                     "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", 
                                                                                     "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", 
                                                                                     "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", 
                                                                                     "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", 
                                                                                     "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", 
                                                                                     "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", 
                                                                                     "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", 
                                                                                     "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", 
                                                                                     "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", 
                                                                                     "Wyoming"))
test(id=10, code={
  abbreviate(minlength=3,names.arg=c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", 
                                     "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", 
                                     "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", 
                                     "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", 
                                     "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", 
                                     "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", 
                                     "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", 
                                     "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", 
                                     "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", 
                                     "Wyoming"),strict=TRUE)
},  o = expected);

expected <- structure(c("Al", "Aa", "Ar", "As", "Cl", "Co", "Cn", "Dl", "Fl", 
                        "Gr", "Hw", "Id", "Il", "In", "Iw", "Kn", "Ky", "Ls", "Mn", "Mr", 
                        "Ms", "Mc", "Mnn", "Mss", "Mri", "Mnt", "Nb", "Nv", "NH", "NJ", 
                        "NM", "NY", "NC", "ND", "Oh", "Ok", "Or", "Pn", "RI", "SC", "SD", 
                        "Tn", "Tx", "Ut", "Vr", "Va", "Ws", "WV", "Wn", "Wy"), .Names = c("Alabama", 
                                                                                          "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", 
                                                                                          "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", 
                                                                                          "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", 
                                                                                          "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", 
                                                                                          "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", 
                                                                                          "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", 
                                                                                          "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", 
                                                                                          "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", 
                                                                                          "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", 
                                                                                          "Wyoming"))
test(id=11, code={
  abbreviate(method="both",minlength=2,names.arg=c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", 
                                                   "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", 
                                                   "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", 
                                                   "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", 
                                                   "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", 
                                                   "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", 
                                                   "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", 
                                                   "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", 
                                                   "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", 
                                                   "Wyoming"))
},  o = expected);

