ore_dict(protocol="\\w+://")

expect_equal(ore_dict("protocol"), "protocol")
expect_equal(ore_dict(paste0("proto","col")), "protocol")
expect_equal(ore_dict(protocol), structure("\\w+://",names="protocol"))
expect_equal(as.character(ore(protocol, "?www\\.")), "(\\w+://)?www\\.")
