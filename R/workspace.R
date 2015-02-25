.Workspace <- new.env()

.Workspace$dictionary <- list(integer="-?\\d+|0[xX][a-fA-F\\d]+(?:[pP][+-]?\\d+)?",
                              number="-?(?:\\d+|\\d*\\.\\d+)(?:[eE][+-]?\\d+)?|0[xX][a-fA-F\\d]+(?:[pP][+-]?\\d+)?",
                              ip_address="\\b(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\b",
                              email_address="\\b[a-zA-Z0-9._%+-]+@(?:[a-zA-Z0-9-]+\\.)+[a-zA-Z]{2,4}\\b")
