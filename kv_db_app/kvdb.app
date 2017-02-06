{application, kvdb, [
    {description, "Very simple in memory key-value database"},
        {vsn, "1"},
        {registered, []},
        {modules, [
            kvdb_app,
            kvdb_sup,
            kvdb_server
        ]},
        {applications, [
            kernel,
            stdlib
        ]},
        {mod, {
                  kvdb_app, []
              }},
        {env, []}
]}.
