{application,epgsql,
             [{description,"PostgreSQL Client"},
              {vsn,"2.0.0"},
              {modules,[apgsql,ipgsql,pgsql,pgsql_binary,pgsql_fdatetime,
                        pgsql_idatetime,pgsql_sock,pgsql_types,pgsql_wire]},
              {registered,[]},
              {applications,[kernel,stdlib,ssl]},
              {env,[]},
              {included_applications,[]}]}.
