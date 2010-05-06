{application, sendfile,
 [{description, "sendfile linked-in driver"},
  {vsn, "0.0.1"},
  {modules, [
             sendfile,
             sendfile_drv
            ]},
  {applications, [
                  kernel,
                  stdlib,
                  sasl
                 ]},
  {registered, []},
  {env, []}
 ]}.

