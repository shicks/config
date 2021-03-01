# cronlog

A simple tool to give more visibility into cron jobs

There are two components: (1) the job wrapper, and (2) the web server.  Both are provided in the `cronlog` binary, which should be installed into `/usr/local/bin`.

The basic usage is to prefix any jobs in your crontab with `cronlog`.  Be sure the binary is visible from cron's path, which tends to be minimal since it does not include any directories added in login shell rc files.  (To determine the path, add a `* * * * * echo $PATH > /tmp/path` line via `crontab -e` and then check the file; an environment variable override `PATH=...` can be added to the top of the crontab).

To install the webserver, copy `cronlog.service` into `/etc/systemd/system`, replacing `USER` with your username, and then enable the service.  Then hitting `localhost:5678` will allow easy browsing of jobs, exit codes, and any logs.

```sh
sed "s/USER/$(whoami)/" cronlog.service | sudo sponge /etc/systemd/system/cronlog.service
sudo systemctl daemon-reload
sudo systemctl enable cronlog
sudo service cronlog start
```
