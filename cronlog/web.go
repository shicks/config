package main

import (
	"bufio"
	"fmt"
	"html/template"
	"log"
	"net/http"
	"os"
	"os/exec"
	"path"
	"regexp"
	"strings"
)

type job struct {
	Name string
	Full string
	Line string
	Last []string
}

func startDaemon() {
	http.HandleFunc("/", rootHandler)
	http.HandleFunc("/log/", logHandler)
	// TODO - add a /run/ function
	log.Fatal(http.ListenAndServe(":5678", nil))
}

var tpl = template.Must(template.New("").Parse(`
{{- define "root" -}}
{{- template "header" -}}
{{- range $name, $job := . -}}
<div class="job">
  <div class="line">{{ $job.Line }}</div>
  <div class="log">
    {{- range $entry := $job.Last }}
    <div class="logentry">{{ $entry }}</div>
    {{- end }}
    {{ $len := len $job.Last }}
    {{- if gt $len 4 }}
    [<a href="/log/{{ $name }}">all</a>]
    {{- end }}
  </div>
</div>
{{- end -}}
{{- template "script" -}}
{{- end -}}

{{define "header"}}
<!DOCTYPE html>
<style>
.job { font-family: monospace; margin-bottom: 1ex; }
.line, .logentry { white-space: pre; }
.log { margin-left: 2em; }
.c1  { font-weight: bold; }
.c30 { color: black; }
.c31 { color: red; }
.c32 { color: green; }
.c33 { color: yellow; }
.c34 { color: blue; }
.c35 { color: magenta; }
.c36 { color: cyan; }
.c37 { color: white; }
.c40 { background-color: black; }
.c41 { background-color: red; }
.c42 { background-color: green; }
.c43 { background-color: yellow; }
.c44 { background-color: blue; }
.c45 { background-color: magenta; }
.c46 { background-color: cyan; }
.c47 { background-color: white; }
</style>
{{end}}

{{define "script"}}
<script>
for (const e of document.querySelectorAll('.logentry')) {
  // Goal: linkify individual runs
  const match = /^(.*\s)(\S+)\[(\d+)\](.*)$/.exec(e.textContent);
  if (match) {
    const [, pre, name, pid, post] = match;
    const a = document.createElement('a');
    a.textContent = pid;
    a.href = '/log/' + name + '/' + pid;
    while (e.childNodes.length) { e.childNodes[0].remove(); }
    e.appendChild(document.createTextNode(pre + name + '['));
    e.appendChild(a);
    e.appendChild(document.createTextNode(']' + post));
  }
}
</script>
{{end}}

{{define "log"}}
{{- template "header" -}}
<div class="job">
  {{- range . }}
  <div class="logentry">{{ . }}</div>
  {{- end }}
</div>
{{end}}

{{template "root"}}
`))

func rootHandler(w http.ResponseWriter, r *http.Request) {
	jobs, err := getJobs()
	if err != nil {
		w.WriteHeader(http.StatusInternalServerError)
		fmt.Fprint(w, err)
		return
	}
	tpl.ExecuteTemplate(w, "root", jobs)
	// TODO - journalctl -t foo _PID=2008396
	// Links: [pid] -> show just the one invocation (js for inline?)
	// all logs - all invocations?
}

func logHandler(w http.ResponseWriter, r *http.Request) {
	// check for job name, then call journalctl to get logs
	split := strings.Split(r.URL.Path, "/")
	if len(split) < 3 {
		w.WriteHeader(http.StatusNotFound)
		fmt.Fprint(w, "404 Not Found")
		return
	}
	args := []string{
		"-t", split[2],
	}
	if len(split) > 3 {
		args = append(args, "_PID="+split[3])
	}
	// TODO - SYSTEMD_COLORS=true
	// TODO - consider also passing `-n #` based on query?
	// TODO - consider `-r` to reverse?
	journal, err := exec.Command("journalctl", args...).Output()
	if err != nil {
		w.WriteHeader(http.StatusInternalServerError)
		fmt.Fprint(w, err)
		return
	}
	tpl.ExecuteTemplate(w, "log", strings.Split(string(journal), "\n"))
}

func getJobs() (map[string]job, error) {
	jobs := map[string]job{}
	crontab, err := exec.Command("crontab", "-l").Output()
	if err != nil {
		return nil, err
	}
	// Parse the crontab (TODO - just use strings.Split?)
	scanner := bufio.NewScanner(strings.NewReader(string(crontab)))
	for scanner.Scan() {
		line := scanner.Text()
		if match := cronLineRe.FindStringSubmatch(line); match != nil {
			if path.Base(match[1]) == path.Base(os.Args[0]) {
				// This is a match, let's get its name!
				nameBytes, err := exec.Command("sh", "-c", fmt.Sprintf("CRONLOG_PARSE=1 %s%s", os.Args[0], match[2])).Output()
				if err != nil {
					return nil, err
				}
				name := strings.TrimSpace(string(nameBytes))
				lastBytes, err := exec.Command("journalctl", "-r", "-n", "5", "-t", name, "-g", "exited (with code|successfully)").Output()
				last := []string{"-- No entries --"}
				if err == nil {
					lastSplit := strings.Split(string(lastBytes), "\n")
					if len(lastSplit) > 1 {
						last = lastSplit[1:]
					}
				}
				jobs[name] = job{
					Name: name,
					Full: match[2],
					Line: line,
					Last: last,
				}
			}
		}
	}
	return jobs, nil
}

var cronLineRe = regexp.MustCompile(`^\s*(?:[^#\s]+\s+){5}([^#\s]*)([^#]*)#?.*$`)
