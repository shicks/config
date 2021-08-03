package main

import (
	"bufio"
	"flag"
	"fmt"
	"io"
	"log"
	"os"
	"os/exec"
	"path"
	"regexp"
	"strings"
	"sync"
)

var (
	sleepFlag     = flag.Duration("s", 0, "Sleeps for a random period of time up to DURATION.")
	repeatsFlag   = flag.Int("r", 0, "Allows the command to be repeated manually COUNT times between each automatic run.  Manual repeats are requested via the web UI.")
	rateLimitFlag = flag.Duration("d", 0, "Rate limit between manual re-runs.")
	nameFlag      = flag.String("n", "", "Specifies the name to use for the job in systemd logs.")
	daemonFlag    = flag.Bool("daemon", false, "Indicates that the web server should be started.")
)

// Internal usage: invoking with CRONLOG_PARSE=1 indicates
// that we just want to know what the name would be.
func getName() string {
	if *nameFlag != "" {
		return *nameFlag
	} else {
		return path.Base(flag.Arg(0))
	}
}

// Internal usage: invoking with CRONLOG_INDIRECT=1 indicates
// that we are running via systemd-run already.  The remaining
// args should be executed, but an extra wrapper on either side
// will be printed.
func runIndirect() error {
	cmd := exec.Command(os.Args[1], os.Args[2:]...)
	fmt.Fprintf(os.Stderr, "starting %s\n", strings.Join(os.Args[1:], " "))
	var wg sync.WaitGroup
	stdout, err := cmd.StdoutPipe()
	if err != nil {
		return err
	}
	stderr, err := cmd.StderrPipe()
	if err != nil {
		return err
	}
	if err := cmd.Start(); err != nil {
		return err
	}
	wg.Add(2)
	go func() {
		defer wg.Done()
		copyLogs(stdout, os.Stdout, "")
	}()
	go func() {
		defer wg.Done()
		copyLogs(stderr, os.Stderr, "<4>")
	}()
	wg.Wait()
	if err := cmd.Wait(); err != nil {
		exit, ok := err.(*exec.ExitError)
		if ok && exit.ExitCode() != 0 {
			fmt.Fprintf(os.Stderr, "<4>exited with code %d\n", exit.ExitCode())
			return nil
		}
		return err
	}
	fmt.Fprintf(os.Stderr, "exited successfully\n")
	return nil
}

func copyLogs(r io.Reader, w io.Writer, prefix string) {
	scanner := bufio.NewScanner(r)
	for scanner.Scan() {
		line := scanner.Text()
		// Prepend the prefix if relevant
		if !logLevelRe.MatchString(line) {
			line = fmt.Sprintf("%s%s", prefix, line)
		}
		// Mark trailing whitespace
		if strings.HasSuffix(line, " ") {
			line = trailingWhitespaceRe.ReplaceAllString(line, "\033[41m$1\033[m")
		}
		fmt.Fprintln(w, line)
	}
	if err := scanner.Err(); err != nil {
		fmt.Fprintln(w, "<3>failed processing output:", err)
	}
}

var logLevelRe = regexp.MustCompile(`^<\d>`)
var trailingWhitespaceRe = regexp.MustCompile(`(\s+)$`)

func runSystemd() error {
	if os.Getenv("XDG_RUNTIME_DIR") == "" {
		// This is what it is on my machine, presumably it's relatively
		// standard.  If not, be sure to set it directly in the crontab.
		os.Setenv("XDG_RUNTIME_DIR", fmt.Sprintf("/run/user/%d", os.Getuid()))
	}

	if err := exec.Command("systemctl", "--user", "reset-failed").Run(); err != nil {
		// NOTE: If this fails for some reason, we're going to have a bad
		// time, since it's really hard to find the error message.
		// We should probably write the logs to a file?
		return err
	}
	name := getName()
	args := []string{
		"-u", name,
		"--user",
		"-E", fmt.Sprintf("PATH=%s", os.Getenv("PATH")),
		"-E", "CRONLOG_INDIRECT=1",
		"-p", fmt.Sprintf("SyslogIdentifier=%s", name),
		os.Args[0],
	}
	// NOTE: os.Args includes $0, the currently running binary.
	// But we set CRONLOG_INDIRECT=1 to take a different path.
	args = append(args, flag.Args()...)
	return exec.Command("systemd-run", args...).Run()
}

func main() {
	if os.Getenv("CRONLOG_INDIRECT") == "1" {
		if err := runIndirect(); err != nil {
			log.Fatal(err)
		}
		return
	}

	flag.Parse()
	if os.Getenv("CRONLOG_PARSE") == "1" {
		fmt.Printf("%s\n", getName())
		return
	}

	if *daemonFlag {
		startDaemon()
	}

	// TODO - check sleep
	// TODO - check repeats
	// TODO - check delay

	err := runSystemd()
	if err != nil {
		// TODO - write to a file instead: ~/cronlog.error
		log.Fatal(err)
	}
}
