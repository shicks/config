package main

import (
	"fmt"
	"io"
	"log"
	"os"
	"os/exec"
	"os/signal"
	"path/filepath"
	"regexp"
	"strings"
	"syscall"
	"unicode/utf8"

	"github.com/creack/pty"
	"golang.org/x/term"
)

var titleRegex = regexp.MustCompile(`(\x1b]([02]);)(.*?)(\x07)`)
var baseTitle = ""

func processTitle(title string) string {
	emoji := "✦"
	if strings.ContainsAny(title, "✋◇✦") {
		if strings.Contains(title, "✋") {
			emoji = "✋"
		} else if strings.Contains(title, "◇") {
			emoji = "◇"
		} else if strings.Contains(title, "✦") {
			emoji = "⏳"
		}
	}
	return baseTitle + " " + emoji
}

func main() {
	// Base title setup
	baseTitle = os.Getenv("MY_TERMINAL_TITLE")
	if baseTitle == "" {
		wd, err := os.Getwd()
		if err != nil {
			wd = "unknown"
		}
		baseTitle = "G:" + filepath.Base(wd)
	}

	geminiCmd := os.Getenv("GEMINI_COMMAND")
	if geminiCmd == "" {
		p, err := exec.LookPath("gemini")
		if err != nil {
			log.Fatal("gemini command not found")
		}
		geminiCmd = p
	}
	args := os.Args[1:]

	cmd := exec.Command(geminiCmd, args...)

	logFile, err := os.Create("/tmp/GEMINI.LOG")
	if err != nil {
		log.Fatalf("Failed to open log file: %v", err)
	}
	defer logFile.Close()

	outFile, err := os.Create("/tmp/GEMINI.OUT")
	if err != nil {
		log.Fatalf("Failed to open out file: %v", err)
	}
	defer outFile.Close()

	ptmx, err := pty.Start(cmd)
	if err != nil {
		log.Fatalf("Failed to start pty: %v", err)
	}
	defer ptmx.Close()

	// Handle window size changes
	ch := make(chan os.Signal, 1)
	signal.Notify(ch, syscall.SIGWINCH)
	go func() {
		for range ch {
			if err := pty.InheritSize(os.Stdin, ptmx); err != nil {
				log.Printf("Error resizing pty: %v", err)
			}
		}
	}()
	ch <- syscall.SIGWINCH // Initial resize

	// Set stdin in raw mode
	oldState, err := term.MakeRaw(int(os.Stdin.Fd()))
	if err != nil {
		log.Printf("Failed to enter raw mode: %v", err)
	}
	defer term.Restore(int(os.Stdin.Fd()), oldState)

	// Copy stdin to pty
	go func() {
		if _, err := io.Copy(ptmx, os.Stdin); err != nil {
			// This error is expected when the pty is closed, so don't log fatal
			log.Printf("Error copying stdin to pty: %v", err)
		}
	}()

	// Read from pty and process
	buff := make([]byte, 4096)
	var currentLine strings.Builder
	for {
		n, err := ptmx.Read(buff)
		if err != nil {
			if err == io.EOF || strings.Contains(err.Error(), "input/output error") {
				log.Printf("PTY read loop exiting: %v", err)
			} else {
				log.Printf("Error reading from pty: %v", err)
			}
			break
		}
		if n == 0 {
			continue
		}

		data := buff[:n]
		logFile.Write(data)

		currentLine.Write(data)
		content := currentLine.String()
		processed := ""
		remaining := content

		for len(remaining) > 0 {
			// Find first delimiter: title, \n, or \r
			titleMatch := titleRegex.FindStringSubmatchIndex(remaining)
			lfIndex := strings.Index(remaining, "\n")
			crIndex := strings.Index(remaining, "\r")

			firstIndex := -1
			delimiterType := 0 // 1: title, 2: LF, 3: CR

			if titleMatch != nil {
				firstIndex = titleMatch[0]
				delimiterType = 1
			}

			if lfIndex != -1 && (firstIndex == -1 || lfIndex < firstIndex) {
				firstIndex = lfIndex
				delimiterType = 2
			}

			if crIndex != -1 && (firstIndex == -1 || crIndex < firstIndex) {
				// Special case: if CR is followed by LF, treat it as LF to handle CRLF as one unit
				if !(crIndex+1 < len(remaining) && remaining[crIndex+1] == '\n') {
					firstIndex = crIndex
					delimiterType = 3
				}
			}

			if firstIndex == -1 {
				// No more delimiters, break and keep in remaining
				break
			}

			// --- Process segment before delimiter ---
			processed += remaining[:firstIndex]

			// --- Handle delimiter ---
			switch delimiterType {
			case 1: // Title
                // Group 1: \x1b]0; or \x1b]2;
                // Group 2: 0 or 2
                // Group 3: title content
                // Group 4: \x07
				prefix := remaining[titleMatch[2]:titleMatch[3]]
				titleContent := remaining[titleMatch[6]:titleMatch[7]]
				suffix := remaining[titleMatch[8]:titleMatch[9]]

				processedTitle := processTitle(titleContent)
				newTitle := fmt.Sprintf("%s%s%s", prefix, processedTitle, suffix)
				processed += newTitle
				remaining = remaining[titleMatch[1]:]
			case 2: // LF
				processed += "\n"
				remaining = remaining[firstIndex+1:]
			case 3: // CR
				processed += "\r"
				remaining = remaining[firstIndex+1:]
			}
		}

		// --- Output processed data ---
		if len(processed) > 0 {
			os.Stdout.WriteString(processed)
			os.Stdout.Sync()
			outFile.WriteString(processed)
		}

		// --- Check for incomplete UTF-8 at the end of remaining ---
		tempRemaining := ""
		r, size := utf8.DecodeLastRuneInString(remaining)
		if r == utf8.RuneError && size == 1 && len(remaining) < utf8.UTFMax {
			// Potential partial rune at the end, keep all of it for next buffer read
			tempRemaining = remaining
		} else {
			// No partial rune, so clear (should be empty if loop exited due to no delimiters)
		}
		currentLine.Reset()
		currentLine.WriteString(tempRemaining)

	}

	// Flush any remaining buffer
	if currentLine.Len() > 0 {
		final := strings.ReplaceAll(currentLine.String(), "\r", "")
		os.Stdout.WriteString(final)
		os.Stdout.Sync()
		outFile.WriteString(final)
	}

	cmd.Wait()
}