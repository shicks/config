go build main
sudo chown root:uinput main
sudo chmod ug+s main
mv main ~/local/bin/fix-uinput
