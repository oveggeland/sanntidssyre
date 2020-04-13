Installation guide

```
sudo apt install tmux 
sudo gem install tmuxinator 
```
Change the following line to your working directory:
```
root: ~/Documents/sanntidssyre
```
Then create a folder for the tmuxinator setup and move the monitor.yml there
```
mkdir ~/.tmuxinator
mv monitor.yml ~/.tmuxinator/
```
Then all thats needed to run the elavator is
```
tmuxinator monitor
```
