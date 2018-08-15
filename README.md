# accidents
Visualisierung Münsteraner Verkehrsunfälle

# start docker image
```
sudo docker build -t accidents-local .
mkdir logs
sudo docker run --rm -p 3838:3838 --privileged -v $PWD/logs/:/var/log/shiny-server/ accidents-local
```
[localhost:3838](open browser), see `logs` directory for app logs

# Rechtliches
GPLv3 licensed, based on [the prototype](https://github.com/mammykins/App-cherry_picker) by Matthew Gregory.
