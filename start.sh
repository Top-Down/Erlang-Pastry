gnome-terminal -- bash -c "sshpass -p 'root' ssh -t root@10.2.1.4 'cd /root/project && make; erl -pa ebin -name node1@10.2.1.4 -setcookie pastry -eval \"controller:start(\\\"control1\\\", 10, \\\"node1@10.2.1.4\\\").\"; exec bash'; exec bash"
gnome-terminal -- bash -c "sshpass -p 'root' ssh -t root@10.2.1.4 'cd /root/servers/apache-tomcat-10.1.16/bin && ./startup.sh; exec bash'; exec bash"
sleep 5
gnome-terminal -- bash -c "sshpass -p 'root' ssh -t root@10.2.1.5 'cd /root/project && make; erl -pa ebin -name node2@10.2.1.5 -setcookie pastry -eval \"controller:start(\\\"control2\\\", 10, \\\"node2@10.2.1.5\\\", \\\"node1@10.2.1.4\\\").\"; exec bash'; exec bash"