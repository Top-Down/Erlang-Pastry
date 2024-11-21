gnome-terminal -- bash -c "sshpass -p 'root' ssh -t root@10.2.1.4 'cd /root/project && make; erl -pa ebin -name control1_node1@10.2.1.4 -setcookie pastry; exec bash'; exec bash"
gnome-terminal -- bash -c "sshpass -p 'root' ssh -t root@10.2.1.4 'cd /root/servers/apache-tomcat-10.1.16/bin && ./startup.sh; exec bash'; exec bash"
sleep 5
gnome-terminal -- bash -c "sshpass -p 'root' ssh -t root@10.2.1.5 'cd /root/project && make; erl -pa ebin -name control2_node2@10.2.1.5 -setcookie pastry; exec bash'; exec bash"