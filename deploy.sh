#!/bin/bash

# Step 0: Add fingerprints for each VM
ssh-keyscan -H 10.2.1.4 >> ~/.ssh/known_hosts
ssh-keyscan -H 10.2.1.5 >> ~/.ssh/known_hosts

# Step 1: Copy folder project to /root/ in both VMs
sshpass -p 'root' scp -r project root@10.2.1.4:/root/
sshpass -p 'root' scp -r project root@10.2.1.5:/root/

# Step 2: Copy pastry.war to the specified directory in 10.2.1.4
sshpass -p 'root' scp pastry-webserver/pastry.war root@10.2.1.4:/root/servers/apache-tomcat-10.1.16/webapps/

# Step 3: Create three new shells and connect to the VMs using ssh, then execute the commands
gnome-terminal -- bash -c "sshpass -p 'root' ssh root@10.2.1.4 'cd /root/project && make && erl -pa ebin -name node1@10.2.1.4 -setcookie pastry -eval \"controller:start(\\\"control1\\\", 10, \\\"node1@10.2.1.4\\\").\"'; exec bash"
gnome-terminal -- bash -c "sshpass -p 'root' ssh root@10.2.1.5 'cd /root/project && make && erl -pa ebin -name node2@10.2.1.5 -setcookie pastry -eval \"controller:start(\\\"control2\\\", 10, \\\"node2@10.2.1.5\\\", {{node1, node1@10.2.1.4}, \\\"node1\\\"}).\"'; exec bash"
gnome-terminal -- bash -c "sshpass -p 'root' ssh root@10.2.1.4 'cd /root/servers/apache-tomcat-10.1.16/bin && ./startup.sh'; exec bash"
