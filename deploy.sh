#!/bin/bash

# Step 1: Copy folder project to /root/ in both VMs
scp -r project root@10.2.1.4:/root/
scp -r project root@10.2.1.5:/root/

# Step 2: Copy pastry.war to the specified directory in 10.2.1.4
scp pastry-webserver/pastry.war root@10.2.1.4:/root/servers/apache-tomcat-10.1.16/webapps/

# Step 3: Create three new shells and connect to the VMs using ssh
gnome-terminal -- bash -c "ssh root@10.2.1.4; exec bash"
gnome-terminal -- bash -c "ssh root@10.2.1.4; exec bash"
gnome-terminal -- bash -c "ssh root@10.2.1.5; exec bash"

# Step 4: On both VMs, head into /root/project and execute make, then execute the respective erl commands
ssh root@10.2.1.4 << 'EOF'
cd /root/project
make
erl -pa ebin -name node1@10.2.1.4 -setcookie pastry -eval "controller:start(\"control1\", 10, \"node1@10.2.1.4\")."
EOF

ssh root@10.2.1.5 << 'EOF'
cd /root/project
make
erl -pa ebin -name node2@10.2.1.5 -setcookie pastry -eval "controller:start(\"control2\", 10, \"node2@10.2.1.5\", {{node1, node1@10.2.1.4}, \"node1\"})."
EOF

# Step 5: In the second 10.2.1.4 shell, go into /root/servers/apache-tomcat-10.1.16/bin and execute ./startup.sh
gnome-terminal -- bash -c "ssh root@10.2.1.4 'cd /root/servers/apache-tomcat-10.1.16/bin && ./startup.sh'; exec bash"
