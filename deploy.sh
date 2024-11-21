#!/bin/bash

# Step 0: Add fingerprints for each VM
ssh-keyscan -H 10.2.1.4 >> ~/.ssh/known_hosts
ssh-keyscan -H 10.2.1.5 >> ~/.ssh/known_hosts

# Step 1: Copy folder project to /root/ in both VMs
sshpass -p 'root' scp -r project root@10.2.1.4:/root/pastry/
sshpass -p 'root' scp -r project root@10.2.1.5:/root/pastry/

# Step 2: Copy pastry.war to the specified directory in 10.2.1.4
sshpass -p 'root' scp pastry-webserver/pastry.war root@10.2.1.4:/root/servers/apache-tomcat-10.1.16/webapps/
