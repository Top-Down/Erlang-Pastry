current_index = math.floor((ns.sim_time() - start_time) / self.t_clock)
current_slot = math.floor(current_index / self.K_attempts)

if current_slot >= self.tot_slots:
    continue

if success_slots[current_slot] != -1:
    continue

# receive the photon
qubit = self.node.ports["q0"].rx_input().items[0]

# store the photon in the quantum memory
self.node.qmemory.put(qubit, positions=[current_slot])

# compute the current attempt index
success_attempt = current_index % self.K_attempts
success_slots[current_slot] = success_attempt

print(f"[{ns.sim_time()}] 
      Repeater {self.node.ID}: 
      Latched photon in slot {current_slot} 
      at attempt {success_attempt}")
continue