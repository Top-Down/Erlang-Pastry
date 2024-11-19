def _get_fidelity(self, position):
    qubits = self.node.qmemory.peek(positions=[position])[0].qstate.qubits
    fidelity = ns.qubits.qubitapi.fidelity(
        qubits, 
        ns.qubits.ketstates.b00, 
        squared=True)
    return fidelity
