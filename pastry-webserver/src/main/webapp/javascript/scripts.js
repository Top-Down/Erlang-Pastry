document.addEventListener('DOMContentLoaded', function() {
    document.getElementById('file').addEventListener('change', updateFileName);
    document.getElementById('populate-table-button').addEventListener('click', populateTable);
});

function updateFileName() {
    var input = document.getElementById('file');
    var label = document.getElementById('file-label');
    label.textContent = input.files[0].name;
}

function populateTable() {
    fetch('allfiles', {
        method: 'GET',
        headers: {
            'Content-Type': 'application/json'
        }
    })
    .then(response => response.json())
    .then(data => {
        var tableBody = document.getElementById('name-table').getElementsByTagName('tbody')[0];
        tableBody.innerHTML = ''; // Clear existing rows
        data.names.forEach(name => {
            var row = tableBody.insertRow();
            var cell = row.insertCell(0);
            var link = document.createElement('a');
            link.href = 'your-other-servlet-url?name=' + name;
            link.textContent = name;
            cell.appendChild(link);
        });
        // Show the table container
        document.querySelector('.table-container').style.display = 'flex';
    })
    .catch(error => console.error('Error:', error));
}
