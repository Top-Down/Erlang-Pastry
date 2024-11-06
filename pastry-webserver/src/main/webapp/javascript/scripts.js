const E_NO_FILE = "File not found";
const E_UP_FAILED = "Upload failed";
const E_DEL_FAILED = "Delete failed";
const OK_UPLOAD = "File uploaded!";

document.addEventListener('DOMContentLoaded', function() {
    document.getElementById('file').addEventListener('change', updateFileName);
    document.getElementById('populate-table-button').addEventListener('click', populateTable);
});

function updateFileName() {
    var input = document.getElementById("file");
    var label = document.getElementById("file-label");
    label.textContent = input.files[0].name;
}

async function updateFile() {
    const uploadMsg = document.getElementById("uploadMsg");
    const fileInput = document.getElementById("file");
    const file = fileInput.files[0];
	
    const formData = new FormData();
    formData.append('file', file);

    try {
        const response = await fetch("/upload", {
            method: 'POST',
            body: formData
        });
		
        if(response.ok) uploadMsg.textContent = OK_UPLOAD;
		else uploadMsg.textContent = E_UP_FAILED;
    }
	catch (error) {
        uploadMsg.textContent = E_UP_FAILED;
    }
}

function downloadRequest(fileName, errMsg) {
	fetch(
			'/download?fileName=' + fileName,
			{ method: 'GET' }
		)
		.then(response => {
	        if(response.ok) {
				errMsg.textContent = "";
				return response.blob();
			}
	        else {
				errMsg.textContent = E_NO_FILE;
				throw new Error(E_NO_FILE);
			}
	    })
	    .then(blob => {
	        const url = window.URL.createObjectURL(blob);
	        const a = document.createElement('a');
	        a.style.display = 'none';
	        a.href = url;
	        a.download = fileName;
	        document.body.appendChild(a);
	        a.click();
	        window.URL.revokeObjectURL(url);
	    })
	    .catch(error => {
	        console.error('Error:', error);
	    });
}

function deleteRequest(fileName, errMsg) {
	fetch(
			'/delete?fileName=' + fileName,
			{ method: 'GET' }
		)
		.then(response => {
	        if(response.ok) uploadMsg.textContent = OK_UPLOAD;
	        else {
				errMsg.textContent = E_DEL_FAILED;
				throw new Error(E_DEL_FAILED);
			}
	    })
	    .catch(error => {
	        console.error('Error:', error);
	    });
}

function downloadFile() {
	const errMsg = document.getElementById("errMsg");
	const fileInput = document.getElementById("file");
    const fileName = fileInput.files[0];
	
	downloadRequest(fileName, errMsg);
}

function deleteFile() {
	const errMsg = document.getElementById("errMsg");
	const fileInput = document.getElementById("file");
    const fileName = fileInput.files[0];
	
	deleteRequest(fileName, errMsg);
}

function populateTable() {
	const errMsg = document.getElementById("errMsgTable");
	fetch(
	        'allfiles',
	        { method: 'GET' }
	    )
	    .then(response => response.json())
	    .then(data => {
	        var tableBody = document.getElementById('name-table').getElementsByTagName('tbody')[0];
	        tableBody.innerHTML = ''; // Clear existing rows
	        data.names.forEach(name => {
	            var row = tableBody.insertRow();

	            // name column
	            var nameCell = row.insertCell(0);
	            nameCell.textContent = name;

	            // download button column
	            var downloadCell = row.insertCell(1);
	            var downloadButton = document.createElement('button');
	            downloadButton.textContent = 'Download';
	            downloadButton.onclick = function() { downloadRequest(name, errMsg); };
	            downloadCell.appendChild(downloadButton);

	            // delete button column
	            var deleteCell = row.insertCell(2);
	            var deleteButton = document.createElement('button');
	            deleteButton.textContent = 'Delete';
	            deleteButton.onclick = function() { deleteRequest(name, errMsg); };
	            deleteCell.appendChild(deleteButton);
	        });
	        document.querySelector('.table-container').style.display = 'flex';	// show table
	    })
	    .catch(error => console.error('Error:', error));
}

async function searchFile() {
    const query = document.getElementById("query").value;
    const response = await fetch("search?query=" + query);
    const result = await response.json();

	const downloadBtn = document.getElementById("downloadButton");
	const deleteBtn = document.getElementById("deleteButton");
	const errMsg = document.getElementById("errMsg");
	
    if(result.status) {
        downloadBtn.style.display = "block";
        deleteBtn.style.display = "block";
		errMsg.textContent = "";
    }
	else {
		downloadBtn.style.display = "none";
		deleteBtn.style.display = "none";
		errMsg.textContent = E_NO_FILE;
    }
}
