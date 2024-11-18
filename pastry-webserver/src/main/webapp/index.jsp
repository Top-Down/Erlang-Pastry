<!DOCTYPE html>
<html>
<head>
    <title>Distributed Pastry</title>
    <link rel="stylesheet" type="text/css" href="./css/styles.css"/>
    <script src="./javascript/scripts.js" defer></script>
    <style>
        /* Initially hide the table */
        .table-container {
            display: none;
        }
    </style>
</head>
<body>
    <div class="header-box">
        <h1>DISTRIBUTED PASTRY</h1>
    </div>
    
    <div class="outer-search-container">
	    <div class="search-container">
		    <input type="text" name="query" id="query" placeholder="ex: file.txt">
		    <button id="searchButton" onclick="searchFile()">Search</button>
		    <button id="downloadButton" style="display:none;" onclick="downloadFile()">Download</button>
		    <button id="deleteButton" style="display:none;" onclick="deleteFile()">Delete</button>
		</div>
		<p id="errMsg"></p>
    </div>

    <div class="centered-div">
	    <form id="upload-form" class="upload-form">
	        <label for="file" id="file-label" class="upload-label">Select a file</label>
	        <input type="file" name="file" id="file" class="upload-input" onchange="updateFileName()">
	        <button type="button" onclick="updateFile()">Upload File</button>
	    	<div id="uploadMsg"></div>
	    </form>
	    <button id="populate-table-button" onclick="populateTable()">Show all files</button>
	</div>

    <div class="table-container">
        <table id="name-table">
            <thead>
                <tr></tr>
            </thead>
            <tbody>
            </tbody>
        </table>
        <p id="errMsgTable"></p>
    </div>
</body>
</html>
