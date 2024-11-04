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
    
    <div class="search-container">
        <form action="search" method="get" class="search-form">
            <input type="text" name="query" id="query" placeholder="Search...">
            <button type="submit">Search</button>
        </form>
    </div>
    
    <div class="centered-div">
        <form action="upload" method="post" enctype="multipart/form-data" class="upload-form">
            <label for="file" id="file-label" class="upload-label">Select a file</label>
            <input type="file" name="file" id="file" class="upload-input" onchange="updateFileName()">
            <button type="submit">Upload File</button>
        </form>
        <button id="populate-table-button" onclick="populateTable()">Populate Table</button>
    </div>

    <div class="table-container">
        <table id="name-table">
            <thead>
                <tr>
                    <th>Name</th>
                </tr>
            </thead>
            <tbody>
            </tbody>
        </table>
    </div>
</body>
</html>
