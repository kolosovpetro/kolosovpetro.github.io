"use strict";

const test = document.getElementById("headerText");

function calculateAge() {
    var dateOfBirth = new Date("03/04/1994");

    var monthDifference = Date.now() - dateOfBirth.getTime();

 
    var ageDateTime = new Date(monthDifference);
   
    var year = ageDateTime.getUTCFullYear();

    var age = Math.abs(year - 1970);

    return age;
}

function formatHeaderText() {
    const ageDecimal = calculateAge();

    const ageBinary = ageDecimal.toString(2);

    test.innerHTML = `${ageBinary} yo .NET Developer from $"{cityName}"`
}

formatHeaderText();