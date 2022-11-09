"use strict";

const test = document.getElementById("headerText");

function calculateAge() {
    const dateOfBirth = new Date("03/04/1994");

    const monthDifference = Date.now() - dateOfBirth.getTime();

    const ageDateTime = new Date(monthDifference);

    const year = ageDateTime.getUTCFullYear();

    const age = Math.abs(year - 1970);

    return age;
}

function formatHeaderText() {
    const ageDecimal = calculateAge();

    const ageBinary = ageDecimal.toString(2);

    test.innerHTML = `${ageBinary} yo .NET Developer from $"{cityName}"`
}

formatHeaderText();