#!/usr/bin/env node
"use strict";

const fs      = require("fs");
const jwt     = require("jsonwebtoken");

const privateKey = fs.readFileSync("../data/AuthKey_4FL42YJG39.p8").toString();
const teamId     = "DR9W889U86";
const keyId      = "4FL42YJG39";

const jwtToken = jwt.sign({}, privateKey, {
  algorithm: "ES256",
  expiresIn: "180d",
  issuer: teamId,
  header: {
    alg: "ES256",
    kid: keyId
  }
});

console.log(jwtToken);
