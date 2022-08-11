# src-location
Convert source location from line and column to index position (character offset), and vise versa.

This lib will take account `\r\n` **as 1 character**, so regardless the end of line (DOS/Unix/Mac), the line number always the same.

[![CircleCI](https://circleci.com/gh/futurist/src-location.svg?style=svg)](https://circleci.com/gh/futurist/src-location)
[![npm](https://img.shields.io/npm/v/src-location.svg "Version")](https://www.npmjs.com/package/src-location)

## Install

``` bash
npm install src-location
```

## Usage

- **locationToIndex(srcStr: string, line: number, column: number, noMergeCRLF) -> number**

> noMergeCRLF: will **not** merge CRLF as 1 char.

``` javascript
var lib = require('src-location')
var src = 'var a=1;\r\n2\n3\r4var b=2;'
expect(lib.locationToIndex(src, 4, 2)).equal(15)
```

- **indexToLocation(srcStr: string, index: number) -> {line: number, column: number}**

> noMergeCRLF: will **not** merge CRLF as 1 char.

``` javascript
var lib = require('src-location')
var src = 'var a=1;\r\n2\n3\r4var b=2;'
expect(lib.indexToLocation(src, 15)).deep.equal({line: 4, column: 2})
```

