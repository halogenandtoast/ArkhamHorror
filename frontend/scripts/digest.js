// read all images in the public/img/arkham/ita/cards directory and write to the  src/digests/ita.json for each file in the format "cards/{filename}"

const fs = require('fs');
const path = require('path');


const dir = path.join(__dirname, '../public/img/arkham/ita/cards');
const digest = path.join(__dirname, '../src/digests/ita.json');

const files = fs.readdirSync(dir).filter(f => f.endsWith('.jpg'));

const digests = files.map(f => `cards/${f}`);

fs.writeFileSync(digest, JSON.stringify(digests, null, 2));
console.log(`Wrote ${digests.length} digests to ${digest}`);
