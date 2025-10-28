// read all images in the public/img/arkham/<lang>/cards directory and write to the src/digests/<lang>.json for each file in the format "cards/{filename}"

const fs = require('fs');
const path = require('path');

// Take 'lang' as an argument
const lang = process.argv[2];

if (!lang) {
  console.error('Please provide a language code (e.g., "ita") as an argument.');
  process.exit(1);
}

const cardsDir = path.join(__dirname, `../public/img/arkham/${lang}/cards`);
const tarotDir = path.join(__dirname, `../public/img/arkham/${lang}/tarot`);
const digest = path.join(__dirname, `../src/digests/${lang}.json`);

if (!fs.existsSync(cardsDir)) {
  console.error(`Directory not found: ${dir}`);
  process.exit(1);
}

const files = fs.readdirSync(cardsDir).filter(f => f.endsWith('.avif'));

const tarot = fs.existsSync(tarotDir) ? fs.readdirSync(tarotDir).filter(f => f.endsWith('.jpg')) : [];

const digests = [...files.map(f => `cards/${f}`), ...tarot.map(f => `tarot/${f}`)];

fs.writeFileSync(digest, JSON.stringify(digests, null, 2));
console.log(`Wrote ${digests.length} digests to ${digest}`);
