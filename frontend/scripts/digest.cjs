// read all images in the public/img/arkham/<lang>/cards and
// public/img/arkham/<lang>/homebrew/*/cards directories and write to
// src/digests/<lang>.json in the format "cards/{filename}" or
// "homebrew/{campaign}/cards/{filename}"

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
  console.error(`Directory not found: ${cardsDir}`);
  process.exit(1);
}

const files = fs.readdirSync(cardsDir).filter(f => f.endsWith('.avif')).sort();

const homebrewDir = path.join(__dirname, `../public/img/arkham/${lang}/homebrew`);
const homebrew = fs.existsSync(homebrewDir)
  ? fs.readdirSync(homebrewDir, { withFileTypes: true }).sort((a, b) => a.name.localeCompare(b.name)).flatMap(entry => {
    if (!entry.isDirectory()) return [];

    const cardsPath = path.join(homebrewDir, entry.name, 'cards');
    if (!fs.existsSync(cardsPath)) return [];

    return fs.readdirSync(cardsPath)
      .filter(f => f.endsWith('.avif'))
      .sort()
      .map(f => `homebrew/${entry.name}/cards/${f}`);
  })
  : [];

const tarot = fs.existsSync(tarotDir) ? fs.readdirSync(tarotDir).filter(f => f.endsWith('.jpg')).sort() : [];

const digests = [...files.map(f => `cards/${f}`), ...homebrew, ...tarot.map(f => `tarot/${f}`)];

fs.writeFileSync(digest, JSON.stringify(digests, null, 2));
console.log(`Wrote ${digests.length} digests to ${digest}`);
