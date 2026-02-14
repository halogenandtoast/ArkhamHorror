#!/usr/bin/env node
//
// Generates a manifest of all image asset paths (relative to frontend/public/).
// This manifest is committed to the repo so that contributors can download
// images from the CDN without needing AWS credentials.
//
// Usage: node scripts/generate-manifest.cjs
//
const fs = require('fs');
const path = require('path');

const publicDir = path.join(__dirname, '../frontend/public');
const outputFile = path.join(__dirname, '../frontend/image-manifest.json');

// Directories containing image assets that are gitignored
const assetDirs = [
  'img/arkham/cards',
  'img/arkham/boxes',
  'img/arkham/portraits',
  'img/arkham/tarot',
  'img/arkham/encounter-sets',
  'img/arkham/mini-cards',
  'img/arkham/sets',
  'img/arkham/customizations',
  'img/arkham/seals',
  'img/arkham/playing-cards',
  'img/arkham/es',
  'img/arkham/fr',
  'img/arkham/ita',
  'img/arkham/ko',
  'img/arkham/zh',
];

function walkDir(dir) {
  const results = [];
  if (!fs.existsSync(dir)) return results;

  const entries = fs.readdirSync(dir, { withFileTypes: true });
  for (const entry of entries) {
    if (entry.name === '.DS_Store') continue;
    const fullPath = path.join(dir, entry.name);
    if (entry.isDirectory()) {
      results.push(...walkDir(fullPath));
    } else {
      results.push(fullPath);
    }
  }
  return results;
}

// Group files by top-level asset directory for structured output
const manifest = {};
let totalCount = 0;

for (const dir of assetDirs) {
  const absDir = path.join(publicDir, dir);
  const files = walkDir(absDir)
    .map(f => path.relative(publicDir, f))
    .sort();

  if (files.length > 0) {
    manifest[dir] = files;
    totalCount += files.length;
  }
}

fs.writeFileSync(outputFile, JSON.stringify(manifest, null, 2) + '\n');
console.log(`Wrote ${totalCount} files across ${Object.keys(manifest).length} directories to ${outputFile}`);
