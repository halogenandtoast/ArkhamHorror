#!/usr/bin/env node
//
// Generates a manifest of all image asset paths (relative to frontend/public/).
// This manifest is committed to the repo so that contributors and Docker Compose
// users can download images from the CDN without needing AWS credentials.
//
// Usage: node scripts/generate-manifest.cjs
//
const fs = require('fs');
const path = require('path');

const publicDir = path.join(__dirname, '../frontend/public');
const imgDir = path.join(publicDir, 'img');
const outputFile = path.join(__dirname, '../frontend/image-manifest.json');

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

// Walk all files under img/ and group by their immediate parent directory.
// This covers both gitignored assets (cards, portraits, …) and git-tracked
// UI assets (tokens, slots, icons, …) so the full set can be fetched from
// the CDN without a git clone.
const manifest = {};

for (const absPath of walkDir(imgDir)) {
  const relPath = path.relative(publicDir, absPath);
  const dirKey = path.relative(publicDir, path.dirname(absPath));
  if (!manifest[dirKey]) manifest[dirKey] = [];
  manifest[dirKey].push(relPath);
}

// Stable sort: keys alphabetically, files within each key alphabetically
const sortedManifest = {};
let totalCount = 0;
for (const key of Object.keys(manifest).sort()) {
  sortedManifest[key] = manifest[key].sort();
  totalCount += sortedManifest[key].length;
}

fs.writeFileSync(outputFile, JSON.stringify(sortedManifest, null, 2) + '\n');
console.log(`Wrote ${totalCount} files across ${Object.keys(sortedManifest).length} directories to ${outputFile}`);
