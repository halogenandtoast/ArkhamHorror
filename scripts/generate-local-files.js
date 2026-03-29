#!/usr/bin/env node
/**
 * 生成本地图片文件清单
 * 扫描 frontend/public/img/arkham/ 目录，生成 local-files.json
 * 支持 Docker 容器内运行
 */

const fs = require('fs');
const path = require('path');

// 检测是否在 Docker 容器内（通过检查特定路径）
const isDocker = fs.existsSync('/workspace/public/img/arkham');

const IMG_DIR = isDocker 
  ? '/workspace/public/img/arkham'
  : path.join(__dirname, '..', 'frontend', 'public', 'img', 'arkham');

const OUTPUT_FILE = isDocker
  ? '/workspace/public/local-files.json'
  : path.join(__dirname, '..', 'frontend', 'public', 'local-files.json');

function scanDirectory(dir, basePath = '') {
  const files = new Set();
  
  try {
    const entries = fs.readdirSync(dir, { withFileTypes: true });
    
    for (const entry of entries) {
      const relativePath = basePath ? `${basePath}/${entry.name}` : entry.name;
      const fullPath = path.join(dir, entry.name);
      
      if (entry.isDirectory()) {
        // 递归扫描子目录
        const subFiles = scanDirectory(fullPath, relativePath);
        subFiles.forEach(f => files.add(f));
      } else {
        // 只添加图片文件
        const ext = path.extname(entry.name).toLowerCase();
        if (['.avif', '.jpg', '.jpeg', '.png', '.webp', '.gif'].includes(ext)) {
          files.add(relativePath);
        }
      }
    }
  } catch (err) {
    console.error(`Error scanning ${dir}:`, err.message);
  }
  
  return files;
}

function main() {
  console.log('🔍 Scanning local image files...');
  console.log(`   Directory: ${IMG_DIR}`);
  
  if (!fs.existsSync(IMG_DIR)) {
    console.error(`❌ Directory not found: ${IMG_DIR}`);
    process.exit(1);
  }
  
  const files = scanDirectory(IMG_DIR);
  const fileArray = Array.from(files).sort();
  
  // 生成 JSON 文件
  const output = {
    generatedAt: new Date().toISOString(),
    count: fileArray.length,
    files: fileArray
  };
  
  fs.writeFileSync(OUTPUT_FILE, JSON.stringify(output, null, 2));
  
  console.log(`✅ Generated: ${OUTPUT_FILE}`);
  console.log(`   Total files: ${fileArray.length}`);
  console.log(`   Sample files:`);
  fileArray.slice(0, 5).forEach(f => console.log(`     - ${f}`));
}

main();
