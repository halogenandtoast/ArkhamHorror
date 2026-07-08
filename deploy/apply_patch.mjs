// apply_patch.mjs
// 把 zh_patch.mjs 里的 UI 中文翻译注入到官方前端 zh chunk（zh-P5ZxcOoo.js）。
// 通过环境变量切换路径，Docker 构建期与本地调试共用同一份逻辑。
//
//   AH_SRC   原始 zh chunk 路径        (默认 /orig/zh-P5ZxcOoo.js)
//   AH_OUT   输出修补后 chunk 路径      (默认 /patched/zh-P5ZxcOoo.js)
//   AH_INDEX 可选：index.html 路径，传入则把 lang="en" 改为 lang="zh"

import patch from './zh_patch.mjs';
import fs from 'fs';
import path from 'path';

const SRC = process.env.AH_SRC || '/orig/zh-P5ZxcOoo.js';
const OUT = process.env.AH_OUT || '/patched/zh-P5ZxcOoo.js';
const IDX = process.env.AH_INDEX; // 可选

if (!fs.existsSync(SRC)) {
  console.error('原始 zh chunk 不存在:', SRC);
  process.exit(1);
}
const src = fs.readFileSync(SRC, 'utf8');

// 将 "a.b.c": "值" 的扁平 key 还原成嵌套对象
function nest(flat) {
  const root = {};
  for (const [k, v] of Object.entries(flat)) {
    const parts = k.split('.');
    let cur = root;
    for (let i = 0; i < parts.length - 1; i++) {
      cur[parts[i]] = cur[parts[i]] || {};
      cur = cur[parts[i]];
    }
    cur[parts[parts.length - 1]] = v;
  }
  return root;
}
const nested = nest(patch);
const lit = JSON.stringify(nested);

// 定位默认导出标记：export{XXX as default};
const m = src.match(/export\s*\{([^}]+)\s+as\s+default\};/);
if (!m) {
  console.error('未在源码中找到默认导出标记 export{... as default};');
  process.exit(1);
}
const varName = m[1].trim();

// 深合并 + 覆盖：patch 优先于原值
const inject = `
const __patch=${lit};
function __dm(t,s){for(const k in s){const v=s[k];if(v&&typeof v==='object'&&!Array.isArray(v)){if(t[k]&&typeof t[k]==='object'&&!Array.isArray(t[k])){t[k]=__dm({...t[k]},v);}else{t[k]={...v};}}else{t[k]=v;}}return t;}
const __pC=__dm({...${varName}},__patch);
export{__pC as default};`;

const out = src.replace(m[0], inject);
fs.mkdirSync(path.dirname(OUT), { recursive: true });
fs.writeFileSync(OUT, out);
console.log(`已写入修补 chunk: ${OUT} (${out.length} 字节, 注入 ${Object.keys(patch).length} 条)`);

// 可选：修正 index.html 的 lang
if (IDX && fs.existsSync(IDX)) {
  let h = fs.readFileSync(IDX, 'utf8');
  if (h.includes('lang="en"')) {
    h = h.replace('lang="en"', 'lang="zh"');
    fs.writeFileSync(IDX, h);
    console.log('已修正 index.html lang="zh"');
  }
}
