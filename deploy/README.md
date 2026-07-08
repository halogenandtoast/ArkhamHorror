# Arkham Horror — 中文本地化补丁层

把 Arkham Horror 网页版里**未中文化的英文 UI 按钮/菜单**补成中文，做成一层可复现的 Docker 镜像补丁，**不改动上游任何源码**。

## 为什么会有英文按钮
前端按语言把翻译拆成独立 JS chunk。官方中文 chunk（`zh-P5ZxcOoo.js`）比英文少约 2,815 个 key，缺失的 key 会回退显示英文。本补丁只覆盖其中**界面控件类（约 253 条）**最显眼的英文文本；战役剧情、场景/卡牌专有名词（约 2,500 条）按约定暂保留英文。

## 本目录内容
| 文件 | 作用 |
|------|------|
| `zh_patch.mjs` | **翻译源（唯一事实来源）**：扁平 `key: 中文` 共 253 条，已保留 i18next 占位符 `{name}`、`@:{slot}`、`{'{'}curse{'}'}` 及 `<br>`/`\|` 结构。 |
| `apply_patch.mjs` | 注入脚本：把 `zh_patch.mjs` 深合并进官方 zh chunk，并可选修正 `index.html` 的 `lang`。 |
| `Dockerfile` | 多阶段构建：阶段1用 node 注入翻译，阶段2覆盖进官方镜像并改 `lang="zh"`。 |

> 仓库**不提交**任何 `.js` bundle 编译产物，镜像构建时才生成修补后的 chunk。

## 本地构建镜像
```bash
docker build -t arkham-horror-zh:latest ./deploy
docker run -p 3000:3000 arkham-horror-zh:latest
```

## 接入现有 docker compose
把 compose 里 web 服务的 `image:` 改为 `arkham-horror-zh:latest`，然后：
```bash
docker compose up -d
```

## 只想要修补后的 chunk（不重建镜像）
```bash
# 用官方镜像里的原始 chunk 作为基底
docker run --rm -v "$PWD/out:/patched" \
  --entrypoint sh halogenandtoast/arkham-horror:latest \
  -c "cp /opt/arkham/src/frontend/dist/assets/zh-P5ZxcOoo.js /orig.js"
# 本地注入
AH_SRC=out/zh-P5ZxcOoo.js AH_OUT=out/zh-P5ZxcOoo.patched.js \
  node deploy/apply_patch.mjs
```

## 如何增删翻译
直接编辑 `zh_patch.mjs`（保持 `key: "中文"`，勿删占位符），重新 `docker build` 即可。
若日后要把战役/卡牌专有名词也中文化，把对应 key 追加进 `zh_patch.mjs` 即可，无需触碰源码。

## 提交到 GitHub
本目录已提交到仓库（分支 `zh-localization`），通过 GitHub 连接器推送，未包含任何 token。
