# Arkham Horror — 中文本地化补丁层
# 仓库只提交「可读的翻译源 + 注入脚本」，镜像构建时自动把中文注入官方前端 chunk。
# 不修改任何上游源码，也不提交任何编译产物（.js bundle）。
#
# 构建：  docker build -t arkham-horror-zh:latest ./deploy
# 运行：  docker run -p 3000:3000 arkham-horror-zh:latest
# 接现有 compose：把 web 服务的 image 改为 arkham-horror-zh:latest 后 docker compose up -d

# 阶段 1：用 node 把翻译注入官方 zh chunk
FROM node:20-slim AS patch
WORKDIR /build
COPY zh_patch.mjs apply_patch.mjs ./
# 从官方镜像取出「原始」zh chunk 作为注入基底
COPY --from=halogenandtoast/arkham-horror:latest \
     /opt/arkham/src/frontend/dist/assets/zh-P5ZxcOoo.js /orig/zh-P5ZxcOoo.js
ENV AH_SRC=/orig/zh-P5ZxcOoo.js AH_OUT=/patched/zh-P5ZxcOoo.js
RUN node apply_patch.mjs

# 阶段 2：在官方镜像基础上覆盖为中文 chunk，并修正 index.html 的 lang
FROM halogenandtoast/arkham-horror:latest
USER root
COPY --from=patch /patched/zh-P5ZxcOoo.js /opt/arkham/src/frontend/dist/assets/zh-P5ZxcOoo.js
RUN sed -i 's/lang="en"/lang="zh"/' /opt/arkham/src/frontend/dist/index.html \
 && chmod 644 /opt/arkham/src/frontend/dist/assets/zh-P5ZxcOoo.js \
             /opt/arkham/src/frontend/dist/index.html
