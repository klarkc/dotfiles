FROM runfusion/fusion:latest

USER root

RUN apt-get update \
  && apt-get install -y --no-install-recommends python3 make g++ \
  && npm install -g node-pty @tobilu/qmd \
  && npm cache clean --force \
  && apt-get purge -y --auto-remove make g++ \
  && rm -rf /var/lib/apt/lists/*

ENV NODE_PATH=/usr/local/lib/node_modules

USER node
