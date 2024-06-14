# ベースイメージを指定
FROM mcr.microsoft.com/vscode/devcontainers/base:ubuntu-20.04

# 開発環境に必要なツールをインストール
RUN apt-get update && apt-get install -y \
    sbcl \
    clisp \
    rlwrap \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

# SBCLのデフォルトコマンドをrlwrapでラップして設定
CMD ["bash", "-c", "rlwrap sbcl"]
