FROM amazonlinux:2023

RUN <<EOF
yum update -y
yum install -y gcc gcc-c++ gmp-devel zlib-devel
EOF

RUN <<EOF
export BOOTSTRAP_HASKELL_NONINTERACTIVE=1
export BOOTSTRAP_HASKELL_GHC_VERSION=9.12.2
export BOOTSTRAP_HASKELL_CABAL_VERSION=3.14.2.0
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
EOF

ENV PATH="/root/.ghcup/bin:$PATH"

WORKDIR /build

COPY *.cabal cabal.project* /build/
RUN cabal update && cabal build --only-dependencies

COPY . /build
RUN cabal build --builddir=/tmp/build-output
