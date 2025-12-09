FROM debian:trixie-slim AS base

# Install system dependencies including OPAM
RUN apt-get update && apt-get install -y \
    curl \
    wget \
    git \
    build-essential \
    pkg-config \
    libgmp-dev \
    zlib1g-dev \
    hyperfine \
    opam \
    && rm -rf /var/lib/apt/lists/*

# Set up OPAM environment
ENV OPAMROOT=/root/.opam

# Initialize OPAM and install OCaml 5.4.0
RUN opam init --disable-sandboxing --yes && \
    opam switch create 5.4.0 && \
    eval $(opam env --switch=5.4.0) && \
    opam install --yes \
        dune \
        ocamlformat \
        ounit2 \
        base \
        stdio \
        ocaml-lsp-server

# Set PATH to include the correct switch
ENV PATH=/root/.opam/5.4.0/bin:$PATH

# Set working directory
WORKDIR /app

# Copy project files
COPY . .

# Default command
CMD ["bash"]
