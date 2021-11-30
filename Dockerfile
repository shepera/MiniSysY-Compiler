FROM racket/racket:8.2
WORKDIR /app/
COPY * ./
RUN rm -r grammer
RUN mkdir grammer
COPY ./grammer/ ./grammer/
RUN racket print-llvm-ir.rkt > 1