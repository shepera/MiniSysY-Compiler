FROM python:3
WORKDIR /app/
COPY ./pre-lexer/ .
WORKDIR /app/pre-lexer