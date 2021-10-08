FROM python:3
WORKDIR /app/
COPY ./main.py ./
COPY ./program ./
COPY ./reserve_word ./
COPY ./grammer ./