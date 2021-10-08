FROM python:3
WORKDIR /app/
COPY ./test.py ./
RUN python3 test.py