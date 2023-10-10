from fastapi import FastAPI
from util import prolog

app = FastAPI()

@app.get("/consult/")
async def root(query: str):
    return prolog.query(query)
