# Pandemic – OCaml Game Project

A simple cooperative game implemented in **OCaml**, inspired by the popular board game *Pandemic*.  
Developed over a two-week period as part of a third-year Computer Science (L3) project.

---

## 👥 Authors

- [Paul Vie]  
- [Joseph Servigne]  
- [Cedrick Wong]  
- [Maxime Montre]

---

## Description

This is a lightweight terminal-based version of the **Pandemic** game designed for 2 to 4 players.  
Each player receives a set of action cards used to treat and cure global diseases.  

### Objectives
- Cure all **4 diseases** to win the game.
- The game is lost if:
  - **8 outbreaks** occur, or
  - There are **no more cards** left to draw from the deck.

---

## How to Play

### Setup
```bash
dune build @fmt --auto-promote --profile release bin/main.exe

dune exec --profile release bin/main.exe

