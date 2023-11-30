# Project Proposal: RPG Grid Map Adventure Game

Members: Liangchun Ma, Fan Tang, Dongming Li, Zhiqiao Gong

### Overview

This project aims to develop a role-playing game (RPG) set in a 16x16 grid map where the protagonist embarks on an adventurous journey. The game will feature a user interface (UI) that displays crucial information about the main character, such as health, coins, and other stats. As the character navigates through the grid, various events will be triggered upon entering specific tiles. These events could range from battles with monsters to choice-driven scenarios, like deciding whether to help others.

### Milestone 1: Map, UI, and Main Character Development

Develop the basic framework of the 16x16 grid map.
Design and implement the UI for displaying character information.
Create the main character with initial attributes and abilities.

### Milestone 2: Event Information Creation

Design various types of events (e.g., battles, choices) that players can encounter.
Implement the logic and mechanics for these events within the game.

### Milestone 3: RPG Storyline and Scripting

Develop a compelling storyline that ties together the game's elements.
Script the narrative elements and integrate them into the gameplay, ensuring a cohesive and immersive experience.

# Technical Considerations

The game will be developed using the Haskell programming language, with a focus on utilizing the Brick library for UI development.
Emphasis will be placed on coding best practices, ensuring the code is efficient, readable, and well-documented.
Regular code reviews and testing will be conducted to maintain quality and functionality.

## Environment Setup

### Docker

Build the image:

```bash
bash build_image.sh
```

Then build the container:

```bash
bash build_container.sh
```

Now attach to the running container in VS Code. The following steps should be executed in the container.

Install haskell extensions (and install hls following vscode's prompt).

Set up project dependencies:

```bash
stack setup --install-ghc
```

Build the project:

```bash
stack build
```

Run the built project:

```bash
stach exec cse230-exe
```
