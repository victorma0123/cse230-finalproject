# cse230-finalproject
Some rough thoughts: By looking through the ex projects under brick lib, I am not sure what we are expected to dev. I will check with TA this afternoon. The following thoughts assume that we require to done something fancy.
NetWork:
If we can figure out the socket, and make network running, maybe we can implement two or three games, like user can choose playing from checkers or Rock Paper Scissors etc. There are only few tutorial to talk about how to implement the network function and is all old version.
Some network implementation: 
https://www.youtube.com/watch?v=FMJhdt9nrOE
https://wiki.haskell.org/Implement_a_chat_server
https://www.kuniga.me/blog/2015/10/26/haskell-basic-networking.html
Local:
If singal game is not enough, make be we can develop a GameCollections, like one home page, and then each of us develop a small game to choose from the home page. Like 2048, snake game, tac toe tic... There really lots of choice.


Project Proposal: RPG Grid Map Adventure Game
Overview
This project aims to develop a role-playing game (RPG) set in a 16x16 grid map where the protagonist embarks on an adventurous journey. The game will feature a user interface (UI) that displays crucial information about the main character, such as health, coins, and other stats. As the character navigates through the grid, various events will be triggered upon entering specific tiles. These events could range from battles with monsters to choice-driven scenarios, like deciding whether to help others.

Objectives
Develop a Grid-Based Map: Create a 16x16 grid that serves as the game's primary environment.
Implement a User Interface: Design and develop a UI that effectively displays the main character's information and game status.
Character Development: Craft the main character with distinct attributes and capabilities.
Event Creation: Develop various interactive events that occur throughout the game.
Narrative Integration: Weave a coherent and engaging storyline that enhances the RPG experience.
Milestones
Milestone 1: Map, UI, and Main Character Development

Develop the basic framework of the 16x16 grid map.
Design and implement the UI for displaying character information.
Create the main character with initial attributes and abilities.
Milestone 2: Event Information Creation

Design various types of events (e.g., battles, choices) that players can encounter.
Implement the logic and mechanics for these events within the game.
Milestone 3: RPG Storyline and Scripting

Develop a compelling storyline that ties together the game's elements.
Script the narrative elements and integrate them into the gameplay, ensuring a cohesive and immersive experience.
Technical Considerations
The game will be developed using the Haskell programming language, with a focus on utilizing the Brick library for UI development.
Emphasis will be placed on coding best practices, ensuring the code is efficient, readable, and well-documented.
Regular code reviews and testing will be conducted to maintain quality and functionality.
