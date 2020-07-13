# cognitive_modeling: Moon landing project

Implementation of a cognitive model (based on ACT-R) which can play a game of "Moon Landing" using different cognitive strategies. 

<img src="https://github.com/janinaschuhmacher/cognitive_modeling/blob/master/Moonlander_screenshot.png" alt="screenshot of moonlanding game" width="350" height="350">


## Game
The [ACT-R model](moonlanding-very-final-JA.lsp) simulates a cognitive agent (in the screenshot: blue square with a circle inside) that falls down from somewhere above the moon (top of the screen). The cognitive agent has the goal to reach the save landing platform (green box). It has only limited vision of its surroundings. To land safely, the agent has to avoid colliding with obstacles (red boxes with an X inside), as well as missing the platform at landing. During the game, the agent falls steadily box by box but it can choose to make a limited number of moves to the left or right before each fall. The game was implemented at the chair for Applied Cognitive Modeling at the technical university of Berlin.

## The cognitive model
The model attempts to capture how a human would behave in the situation. Thus, it follows two primary strategies. Firstly, it pursues hierarchical goals, namely the global goal to reach the platform and the local goal to not collide with an obstacle. Secondly, it internally simulates a couple of steps before making a decision to move to the left, to the right or to stay in place.
