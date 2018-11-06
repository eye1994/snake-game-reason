open Reprocessing;

type cordT = (int, int);
type directionT = Left | Right | Up | Down;
type gameStateT = Playing | GameOver;
type stateT = {
  food: cordT,
  snake: list(cordT),
  scale: int,
  delay: int,
  elapsed: int,
  direction: directionT,
  dimensions: int,
  gameState: gameStateT,
  score: int,
  font: fontT
};

let headerHeight = 50;

let gameWidth = 600;
let gameHeigth = 600;
let getRandomFoodPosition = (~omitedCordinates: list(cordT), ~dimensions) => {
  let cord = ref((Random.int(dimensions), Random.int(dimensions)))

  while(
    List.exists(item => {
      let (x, y) = item;
      let (cordX, cordY) = cord^;
      x == cordX && y == cordY; 
    }, omitedCordinates)
  ) {
    cord := (Random.int(dimensions), Random.int(dimensions));
  }

  cord^;
}

let initialDimensions = 24;
let startingSnake = [(0, 0), (1, 0), (2, 0)];
let getInitialState = (env) => 
  { 
    snake: startingSnake,
    food: getRandomFoodPosition(~omitedCordinates=startingSnake, ~dimensions=initialDimensions),
    scale: 25,
    delay: 9,
    elapsed: 0,
    direction: Right,
    dimensions: initialDimensions,
    score: 0,
    gameState: Playing,
    font: Draw.loadFont(~filename="assets/3x5.fnt", ~isPixel=false, env)
  }

let setup = (env) => {
  Env.size(~width=gameWidth, ~height=gameHeigth + headerHeight, env);
  getInitialState(env)
};

let moveRight = (cell, dimensions) => {
  let (x, y) = cell;
  let nextX = x + 1;

  nextX > dimensions - 1 ? (0, y) : (nextX, y);
}

let moveLeft = (cell, dimensions) => {
  let (x, y) = cell;
  let nextX = x - 1 < 0 ? x + dimensions - 1 : x - 1;
  (nextX, y); 
}

let moveUp = (cell, dimensions) => {
  let (x, y) = cell;
  let nextY = y - 1 < 0 ? dimensions - 1: y - 1;
  (x, nextY); 
}

let moveDown = (cell, dimensions) => {
  let (x, y) = cell;
  let nextY = y + 1;
  
  nextY > dimensions - 1 ? (x, 0) : (x, nextY);
}

let moveCord = (~cell, ~direction, ~dimensions): cordT => {
  switch direction {
  | Right => moveRight(cell, dimensions)
  | Left => moveLeft(cell, dimensions)
  | Up => moveUp(cell, dimensions)
  | Down => moveDown(cell, dimensions)
  };
}

let drawSnake = (snake, scale, env) => 
  List.iter(cord => {
    let (x, y) = cord;
    Draw.fill(Utils.color(~r=255, ~g=255, ~b=255, ~a=255), env);
    Draw.rect(~pos=(x * scale, headerHeight + y * scale), ~width=scale, ~height=scale, env);
    Draw.stroke(Utils.color(~r=38, ~g=50, ~b=56, ~a=255), env);
    Draw.strokeWeight(3, env);
  }, snake);

let isOverlapingCordinates(head: cordT, food: cordT) {
  let (headX, headY) = head;
  let (foodX, foodY) = food;
  headX == foodX && headY == foodY;
}

let isEatingTale(head: cordT, tail: list(cordT)) {
  List.exists(item => isOverlapingCordinates(item, head), tail);
}

let drawFood = (food, scale, env) => {
  Draw.fill(Utils.color(~r=255, ~g=0, ~b=0, ~a=255), env);
  let (foodX, foodY) = food;
  Draw.rect(~pos=(foodX * scale, headerHeight +  foodY * scale), ~width=scale, ~height=scale, env);
}

let drawHeader = (state, env) => {
  Draw.fill(Utils.color(~r=144, ~g=164, ~b=174, ~a=252), env);
  Draw.rect(~pos=(0, 0), ~width=gameWidth, ~height=headerHeight, env);

  Draw.text(
    ~font=state.font,
    ~body=string_of_int(state.score), 
    ~pos=(gameWidth / 2, 8),
    env
  );
}

let drawBackground = (env) => {
  Draw.background(Utils.color(~r=38, ~g=50, ~b=56, ~a=255), env);
  Draw.fill(Utils.color(~r=255, ~g=0, ~b=0, ~a=255), env);
}

let getNextDirection = (state, env) => {
  let (isKeyUp, isKeyDown, isKeyLeft, isKeyRight) = (
    Env.keyPressed(Up, env),
    Env.keyPressed(Down, env),
    Env.keyPressed(Left, env),
    Env.keyPressed(Right, env),
  )

  switch (isKeyUp, isKeyDown, isKeyLeft, isKeyRight) {
    | (true, _, _, _) => Up
    | (_, true, _, _) => Down
    | (_, _, true, _) => Left
    | (_, _, _, true) => Right
    | _ => state.direction
  };
}

let draw = ({snake, food, scale} as state, env) => {
  drawBackground(env);
  drawHeader(state, env);

  if (state.gameState != GameOver) {
    drawFood(food, scale, env);
  }

  let isMoving = state.elapsed > state.delay;
  let nextDirection = getNextDirection(state, env);


  if (Env.keyPressed(Space, env) && state.gameState == GameOver) {
    getInitialState(env)
  } else if (state.gameState == GameOver) {
    let textWidth = Draw.textWidth(~font=state.font, ~body="Game Over", env);
    let subTextWidth = Draw.textWidth(~font=state.font, ~body="Press Space to restart", env);
    Draw.text(~font=state.font, ~body="Game Over", ~pos=(gameWidth / 2 - textWidth / 2, headerHeight + gameHeigth / 2 - 20), env);
    Draw.text(~font=state.font, ~body="Press Space to restart", ~pos=(gameWidth / 2 - subTextWidth / 2, headerHeight + gameHeigth / 2 + 20), env);
    state;
  } else if ((isMoving || nextDirection != state.direction) && state.gameState != GameOver) {
    let newHead = moveCord(~cell=snake -> List.rev -> List.hd, ~direction=nextDirection, ~dimensions=state.dimensions);
    let isEating = isOverlapingCordinates(newHead, food);
    let isEatingTale = isEatingTale(newHead, List.tl(snake));

    if (isEatingTale) {
      { ...state,  gameState: GameOver }
    } else {
      let newHead = isEating ?
      moveCord(~cell=newHead, ~direction=nextDirection, ~dimensions=state.dimensions) : newHead;

      let newSnake = isEating ? 
        List.append(List.tl(snake), [food, newHead]) : List.append(List.tl(snake), [newHead]);


      drawSnake(newSnake, scale, env);
      { 
        ...state, direction: nextDirection,
        snake: newSnake,
        elapsed: 0,
        score: isEating ? state.score + 1 : state.score,
        food: isEating ? getRandomFoodPosition(~omitedCordinates=newSnake, ~dimensions=state.dimensions) : food
      };
    }
  } else {
    drawSnake(snake, scale, env);
    { ...state, direction: nextDirection, elapsed: state.elapsed + 1 };
  }
};

run(~setup, ~draw, ());
