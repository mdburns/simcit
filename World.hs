module World ( World, drawWorld, generate ) where

import Graphics.UI.GLUT

data RoomType = Lab | Research | Office | MasterPit | Classroom | Atrium | Corridor deriving Eq
data WorldTileType = Grass | Building RoomType deriving Eq
data WorldTile = WorldTile ( GLfloat, GLfloat, GLfloat, WorldTileType )
data World = World [[[ WorldTile ]]]

-- define for the tile edge size
tileSize = 0.1 :: GLfloat

-- returns a new world
generate :: World
generate = World [ [ [ WorldTile ( x, y, 0, Building Classroom ) | x <- [0, tileSize .. 1-tileSize] ]
                  | y <- [0, tileSize .. 1-tileSize] ] ]

-- Returns a 3-tuple representing (R,G,B) color by tile type
tileColor :: WorldTileType -> ( GLfloat, GLfloat, GLfloat )
tileColor wtt = case wtt of
  Grass    -> ( 0.1, 0.6, 0.1 )
  Building t -> (case t of
    Lab       -> ( 0.3, 0.4, 0.8 )
    Research  -> ( 0.6, 0.4, 0.4 )
    Office    -> ( 0.4, 0.6, 0.4 )
    MasterPit -> ( 0.3, 0.7, 0.3 )
    Classroom -> ( 0.4, 0.4, 0.4 )
    Atrium    -> ( 0.8, 0.8, 0.2 )
    Corridor  -> ( 0.0, 0.0, 1.0 ) )

-- renders a world object
drawWorld :: World -> IO ()
drawWorld ( World world ) = do
  mapM_ (\ slice -> mapM_ (\ row -> mapM_ renderTile row ) slice ) world

-- renders a tile in the proper position and color
renderTile :: WorldTile -> IO ()
renderTile ( WorldTile (x, y, z, t_type) ) =
  let ( col_r, col_g, col_b ) = tileColor t_type in do
    color $ Color3 col_r col_g col_b
    renderPrimitive Quads $ do
      vertex $ Vertex3  x y z
      vertex $ Vertex3 (x+tileSize) y z
      vertex $ Vertex3 (x+tileSize) (y+tileSize) z
      vertex $ Vertex3  x (y+tileSize) z
