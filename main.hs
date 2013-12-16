import Graphics.UI.GLUT

myPoints :: [ (GLfloat,GLfloat,GLfloat) ]
myPoints = [ ( sin(2*pi*k/12), cos(2*pi*k/12), 0 ) | k <- [1..10] ]

main::IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow _progName
  displayCallback $= display
  mainLoop

display :: DisplayCallback
display = do
  clear [ ColorBuffer ]
  renderPrimitive Points $
    mapM_ (\(x,y,z) -> vertex $ Vertex3 x y z) myPoints
  flush
