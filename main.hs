import Graphics.UI.GLUT
import World

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
  loadIdentity
  preservingMatrix $ do
    translate $ Vector3 (-0.5::GLfloat) (-0.5::GLfloat) 0
    drawWorld $ generate
  flush
