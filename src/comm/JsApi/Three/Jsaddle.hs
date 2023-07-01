module JsApi.Three.Jsaddle where


import Control.Monad
import Language.Javascript.JSaddle
import Miso.String


newtype Tpl7 a a1 a2 a3 a4 a5 a6 = Tpl7 (a, a1, a2, a3, a4, a5, a6)

instance
    ( ToJSVal a
    , ToJSVal a1
    , ToJSVal a2
    , ToJSVal a3
    , ToJSVal a4
    , ToJSVal a5
    , ToJSVal a6
    )
    =>
    MakeArgs (Tpl7 a a1 a2 a3 a4 a5 a6) where
        makeArgs (Tpl7 (a, a1, a2, a3, a4, a5, a6)) = do
            a' <- toJSVal a
            a1' <- toJSVal a1
            a2' <- toJSVal a2
            a3' <- toJSVal a3
            a4' <- toJSVal a4
            a5' <- toJSVal a5
            a6' <- toJSVal a6
            return [a', a1', a2', a3', a4', a5', a6']


ambLight :: JSVal -> JSM JSVal
ambLight = new $ three ! "AmbientLight"

axesHlpr :: Int -> JSM JSVal
axesHlpr size = new (three ! "AxesHelper") [size]

circGeom :: Double -> Int -> Double -> Double -> JSM JSVal
circGeom r segs thetaS thetaLen
    =
    new (three ! "CircleGeometry") (r, segs, thetaS, thetaLen)

gltfLoader :: JSM JSVal
gltfLoader = new (three ! "GLTFLoader") ()

gridHlpr :: Double -> Double -> JSM JSVal
gridHlpr size divs = new (three ! "GridHelper") (size, divs)

grp :: JSM JSVal
grp = new (three ! "Group") ()

mesh :: JSVal -> JSVal -> JSM JSVal
mesh geom' mat = new (three ! "Mesh") (geom', mat)

meshBscMat :: JSVal -> JSM JSVal
meshBscMat = new $ three ! "MeshBasicMaterial"

orbCtrls :: JSVal -> JSVal -> JSM JSVal
orbCtrls cam domElem' = new (three ! "OrbitControls") (cam, domElem')

plnGeom :: Double -> Double -> JSM JSVal
plnGeom w h = new (three ! "PlaneGeometry") (w, h)

prspCam :: Double -> Double -> Double -> Double -> JSM JSVal
prspCam fov asp near far
    =
    new (three ! "PerspectiveCamera") (fov, asp, near, far)

raycstr :: JSM JSVal
raycstr = new (three ! "Raycaster") ()

scn :: JSM JSVal
scn = new (three ! "Scene") ()

vec2 :: Double -> Double -> JSM JSVal
vec2 x' y' = new (three ! "Vector2") (x', y')

webGlRndrer :: JSVal -> JSM JSVal
webGlRndrer = new $ three ! "WebGLRenderer"


children :: JSVal -> JSM JSVal
children = (! "children")

domElem :: JSVal -> JSM JSVal
domElem = (! "domElement")

enabled :: JSVal -> JSM Bool
enabled = valToBool . (! "enabled")

geom :: JSVal -> JSM JSVal
geom = (! "geometry")

getObjByName :: JSVal -> String -> JSM JSVal
getObjByName this name' = this # "getObjectByName" $ [name']

intersectObj :: JSVal -> JSVal -> JSM JSVal
intersectObj = (# "intersectObject")

intersectObjs :: JSVal -> JSVal -> JSM JSVal
intersectObjs = (# "intersectObjects")

name :: JSVal -> JSM JSString
name = valToStr . (! "name")

obj :: JSVal -> JSM JSVal
obj = (! "object")

pnt :: JSVal -> JSM JSVal
pnt = (! "point")

prnt :: JSVal -> JSM JSVal
prnt = (! "parent")

pos :: JSVal -> JSM JSVal
pos = (! "position")

rot :: JSVal -> JSM JSVal
rot = (! "rotation")

scl :: JSVal -> JSM JSVal
scl = (! "scale")

scn' :: JSVal -> JSM JSVal
scn' = (! "scene")

three :: JSM JSVal
three = jsg "THREE"

x :: JSVal -> JSM Double
x = valToNumber . (! "x")

y :: JSVal -> JSM Double
y = valToNumber . (! "y")

z :: JSVal -> JSM Double
z = valToNumber . (! "z")


add :: JSVal -> JSVal -> JSM ()
add this = void . (this # "add")

clr :: JSVal -> JSM ()
clr this = void $ this # "clear" $ ()

copy :: JSVal -> JSVal -> JSM ()
copy this = void . (this # "copy")

loadGltf :: JSVal -> MisoString -> JSVal -> JSVal -> JSVal -> JSM ()
loadGltf loader gltfName sucCb procCb failCb
    =
    void $ loader # "load" $ (gltfName, sucCb, procCb, failCb)

rmFromPrnt :: JSVal -> JSM ()
rmFromPrnt this = void $ this # "removeFromParent" $ ()

rndr :: JSVal -> JSVal -> JSVal -> JSM ()
rndr this scn'' cam = void $ this # "render" $ (scn'', cam)

setEnabled :: JSVal -> Bool -> JSM ()
setEnabled = (<# "enabled")

setName :: JSVal -> String -> JSM ()
setName = (<# "name")

setRaycstrFromCam :: JSVal -> JSVal -> JSVal -> JSM ()
setRaycstrFromCam raycstr' coords cam
    =
    void $ raycstr' # "setFromCamera" $ (coords, cam)

setSize :: JSVal -> Double -> Double -> JSM ()
setSize this w h = void $ this # "setSize" $ (w, h)

setX, setY, setZ :: SetAx
setX = setAx "x"
setY = setAx "y"
setZ = setAx "z"

setAx :: AxName -> SetAx
setAx axName this = void . (this <# axName)


type AxName = String
type SetAx = JSVal -> Double -> JSM ()
