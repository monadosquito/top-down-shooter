module JsApi.Three.Ffi where

import Miso.String
import Language.Javascript.JSaddle


foreign import javascript
    "new THREE['AmbientLight']"
    ambLight :: JSVal -> IO JSVal

foreign import javascript
    "new THREE['AxesHelper']($1)"
    axesHlpr :: Int -> IO JSVal

foreign import javascript
    "new THREE['CircleGeometry']($1, $2, $3, $4)"
    circGeom :: Double -> Int -> Double -> Double -> IO JSVal

foreign import javascript
    "new THREE['GLTFLoader']"
    gltfLoader :: IO JSVal

foreign import javascript
    "new THREE['GridHelper']($1, $2)"
    gridHlpr :: Double -> Double -> IO JSVal

foreign import javascript
    "new THREE['Group']"
    grp :: IO JSVal

foreign import javascript
    "new THREE['Mesh']($1, $2)"
    mesh :: JSVal -> JSVal -> IO JSVal

foreign import javascript
    "new THREE['MeshBasicMaterial']($1)"
    meshBscMat :: JSVal -> IO JSVal

foreign import javascript
    "new THREE['OrbitControls']($1, $2)"
    orbCtrls :: JSVal -> JSVal -> IO JSVal

foreign import javascript
    "new THREE['PlaneGeometry']($1, $2)"
    plnGeom :: Double -> Double -> JSM JSVal

foreign import javascript
    "new THREE['PerspectiveCamera']($1, $2, $3, $4)"
    prspCam :: Double -> Double -> Double -> Double -> IO JSVal

foreign import javascript
    "new THREE['Raycaster']"
    raycstr :: IO JSVal

foreign import javascript
    "new THREE['Scene']"
    scn :: IO JSVal

foreign import javascript
    "new THREE['Scene']($1, $2)"
    vec2 :: Double -> Double -> IO JSVal

foreign import javascript
    "new THREE['WebGLRenderer']($1)"
    webGlRndrer :: JSVal -> IO JSVal


foreign import javascript
    "$1.children"
    children :: JSVal -> IO JSVal

foreign import javascript
    "$1.domElement"
    domElem :: JSVal -> IO JSVal

foreign import javascript
    "$1.enabled"
    enabled :: JSVal -> IO Bool

foreign import javascript
    "$1.geometry"
    geom :: JSVal -> IO JSVal

foreign import javascript
    "$1.getObjectByName($2)"
    getObjByName :: JSVal -> JSString -> IO JSVal

foreign import javascript
    "$1.intersectObject($2)"
    intersectObj :: JSVal -> JSVal -> IO JSVal

foreign import javascript
    "$1.intersectObjects($2)"
    intersectObjs :: JSVal -> JSVal -> IO JSVal

foreign import javascript
    "$.name"
    name :: JSVal -> IO JSString

foreign import javascript
    "$1.object"
    obj :: JSVal -> IO JSVal

foreign import javascript
    "$1.pnt"
    pnt :: JSVal -> IO JSVal

foreign import javascript
    "$1.parent"
    prnt :: JSVal -> IO JSVal

foreign import javascript
    "$1.position"
    pos :: JSVal -> IO JSVal

foreign import javascript
    "$1.rotation"
    rot :: JSVal -> IO JSVal

foreign import javascript
    "$1.scale"
    scl :: JSVal -> IO JSVal

foreign import javascript
    "$1.scene"
    scn' :: JSVal -> IO JSVal

foreign import javascript
    "$1.x"
    x :: JSVal -> IO Double

foreign import javascript
    "$1.y"
    y :: JSVal -> IO Double

foreign import javascript
    "$1.z"
    z :: JSVal -> IO Double


foreign import javascript
    "$1.add($2)"
    add :: JSVal -> JSVal -> IO ()

foreign import javascript
    "$1.clear()"
    clr :: JSVal -> IO ()

foreign import javascript
    "$1.copy($2)"
    copy :: JSVal -> JSVal -> IO ()

foreign import javascript
    "$1.load($2, $3, $4)"
    loadGltf :: JSVal -> MisoString -> JSVal -> JSVal -> JSVal -> IO ()

foreign import javascript
    "$1.removeFromParent()"
    rmFromPrnt :: JSVal -> IO ()

foreign import javascript
    "$1.render($2, $3)"
    rndr :: JSVal -> JSVal -> JSVal -> IO ()

foreign import javascript
    "$1.enabled = $2"
    setEnabled :: JSVal -> Bool -> IO ()

foreign import javascript
    "$1.name = $2"
    setName :: JSVal -> JSString -> IO ()

foreign import javascript
    "$1.setFromCamera($1, $2)"
    setRaycstrFromCam :: JSVal -> JSVal -> JSVal -> IO ()

foreign import javascript
    "$1.setSize($2, $3)"
    setSize :: JSVal -> Int -> Int -> IO ()

foreign import javascript
    "$1.x = $2"
    setX :: JSVal -> Double -> IO ()

foreign import javascript
    "$1.y = $2"
    setY :: JSVal -> Double -> IO ()

foreign import javascript
    "$1.z = $2"
    setZ :: JSVal -> Double -> IO ()
