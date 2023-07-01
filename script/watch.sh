readonly appPartNotPassedErr='The application part argument must be specified!'
readonly helpMsg="
watch [--help | --port=/-P=] {cold | loc-ed | pref-ed | warm}
When the .hs or .sass files change:
    Kill the Warp server if it is run.
    Start the Warp server on the --port/-P(8000).
    Serve a specified application part. 
    Refresh the browser tab.

$appPartNotPassedErr

--help -- Print this message.\
"

mkBadUsgErr () {
    echo "The $1 parameter is unknown!"
}
mkGhciCmd () {
    echo "
        Language.Javascript.JSaddle.Warp.debugOr $port (inclPub \"$1\" app) \
            =<< loadPub \"$1\" app
    "
}


port=8000

while (( $# )); do
    case ${1%=*} in
        --help | -H)
            help=true
            echo -e "$helpMsg"
            ;;
        --port | -P)
            port=${1#*=}
            ;;
        cold | warm)
            appPartPassed=true
            ghcid \
                --clear \
                --warnings \
                --command="$(echo " \
                    ghci \
                    -D=${1^^}_PLATF \
                    -D=DEV \
                    -Wall \
                    -fdiagnostics-color=always \
                    -i=src/comm \
                    -i=src/dev-comm \
                    -i=src/main \
                    Main \
                    " | tr --squeeze-repeats ' ')" \
                --reload=data/pub/auto/dev-tmp/$1-style.css \
                --run="$(mkGhciCmd $1)" &
            sass \
                --watch \
                src/main/Dtl/View/Comm/Root/${1^}.sass \
                data/pub/auto/dev-tmp/$1-style.css \
            ;;
        loc-ed | pref-ed)
            appPartPassed=true
            ghcid \
                --clear \
                --warnings \
                --command="$(echo " \
                    ghci \
                    -D=DEV \
                    -Wall \
                    -fdiagnostics-color=always \
                    -i=data/auto \
                    -i=src/comm \
                    -i=src/dev-comm \
                    -i=src/expn-tool/$1 \
                    -i=src/expn-tool/comm \
                    -i=/home/monadosquito/work/bem/src \
                    -i=/home/monadosquito/work/miso-bem/src \
                    Main \
                    " | tr --squeeze-repeats ' ')" \
                --reload=data/pub/auto/dev-tmp/$1-style.css \
                --run="$(mkGhciCmd $1)" &
            sass \
                --watch \
                src/expn-tool/$1/View/Root.sass \
                data/pub/auto/dev-tmp/$1-style.css
            ;;
        *)
            echo $(mkBadUsgErr $1)
            exit 1
            ;;
    esac
    shift
done
if [ ! $appPartPassed ] && [ ! $help ]; then
    echo $appPartNotPassedErr
    exit 1
fi
