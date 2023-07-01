readonly badUsageErr='The --help/-H flag is only available!'
readonly gcrootsUnsetErr="\
The GCROOTS environment variable must be set and point to a directory!\
"
readonly pureNixShellErr="\
The nix-shell command must be run without the --pure flag!\
"
readonly helpMsg="
gcroot-dev-deps-outs [--help/-H]
Make gcroots for the development dependencies outputs\
    to keep them when garbage collecting

$gcrootsUnsetErr
$pureNixShellErr

--help -- Print this message.\
"


while (( $# )); do
    case $1 in
        --help | -H)
            help=true

            echo -e "$helpMsg" | tr --squeeze-repeats ' '
            ;;
        *)
            echo $badUsage
            exit 1
            ;;
    esac
    shift
done

if [[ ! $help ]]; then
    if [[ $IN_NIX_SHELL = pure ]]; then
        echo $pureNixShellErr
        exit 1
    elif [[ ! $GCROOTS ]]; then
        echo $gcrootsUnsetErr
        exit 1
    fi

    nix-store --indirect --realise --add-root $GCROOTS/project-prod \
        $(nix-store --query --references \
            $(nix-instantiate --quiet --quiet default.nix) \
        )
fi
