jsDepStorePathArr=($jsDepStorePaths)
usedJsDepScriptLclPathArr=($usedJsDepScriptLclPaths)
for (( i=0; i<${#jsDepStorePathArr[@]}; i++ )); do
    for (( j=0; j<${#usedJsDepScriptLclPathArr[@]}; j++ )); do
        install -D --mode=u=r \
            "$(echo "${jsDepStorePathArr[$i]} \
                /lib/node_modules \
                /${usedJsDepScriptLclPathArr[$j]} \
                " | tr --delete ' ' \
            )" \
            "data/pub/auto/dev-tmp/${usedJsDepScriptLclPathArr[$j]}"
    done
done
