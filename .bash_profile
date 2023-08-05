export CONDA_PATH="$HOME/mambaforge/bin:$HOME/mambaforge/condabin:"

if [ -r ~/.bashrc ]; then
   source ~/.bashrc
fi

if [ "$HOSTNAME" = "minibaps" ]; then
    OPENAI_API_KEY=$(pass openai)
    RMK_PWD=$(pass rmk)

    binance_api=$(pass binance/binance_api)
    binance_secret=$(pass binance/binance_secret)

    testnet_api=$(pass binance/testnet_api)
    testnet_secret=$(pass binance/testnet_secret)
fi
