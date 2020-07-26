# mini-aes
[![GitHub CI](https://github.com/czwinzscher/mini-aes/workflows/CI/badge.svg)](https://github.com/czwinzscher/mini-aes/actions)

Program to encrypt or decrypt 16 bit messages using [Mini AES](https://piazza.com/class_profile/get_resource/ixlc30gojpe5fs/iyv0273azwtz4).

## installation
```bash
git clone https://github.com/czwinzscher/mini-aes.git
cd mini-aes
stack install
```

## usage
```bash
mini-aes [encrypt|decrypt] [message] [key]
# example
mini-aes encrypt "1000 0111 1111 1011" "1100 0011 1111 00000"
mini-aes decrypt "0110 1101 1111 0111" "1100 0011 1111 00000"
```
