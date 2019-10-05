# love-jaredweakly-com

On Arch Linux, this requires having the libgmp-static package from the aur installed.

After reading the source code, I've realized that the correct handler for aws is "src/Lib.handler".
In retrospect, this was visible in the docs, just not explicitly pointed out.

```sh
aws lambda update-function-configuration --function-name test --handler src/Lib.handler
```
