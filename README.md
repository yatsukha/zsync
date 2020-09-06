# zsync

Rsync-like backup manager written using [ZIO](https://zio.dev/) entirely for the purpose of learning functional effects.

## Building

Assuming you have `sbt` and `GraalVM`'s `native-image` in the path, use the provided `package.sh` script to create a standalone native executable. This will create an executable named `zsync` in the top directory of this project.

## Using

See `zsync help`.