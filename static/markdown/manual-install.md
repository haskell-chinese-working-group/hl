### 1. 安装GHC

GHC站点上可以找到许可证协议、问答、下载链接以及版本变动的相关信息。
对于你所使用的操作系统，你可以找到使用操作系统的包管理器构建的软件
包，或者一个GHC的安装器（例如Windows系统）。

你也可以直接下载 .tar.gz/.zip 文件，解压，然后手动安装这些程序。

或者你可以选择从源码安装，请参考[这里的文档](https://ghc.haskell.org/trac/ghc/wiki/Building)。

[马上下载GHC →](https://www.haskell.org/ghc/download)

### 2. 安装Cabal-install

安装GHC后，你需要安装Haskell的包管理器：

[下载Cabal →](http://hackage.haskell.org/package/cabal-install)

下载 tar.gz 文件，解压，然后在解压得到的额目录下执行命令：

    $ sh ./bootstrap.sh

这个命令会自动下载设置Cabal-install所需要的所有额外的包。

安装完成之后，你需要将 `$HOME/.cabal/bin` 目录添加到你的 PATH 环境
变量中。你可以编译你的 `~/.bashrc` 文件，在这个文件中加入：

    export PATH=$HOME/.cabal/bin:$PATH

现在，你应该已经能够执行 cabal 命令了：

    $ cabal --version
    cabal-install version 1.18.0.2
    using version 1.18.1.2 of the Cabal library

执行下面的命令，更新 Cabal 的包缓存：

    $ cabal update

为了避免不同项目之间的冲突，你可以在沙盒中安装附件的库：

    $ mkdir my-project
    $ cd my-project
    $ cabal sandbox init
    $ cabal install the-package-name

### 3. 安装Stack

通用的Stack二进制可执行文件可以从[Stack的站点](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md#linux)下载。
