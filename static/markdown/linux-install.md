## 基于系统包管理器的安装方法

### Ubuntu

安装GHC和Cabal的步骤：

    sudo apt-get update
    sudo apt-get install -y software-properties-common
    sudo add-apt-repository -y ppa:hvr/ghc
    sudo apt-get update
    sudo apt-get install -y cabal-install-1.22 ghc-7.10.3
    cat >> ~/.bashrc <<EOF
    export PATH="\$HOME/.cabal/bin:/opt/cabal/1.22/bin:/opt/ghc/7.10.3/bin:\$PATH"
    EOF
    export PATH=~/.cabal/bin:/opt/cabal/1.22/bin:/opt/ghc/7.10.3/bin:$PATH

在[Stack的站点](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md#ubuntu)
上可以找到安装Stack的步骤。

### Debian (jessie)

安装GHC和Cabal的步骤：

    echo 'deb http://ftp.debian.org/debian/ jessie-backports main' | sudo tee /etc/apt/sources.list.d/backports.list
    sudo apt-get update && apt-get -t jessie-backports install ghc cabal-install
    cabal update && echo export PATH='$HOME/.cabal/bin:$PATH' >> $HOME/.bashrc

在[Stack的站点](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md#ubuntu)
上可以找到安装Stack的步骤。

### Fedora 22

在Fedora的官方仓库中可以找到GHC 7.8.4：

    sudo dnf install ghc
    sudo dnf install cabal-install

在[Stack的站点](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md#ubuntu)
上可以找到安装Stack的步骤。

### Fedora 21

从非官方的仓库安装GHC 7.8.4的步骤：

    sudo yum-config-manager --add-repo https://copr.fedoraproject.org/coprs/petersen/ghc-7.8.4/repo/fedora-21/petersen-ghc-7.8.4-fedora-21.repo
    sudo yum install ghc cabal-install

根据 [petersen/ghc-7.8.4 copr page](https://copr.fedoraproject.org/coprs/petersen/ghc-7.8.4/) 上的说明，
这个版本的GHC不能与Fedo/EPFL官方仓库中的GHC同时安装。

如果要安装更早版本的GHC（例如7.6.x）以及Cabal（例如1.16.x），直接运行命令从官方仓库安装即可。

在[Stack的站点](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md#ubuntu)
上可以找到安装Stack的步骤。

### Arch Linux

Arch Linux的官方仓库中已经包含了`ghc`、`cabal-install`、`happy`、`alex`、`haddock`等软件包，
使用如下的命令安装：

    sudo pacman -S ghc cabal-install happy alex haddock

在[Stack的站点](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md#ubuntu)
上可以找到安装Stack的步骤。

### Generic Tarballs

通用的最小化安装器适用于如今大多数的Linux发行版，可以从[Haskell Platform](https://www.haskell.org/platform/linux.html#linux-generic)获取。
