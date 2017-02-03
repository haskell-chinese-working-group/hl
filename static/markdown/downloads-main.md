你可以通过多种方法在我们支持的平台上安装Haskell工具链。包括：

- [Minimal安装器](#minimal)：仅仅通过你的操作系统的包管理器在系统上安装
  GHC编译器和以及构建工具（主要是Cabal和Stack）。

- [Stack](#stack)：在你的系统上全局安装`stack`命令，`stack`是一个以项目为核心
  的构建工具，能够基于每个工程的设置自动下载和管理Haskell依赖。

- [Haskell Platform](#platform)：在你的系统上全局地安装GHC、Cabal和一些其他的
  工具，以及一些必要的基础库。

这些方法的主要区别在于，你需要全局安装哪些程序以及在单个工程的特定环境中需要
维护哪些内容。全局安装允许你在多个用户和项目之前共享开发环境，但在多个项目之
间存在潜在的冲突。为了避免这些冲突，上述的每个方案提供了一个轻量级的*沙盒*的
特性，用来为单个项目创建各自的构建环境。使用Minimal安装器你可以按照需求将
库安装在沙盒中，可以避免大多数的冲突。Stack将编译器、其他工具和库都放在沙盒环
境中，可以避免不同项目之间几乎所有类型的冲突。使用Haskell Platform你也可以将
库放在沙盒中，但是不包括全局安装的包含在Haskell Platform中的库。

### Haskell集成开发环境和其他的发行版

除了这些通用的、跨平台的Haskell工具链，还有一些针对不同平台并且很容易使用的
发行版和集成开发环境。Haskell Wiki上有一个包括一些
[受欢迎的发行版的列表](https://wiki.haskell.org/Distributions)。

<!-- For information on other platforms and methods, please see the section on
[third party installers](#other). -->

<hr style="height: 1px; background-color: black;" --/>

## Minimal安装器

### Minimal安装器是什么 <a name="minimal"></a>

Minimal安装器主要提供了[GHC](https://www.haskell.org/ghc)编译器，以及用于安装附
加的包的工具[Cabal](https://www.haskell.org/cabal/)
和[Stack](https://github.com/commercialhaskell/stack)。
某些版本的Minimal安装器可能还包含安装其他的工具（例如用于词法分析和语法分析的工具）。

### 你将会获得

- 仅仅包含针对各自平台的必要的核心库。
- 安装之后，你需要使用Cabal或者Stack来安装额外的包。

### 马上获取

- [Linux](/downloads/linux)
- [OS X](https://www.haskell.org/platform/mac.html)
- [Windows](https://www.haskell.org/platform/windows.html)

### 寻求帮助 <a name="help" />

- 如果需要Haskell语言本身的帮助，你可以从[Haskell文档](https://www.haskell.org/documentation)
  [Haskell Wiki](https://wiki.haskell.org/)开始。
- 如果需要关于Haskell编译器GHC[GHC](https://www.haskell.org/ghc)的帮助，可以参考
  [GHC User Manual](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/index.html)
  这份全面详尽的用户手册。
- 如果需要关于如何使用Cabal下载和创建包的帮助（参考[这里](#libraries)），可以查看Cabal的用户指南
  [Cabal User Guide](https://www.haskell.org/cabal/users-guide/)。
- 如果需要关于如何使用Stack下载和创建包的帮助，请参考[Stack的文档](#stackhelp)。
- 最后，你可以通过Freenode IRC上的Haskell的IRC频道[\#haskell IRC channel](irc://irc.freenode.net/haskell)
  向其他的Haskell用户和专家提问。

## Stack

### Stack是什么 <a name="stack"></a>

Stack是一个跨平台的Haskell构建工具，能够管理Haskell工具链（包括GHC编译器和用于Windows
系统的MSYS2），以及构建和配置包，等等。

### 你将会获得

- 下载之后，Stack能够为你下载和安装GHC以及其他的核心工具。
- 项目的开发被隔离在沙盒中，包括为给定的项目自动下载对应版本的GHC。
- Stack可以管理所有的Haskell相关的依赖，保证可以重复的构建过程。
- Stack默认会从一个包含数千个包的精心组织的仓库中获取额外的包，并能保证相互之间的兼容。
- Stack能够使用Docker来进行可重复的独立部署。

### 如何获取

[安装和升级文档](http://docs.haskellstack.org/en/stable/install_and_upgrade/)
描述了如何下载供不同平台使用的Stack，主要的版本如下：

- [Ubuntu Linux](http://docs.haskellstack.org/en/stable/install_and_upgrade/#ubuntu)
- [OS X](http://docs.haskellstack.org/en/stable/install_and_upgrade/#os-x)
- [Windows](http://docs.haskellstack.org/en/stable/install_and_upgrade/#windows)

此外，在文档中你还能找到如何在其他Linux发行版，包括Debian、Fedore、Red Hat、Nix OS
和Arch Linux上安装Stack的步骤。

### 寻求帮助 <a name="stackhelp"></a>

关于Haskell和GHC的帮助，可以参考[上文](#help)提到的内容。这里还有一些关于Stack本身
的资源：

- Stack的[README](https://github.com/commercialhaskell/stack/#readme)提供了Stack的
  介绍以及关于安装的帮助。
- 一个关于Stack的使用的[深入的指导](http://docs.haskellstack.org)。
- [Getting started with Stack](http://seanhess.github.io/2015/08/04/practical-haskell-getting-started.html)
  一文介绍了如何使用Stack来构建新的项目。
- 可以在[GitHub issue tracker](https://github.com/commercialhaskell/stack)
  反馈Stack的Bug和Feature Requests。
- [Stack的邮件列表](https://groups.google.com/d/forum/haskell-stack)。
- Freenode IRC上专门用于Stack的IRC频道[\#haskell-stack IRC channel](irc://irc.freenode.net/haskell-stack)。
- [StackOverflow 的 haskell-stack 标签](http://stackoverflow.com/questions/tagged/haskell-stack)
  下面有许多关于Stack的问题和回答。

## Haskell Platform

### Haskell Platform是什么

<a name="platform"></a>Haskell Platform是一个自成一体的、完整的安装器。下载之后，
你就能获得一系列有用的核心库，以及基于此构建Haskell程序的所有必要工具。Haskell Platform
有两个版本，一个是不包括GHC核心之外的任何库的最小版本，另一个则更加全面，包含了更多
的库并将全局安装。

### 你将会获得

- GHC编译器[Glasgow Haskell Compiler](https://www.haskell.org/ghc)
- [Cabal构建系统](https://www.haskell.org/cabal/)，可以用来安装额外的包，Cabal默认从
  Haskell的中央仓库[Hackage](https://hackage.haskell.org/)获取包的源代码。
- [Stack](http://docs.haskellstack.org)工具用于项目开发。
- 对性能剖析和代码覆盖分析的支持。
- 35个核心的、广泛使用的[库](https://www.haskell.org/platform/contents.html)。

### 马上获取

Haskell Platform以单个安装程序的形式提供，可以从下列地址下载：

- [Linux](http://www.haskell.org/platform/linux.html)
- [OS X](http://www.haskell.org/platform/mac.html)
- [Windows](http://www.haskell.org/platform/windows.html)

### 寻求帮助

- 关于[Haskell Platform提供了哪些包](https://www.haskell.org/platform/contents.html)
  的一个详尽的列表。
- [上方的帮助列表](#help)中包含了关于GHC，以及Cabal和Stack工具的帮助文档。

<hr style="height: 1px; background-color: black;" --/>

## 附加的库 <a name='libraries'></a>

Haskell中，附件的库通过Cabal包管理系统来配置和构建，关于更详细的规范，请阅读
[The Cabal User Guide](https://www.haskell.org/cabal/users-guide/)。命令行工具
`cabal`或者`stack`专门用来下载和安装额外的库，二者有着不同的工作流程。关于更详
细的使用方法，请参考上面提到的文档。

### Hackage

Hackage是任何人在任何时候都可以自由地上传库的仓库，你的库立刻就能被其他用户获
取，并且文档也会自动生成并托管在Hackage上。Cabal的安装过程用到了Hackage。

你可以通过运行下面的名来来使用Cabal安装附加的库：

    $ cabal update
    $ cabal install the-package

注意如果你没有在沙盒中运行安装命令，这个包会被全局安装，通常这并不是你期望的效果，
因此建议你在项目目录下运行`cabal sandbox init`来创建沙盒环境。

[前往 Hackage →](https://hackage.haskell.org/packages/)

### LTS Haskell

LTS Haskell是一个基于stackage的一系列受长期支持的库的集合，LTS Haskell将构建、
测试以及向后支持的问题修复集成在一起。

[获取 LTS Haskell →](http://www.stackage.org/lts)

### Stackage Nightly

Stackage Nightly是一个每日构建的稳定仓库，包含了一系列的库的快照，只有能够成功构建并
通过测试的包才会被汇集到快照中。

[获取 Stackage Nightly →](http://www.stackage.org/nightly)

### 源码仓库

你也可以直接从源码仓库安装一个额外的库，例如，源代码仓库克隆和安装network包，
你需要运行：

    $ git clone https://github.com/haskell/network
    $ cabal install network/

或者：

    $ git clone https://github.com/haskell/network
    $ cd network
    $ cabal install
