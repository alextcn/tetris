# Tetris

### Развертывание окружения для разработки
1. Скачать и установить [stack](https://github.com/commercialhaskell/stack).
2. `git clone git@github.com:SkyA1ex/tetris.git`, `cd tetris`
3. Настроить ghc. Для этого необходимо вызвать `stack setup`, которая либо найдет компилятор в переменной `PATH`, либо загрузит его в глобальную директорию stack (по-умолчанию `~./stack`).
4. Собрать проект, вызвав `stack build`. Это необходимо делать каждый раз перед запуском.
5. Запустить проект `stack exec tetris-exe`. stack знает, где лежит исполняемый файл (`./stack-work`) и вызвает его.

### Структура проекта

* Директория `src/` содержит файлы исходного кода, которые будут представлять собой модули.
* Директория `app/` должна содержать только файлы, которые будут исполняемыми. В нашем случае там будет только `main :: IO ()`
* Директория `test/` нам точно не нужна)))
* `tetris.cabal` – главный конфигурационный файл проекта, в нем прописываются зависимости, которые загружаются при вызове `stack build`.
* `stack.yaml` – настройки проекта. `packages` указывает, какие пакеты собирать. В `extra-deps` добавляются дополнительные зависимости, которых нет в [LTS](https://www.stackage.org/lts-3.17). `resolver` указывает какой резолвер использовать, от этого зависит версия ghc и используемых библиотек, которые резолвер предоставляет.

### Подключение модулей
В ходе разработки могут понадобиться новые библиотеки или модули, которые не распознаются по-умолчанию. Для этого необходимо добавть зависимость в используемый компонент в файле `tetris.cabal`. Например, подключив модуль `import Data.Text.IO`, нужно добавить `text` в `build-depends` для данного компонента `library`. 
```
library
  hs-source-dirs:      src
  exposed-modules:     Lib
  build-depends:       base >= 4.7 && < 5
                       -- This next line is the new one
                     , text
  default-language:    Haskell2010
  ```
Таким образом будет при сборке будет загружена добавленная библиотека из [LTS](https://www.stackage.org/lts-3.17).
Может оказаться, что библиотеки в LTS нет, тогда необходимо дополнительно добавить в `extra-deps` в файл `stack.yaml`. Можно посмотреть как это работает на примере используемой в проекте графической библиотеки `gloss-1.9.4.1`.

### Git
В проекте используется [A successful Git branching model](http://nvie.com/posts/a-successful-git-branching-model/) ([перевод](http://habrahabr.ru/post/106912/)).
Имеются две главные ветки `master` и `dev`. Разработка ведется в `dev`, либо в `feature-[name]` ветках, которые сливаются. В `master` всегда имеется работающая версия приложения.

