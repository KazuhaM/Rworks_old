
# font表示用 -----------------------------------------------------------------

if(require("tidyverse")){
  print("tidyverse is loaded correctly")
} else {
  print("trying to install tidyverse")
  install.packages("tidyverse")
  if(require("tidyverse")){
    print("tidyverse installed and loaded")
  } else {
    stop("could not install tidyverse")
  }
}
if(require("systemfonts")){
  print("systemfonts is loaded correctly")
} else {
  print("trying to install systemfonts")
  install.packages("systemfonts")
  if(require("systemfonts")){
    print("systemfonts installed and loaded")
  } else {
    stop("could not install systemfonts")
  }
}

# fontregistererの読み込み
# 事前に必要なパッケージのロード
library(stats)
library(grDevices)
if(require("fontregisterer")){
  print("fontregisterer is loaded correctly")
} else {
  print("trying to install fontregisterer")
  library(remotes)
  remotes::install_github("Gedevan-Aleksizde/fontregisterer", repos = NULL, type = "source")
  if(require("fontregisterer")){
    print("fontregisterer installed and loaded")
  } else {
    stop("could not install fontregisterer")
  }
}
names(windowsFonts())
# フォントの設定
# windowsの通常なら游書体が登録される
if(Sys.info()["sysname"] == "Windows"){
  if(as.integer(str_extract(Sys.info()["release"], "^[0-9]+")) >=8){
    family_jsans <- "Yu Gothic"
    family_jserif <- "Yu Mincho"
    family_sans <- "Arial"
    family_serif <- "Times New Roman"
  } else {
    family_sans <- "MS Gothic"
    family_serif <- "MS Mincho"
  }
} else if(Sys.info()["sysname"] == "Linux") {
  family_sans <- "Noto Sans CJK JP"
  family_serif <- "Noto Serif CJK JP"
} else if(Sys.info()["sysname"] == "Darwin"){
  family_serif <- "Hiragino Mincho ProN"
  family_sans <- "Hiragino Sans"
} else {
  # インストールすればとりあえず動く
  family_sans <- "Noto Sans CJK JP"
  family_serif <- "Noto Serif CJK JP"
}
