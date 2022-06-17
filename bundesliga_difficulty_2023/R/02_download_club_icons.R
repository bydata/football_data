library(purrr)



target_dir <- "bundesliga_difficulty_2023/input/team_icons/"
if (!dir.exists(target_dir)) {
  dir.create(target_dir)
}

icon_urls <- c(
  "Bayern München" 	= "https://derivates.kicker.de/image/fetch/w_30,h_30,c_fit,q_auto:best/https%3A%2F%2Fsecure-mediadb.kicker.de%2F2018%2Ffussball%2Fvereine%2Fxxl%2F14_20170731800.png",
  "Borussia Dortmund" 	= "https://derivates.kicker.de/image/fetch/w_30,h_30,c_fit,q_auto:best/https%3A%2F%2Fsecure-mediadb.kicker.de%2F2009%2Ffussball%2Fvereine%2Fxxl%2F17_20150212741.png",
  "Bayer 04 Leverkusen" 	= "https://derivates.kicker.de/image/fetch/w_30,h_30,c_fit,q_auto:best/https%3A%2F%2Fsecure-mediadb.kicker.de%2F2019%2Ffussball%2Fvereine%2Fxxl%2F9_20181114991.png",
  "RB Leipzig" 	= "https://derivates.kicker.de/image/fetch/w_30,h_30,c_fit,q_auto:best/https%3A%2F%2Fsecure-mediadb.kicker.de%2F2021%2Ffussball%2Fvereine%2Fxxl%2F15778_20200716872.png",
  "1. FC Union Berlin" 	= "https://derivates.kicker.de/image/fetch/w_30,h_30,c_fit,q_auto:best/https%3A%2F%2Fsecure-mediadb.kicker.de%2F2021%2Ffussball%2Fvereine%2Fxxl%2F62_20200812304.png",
  "SC Freiburg" 	= "https://derivates.kicker.de/image/fetch/w_30,h_30,c_fit,q_auto:best/https%3A%2F%2Fsecure-mediadb.kicker.de%2F2013%2Ffussball%2Fvereine%2Fxxl%2F7_20150226260.png",
  "1. FC Köln" 	= "https://derivates.kicker.de/image/fetch/w_30,h_30,c_fit,q_auto:best/https%3A%2F%2Fsecure-mediadb.kicker.de%2F2009%2Ffussball%2Fvereine%2Fxxl%2F16_20150226713.png",
  "1. FSV Mainz 05" 	= "https://derivates.kicker.de/image/fetch/w_30,h_30,c_fit,q_auto:best/https%3A%2F%2Fsecure-mediadb.kicker.de%2F2021%2Ffussball%2Fvereine%2Fxxl%2F30_20200804894.png",
  "TSG Hoffenheim" 	= "https://derivates.kicker.de/image/fetch/w_30,h_30,c_fit,q_auto:best/https%3A%2F%2Fsecure-mediadb.kicker.de%2F2017%2Ffussball%2Fvereine%2Fxxl%2F3209_20160810540.png",
  "Borussia Mönchengladbach" 	= "https://derivates.kicker.de/image/fetch/w_30,h_30,c_fit,q_auto:best/https%3A%2F%2Fsecure-mediadb.kicker.de%2F2016%2Ffussball%2Fvereine%2Fxxl%2F15_20160215335.png",
  "Eintracht Frankfurt" 	= "https://derivates.kicker.de/image/fetch/w_30,h_30,c_fit,q_auto:best/https%3A%2F%2Fsecure-mediadb.kicker.de%2F2009%2Ffussball%2Fvereine%2Fxxl%2F32_20150225827.png",
  "VfL Wolfsburg" 	= "https://derivates.kicker.de/image/fetch/w_30,h_30,c_fit,q_auto:best/https%3A%2F%2Fsecure-mediadb.kicker.de%2F2018%2Ffussball%2Fvereine%2Fxxl%2F24_20180318873.png",
  "VfL Bochum"           = "https://derivates.kicker.de/image/fetch/w_30,h_30,c_fit,q_auto:best/https%3A%2F%2Fsecure-mediadb.kicker.de%2F2021%2Ffussball%2Fvereine%2Fxxl%2F8_20200804901.png",
  "FC Augsburg" 	= "https://derivates.kicker.de/image/fetch/w_30,h_30,c_fit,q_auto:best/https%3A%2F%2Fsecure-mediadb.kicker.de%2F2016%2Ffussball%2Fvereine%2Fxxl%2F91_20160502546.png",
  "VfB Stuttgart" 	= "https://derivates.kicker.de/image/fetch/w_30,h_30,c_fit,q_auto:best/https%3A%2F%2Fsecure-mediadb.kicker.de%2F2015%2Ffussball%2Fvereine%2Fxxl%2F11_20150226154.png",
  "Hertha BSC" 	= "https://derivates.kicker.de/image/fetch/w_30,h_30,c_fit,q_auto:best/https%3A%2F%2Fsecure-mediadb.kicker.de%2F2013%2Ffussball%2Fvereine%2Fxxl%2F29_20150226842.png",
  "FC Schalke 04" = "https://derivates.kicker.de/image/fetch/w_30,h_30,c_fit,q_auto:best/https://mediadb.kicker.de/2009/fussball/vereine/xxl/2.png",
  "Werder Bremen" = "https://derivates.kicker.de/image/fetch/w_30,h_30,c_fit,q_auto:best/https://mediadb.kicker.de/2009/fussball/vereine/xxl/4_20150225702.png"
)


download.file(icon_urls, method = "libcurl", mode = "wb", 
              destfile = paste0(target_dir, "team_2023_", stringr::str_pad(1:18, side = "left", pad = "0", width = 2), ".png"))

icon_files <- list.files(target_dir, pattern = "*.png")
names(icon_files) <- names(icon_urls)
icon_files

# check of all plots are in png format
walk(file.path(target_dir, icon_files), function(x) {message(x)
  png::readPNG(x)})




