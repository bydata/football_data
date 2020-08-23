target_dir <- "input/team_icons/"
if (!dir.exists(target_dir)) {
  dir.create(target_dir)
}

# icon_urls <- c(
#   "Bayern München" = "https://assets.dfb.de/uploads/000/201/052/small_FC_Bayern_Muenchen.jpg?1560415949",
#   "Borussia Dortmund" = "https://assets.dfb.de/uploads/000/018/112/small_Borussia_Dortmund.jpg?1493059502",
#   "RB Leipzig" = "https://assets.dfb.de/uploads/000/082/643/small_RB_Leipzig_logo_groesser.jpg?1493109025",
#   "Borussia Mönchengladbach" = "https://assets.dfb.de/uploads/000/018/191/small_borussia-moenchengladbach.jpg?1493059528",
#   "Bayer 04 Leverkusen" = "https://assets.dfb.de/uploads/000/165/430/small_Bayer_04_Leverkusen_Logo_2018.gif?1523897293",
#   "FC Schalke 04" = "https://assets.dfb.de/uploads/000/018/122/small_FC_Schalke.jpg?1493059504",
#   "Eintracht Frankfurt" = "https://assets.dfb.de/uploads/000/018/117/small_Eintracht_Frankfurt.jpg?1493059503",
#   "VfL Wolfsburg" = "https://assets.dfb.de/uploads/000/018/239/small_vfl-wolfsburg.jpg?1493059544",
#   "Hertha BSC" = "https://assets.dfb.de/uploads/000/018/129/small_Hertha_BSC.jpg?1493059507",
#   "TSG Hoffenheim" = "https://assets.dfb.de/uploads/000/113/632/small_1899_hoffenheim.jpg?1493132487",
#   "Werder Bremen" = "https://assets.dfb.de/uploads/000/018/246/small_werder-bremen.jpg?1493059547",
#   "1. FC Köln" = "https://assets.dfb.de/uploads/000/018/194/small_fc-koeln.jpg?1493059529",
#   "1. FSV Mainz 05" = "https://assets.dfb.de/uploads/000/200/976/small_FSV_Mainz_05_500x500.jpg?1560342690",
#   "SC Freiburg" = "https://assets.dfb.de/uploads/000/018/224/small_sc-freiburg.jpg?1493059539",
#   "FC Augsburg" = "https://assets.dfb.de/uploads/000/018/119/small_FC_Augsburg.jpg?1493059504",
#   "VfB Stuttgart" = "https://assets.dfb.de/uploads/000/027/692/small_vfb.jpg?1493068594",
#   "1. FC Union Berlin" = "https://assets.dfb.de/uploads/000/018/232/small_union-Berlin.jpg?1493059542",
#   "Arminia Bielefeld" = "https://assets.dfb.de/uploads/000/018/139/small_Arminia_Bielefeld.jpg?1493059508"
# )

icon_urls <- c(
  "Bayern München" 	= "https://derivates.kicker.de/image/fetch/w_30,h_30,c_fit,q_auto:best/https%3A%2F%2Fsecure-mediadb.kicker.de%2F2018%2Ffussball%2Fvereine%2Fxxl%2F14_20170731800.png",
  "Borussia Dortmund" 	= "https://derivates.kicker.de/image/fetch/w_30,h_30,c_fit,q_auto:best/https%3A%2F%2Fsecure-mediadb.kicker.de%2F2009%2Ffussball%2Fvereine%2Fxxl%2F17_20150212741.png",
  "RB Leipzig" 	= "https://derivates.kicker.de/image/fetch/w_30,h_30,c_fit,q_auto:best/https%3A%2F%2Fsecure-mediadb.kicker.de%2F2021%2Ffussball%2Fvereine%2Fxxl%2F15778_20200716872.png",
  "Borussia Mönchengladbach" 	= "https://derivates.kicker.de/image/fetch/w_30,h_30,c_fit,q_auto:best/https%3A%2F%2Fsecure-mediadb.kicker.de%2F2016%2Ffussball%2Fvereine%2Fxxl%2F15_20160215335.png",
  "Bayer 04 Leverkusen" 	= "https://derivates.kicker.de/image/fetch/w_30,h_30,c_fit,q_auto:best/https%3A%2F%2Fsecure-mediadb.kicker.de%2F2019%2Ffussball%2Fvereine%2Fxxl%2F9_20181114991.png",
  "FC Schalke 04" 	= "https://derivates.kicker.de/image/fetch/w_30,h_30,c_fit,q_auto:best/https%3A%2F%2Fsecure-mediadb.kicker.de%2F2009%2Ffussball%2Fvereine%2Fxxl%2F2.png",
  "Eintracht Frankfurt" 	= "https://derivates.kicker.de/image/fetch/w_30,h_30,c_fit,q_auto:best/https%3A%2F%2Fsecure-mediadb.kicker.de%2F2009%2Ffussball%2Fvereine%2Fxxl%2F32_20150225827.png",
  "VfL Wolfsburg" 	= "https://derivates.kicker.de/image/fetch/w_30,h_30,c_fit,q_auto:best/https%3A%2F%2Fsecure-mediadb.kicker.de%2F2018%2Ffussball%2Fvereine%2Fxxl%2F24_20180318873.png",
  "Hertha BSC" 	= "https://derivates.kicker.de/image/fetch/w_30,h_30,c_fit,q_auto:best/https%3A%2F%2Fsecure-mediadb.kicker.de%2F2013%2Ffussball%2Fvereine%2Fxxl%2F29_20150226842.png",
  "TSG Hoffenheim" 	= "https://derivates.kicker.de/image/fetch/w_30,h_30,c_fit,q_auto:best/https%3A%2F%2Fsecure-mediadb.kicker.de%2F2017%2Ffussball%2Fvereine%2Fxxl%2F3209_20160810540.png",
  "Werder Bremen" 	= "https://derivates.kicker.de/image/fetch/w_30,h_30,c_fit,q_auto:best/https%3A%2F%2Fsecure-mediadb.kicker.de%2F2009%2Ffussball%2Fvereine%2Fxxl%2F4_20150225702.png",
  "1. FC Köln" 	= "https://derivates.kicker.de/image/fetch/w_30,h_30,c_fit,q_auto:best/https%3A%2F%2Fsecure-mediadb.kicker.de%2F2009%2Ffussball%2Fvereine%2Fxxl%2F16_20150226713.png",
  "1. FSV Mainz 05" 	= "https://derivates.kicker.de/image/fetch/w_30,h_30,c_fit,q_auto:best/https%3A%2F%2Fsecure-mediadb.kicker.de%2F2021%2Ffussball%2Fvereine%2Fxxl%2F30_20200804894.png",
  "SC Freiburg" 	= "https://derivates.kicker.de/image/fetch/w_30,h_30,c_fit,q_auto:best/https%3A%2F%2Fsecure-mediadb.kicker.de%2F2013%2Ffussball%2Fvereine%2Fxxl%2F7_20150226260.png",
  "FC Augsburg" 	= "https://derivates.kicker.de/image/fetch/w_30,h_30,c_fit,q_auto:best/https%3A%2F%2Fsecure-mediadb.kicker.de%2F2016%2Ffussball%2Fvereine%2Fxxl%2F91_20160502546.png",
  "VfB Stuttgart" 	= "https://derivates.kicker.de/image/fetch/w_30,h_30,c_fit,q_auto:best/https%3A%2F%2Fsecure-mediadb.kicker.de%2F2015%2Ffussball%2Fvereine%2Fxxl%2F11_20150226154.png",
  "1. FC Union Berlin" 	= "https://derivates.kicker.de/image/fetch/w_30,h_30,c_fit,q_auto:best/https%3A%2F%2Fsecure-mediadb.kicker.de%2F2021%2Ffussball%2Fvereine%2Fxxl%2F62_20200812304.png",
  "Arminia Bielefeld" 	= "https://derivates.kicker.de/image/fetch/w_30,h_30,c_fit,q_auto:best/https%3A%2F%2Fsecure-mediadb.kicker.de%2F2019%2Ffussball%2Fvereine%2Fxxl%2F10_20191127726.png"
  
)





download.file(icon_urls, method = "libcurl", mode = "wb", 
              destfile = str_c(target_dir, "team_2021_", str_pad(1:18, side = "left", pad = "0", width = 2), ".png"))

icon_files <- list.files(target_dir, pattern = "*.png")
names(icon_files) <- names(icon_urls)
icon_files
