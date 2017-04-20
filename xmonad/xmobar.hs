Config { font = "xft:Fira Code:pixelsize=15"
       , additionalFonts = ["xft:FontAwesome:pixelsize=15"]
       , borderColor = "black"
       , border = BottomB
       , bgColor = "#002b36"
       , fgColor = "#657b83"
       , alpha = 255
       , position = Top
       , lowerOnStart = True
       , allDesktops = True
       , overrideRedirect = False
       , commands = [ Run Weather "CYVR" ["-t","YVR: <tempC>ºC",
                                          "-L","18","-H","25",
                                          "--normal","#657b83",
                                          "--high","#657b83",
                                          "--low","#657b83"] 36000
                    , Run Date "%H:%M" "date" 10
                    , Run StdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = " %StdinReader% }{ %CYVR% <fc=#b58900>%date%</fc> "
       }
