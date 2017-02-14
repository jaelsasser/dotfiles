Config { font = "xft:Hasklig:pixelsize=15"
       , additionalFonts = []
       , borderColor = "black"
       , border = BottomB
       , bgColor = "#002b36"
       , fgColor = "#657b83"
       , alpha = 255
       , position = Top
       , lowerOnStart = True
       , persistent = True
       , hideOnStart = False
       , allDesktops = True
       , overrideRedirect = True
       , commands = [ Run Weather "CYVR" ["-t","YVR: <tempC>C",
                                          "-L","18","-H","25",
                                          "--normal","#657b83",
                                          "--high","#657b83",
                                          "--low","#657b83"] 36000
                    , Run DynNetwork ["-t","<dev>: <rx>KB,<tx>KB",
                                          "-L","0","-H","4096",
                                          "--normal","#647b83",
                                          "--high","#cb4b16"] 10
                    , Run Cpu ["-L","3","-H","50",
                               "--normal","#657b83",
                               "--high","#cb4b16"] 10
                    , Run Memory ["-t","Mem: <usedratio>%"] 10
                    , Run Date "%H:%M" "date" 10
                    , Run StdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = " %StdinReader% }\
                    \{ %cpu% | %memory% | %dynnetwork% | %CYVR% | <fc=#b58900>%date%</fc> "
       }
