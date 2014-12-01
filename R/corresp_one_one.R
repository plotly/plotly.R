# Convert R pch point codes to plotly "symbol" codes.
pch2symbol <- c("0"="square-open",
                "1"="circle-open",
                "2"="triangle-up-open",
                "3"="cross-thin-open",
                "4"="x-thin-open",
                "5"="diamond-open",
                "6"="triangle-down-open",
                "7"="square-x-open",
                "8"="asterisk-open",
                "9"="diamond-x-open",
                "10"="circle-cross-open",
                "11"="hexagram-open",
                "12"="square-cross-open",
                "13"="circle-x-open",
                "14"="square-open-dot",
                "15"="square",
                "16"="circle",
                "17"="triangle-up",
                "18"="diamond",
                "19"="circle",
                "20"="circle",
                "21"="circle",
                "22"="square",
                "23"="diamond",
                "24"="triangle-up",
                "25"="triangle-down",
                "32"="circle",
                "35"="hash-open",
                "42"="asterisk-open",
                "43"="cross-thin-open",
                "45"="line-ew-open",
                "47"="line-ne-open",
                "48"="circle-open",
                "79"="circle-open",
                "88"="x-thin-open",
                "92"="line-nw-open",
                "95"="line-ew-open",
                "111"="circle-open",
                "o"="circle-open",
                "O"="circle-open",
                "+"="cross-thin-open")

# Convert ggplot2 aes to plotly "marker" codes.
aes2marker <- c(alpha="opacity",
                colour="color",
                size="size",
                sizeref="sizeref",
                sizemode="sizemode",
                shape="symbol")

# Convert numeric line type.
numeric.lty <- c("0"="none",
                 "1"="solid",
                 "2"="dash",
                 "3"="dot",
                 "4"="dashdot",
                 "5"="longdash",
                 "6"="longdashdot")

# Convert named line type.
named.lty <- c("blank"="none",
               "solid"="solid",
               "dashed"="dash",
               "dotted"="dot",
               "dotdash"="dashdot",
               "longdash"="longdash",
               "twodash"="longdashdot")

# Convert coded line type.
coded.lty <- c("22"="dash",
               "42"="dot",
               "44"="dashdot",
               "13"="longdash",
               "1343"="longdashdot",
               "73"="dash",
               "2262"="dotdash",
               "12223242"="dotdash",
               "F282"="dash",
               "F4448444"="dash",
               "224282F2"="dash",
               "F1"="dash")

# Convert ggplot2 aes to line parameters.
aes2line <- c(linetype="dash",
              colour="color",
              size="width",
              direction="shape")
