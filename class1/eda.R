
txt = readLines("/Users/pankaj/dev/git/smu/qtw/class1/data/offline.final.trace.txt")

length(txt)

strsplit(txt[4], ";")[[1]]

unlist(lapply(strsplit(txt[4], ";")[[1]],
              function(x)
                sapply(strsplit(x, "=")[[1]], strsplit, ",")))

tokens = strsplit(txt[4], "[;=,]")[[1]]

tokens[1:10]

tokens[c(2, 4, 6:8, 10)]

tmp = matrix(tokens[ - (1:10) ], ncol = 4, byrow = TRUE)
mat = cbind(matrix(tokens[c(2, 4, 6:8, 10)], nrow = nrow(tmp),
                   ncol = 6, byrow = TRUE),
            tmp)
dim(mat)


locCounts = t(locCounts)
plot(locCounts, type = "n", xlab = "", ylab = "")
text(locCounts, labels = locCounts[,3], cex = .8, srt = 45)


processLine = function(x)
{
  tokens = strsplit(x, "[;=,]")[[1]]
  if (length(tokens) == 10)
    return(NULL)
  tmp = matrix(tokens[ - (1:10) ], , 4, byrow = TRUE)
  cbind(matrix(tokens[c(2, 4, 6:8, 10)], nrow(tmp), 6,
               byrow = TRUE), tmp)
}

lines = txt[ substr(txt, 1, 1) != "#" ]
tmp = lapply(lines, processLine)
offline = as.data.frame(do.call("rbind", tmp),stringsAsFactors = FALSE)

head(offline)

names(offline) = c("time", "scanMac", "posX", "posY", "posZ",
                   "orientation", "mac", "signal",
                   "channel", "type")

head(offline)

offline[offline$time==1139643118358,]

dim(offline)



numVars = c("time", "posX", "posY", "posZ",
            "orientation", "signal")


offline[ numVars ] = lapply(offline[ numVars ], as.numeric)

offline = offline[ offline$type == "3", ]
offline = offline[ , "type" != names(offline) ]
dim(offline)


offline$rawTime = offline$time
offline$time = offline$time/1000
class(offline$time) = c("POSIXt", "POSIXct")

unlist(lapply(offline, class))


summary(sapply(offline[ , c("mac", "channel", "scanMac")],
               as.factor))


offline = offline[ , !(names(offline) %in% c("scanMac", "posZ"))]

length(unique(offline$orientation))

plot(ecdf(offline$orientation))

roundOrientation = function(angles) {
  refs = seq(0, by = 45, length = 9)
  q = sapply(angles, function(o) which.min(abs(o - refs)))
  c(refs[1:8], 0)[q]
}

offline$angle = roundOrientation(offline$orientation)

with(offline, boxplot(orientation ~ angle,
                      xlab = "nearest 45 degree angle",
                      ylab="orientation"))

c(length(unique(offline$mac)), length(unique(offline$channel)))

table(offline$mac)

subMacs = names(sort(table(offline$mac), decreasing = TRUE))[1:7]
offline = offline[ offline$mac %in% subMacs, ]

macChannel = with(offline, table(mac, channel))
apply(macChannel, 1, function(x) sum(x > 0))

offline = offline[ , "channel" != names(offline)]

locDF = with(offline,
             by(offline, list(posX, posY), function(x) x))
length(locDF)

sum(sapply(locDF, is.null))
locDF = locDF[ !sapply(locDF, is.null) ]

length(locDF)

locCounts = sapply(locDF, nrow)

locCounts = sapply(locDF,
                   function(df)
                     c(df[1, c("posX", "posY")], count = nrow(df)))


class(locCounts)
dim(locCounts)
locCounts[ , 1:8]


locCounts = t(locCounts)
plot(locCounts, type = "n", xlab = "", ylab = "")



text(locCounts, labels = locCounts[,3], cex = .8, srt = 45)
findGlobals(readData, merge = FALSE)$variables


bwplot(signal ~ factor(angle) | mac, data = offline,
       subset = posX == 2 & posY == 12
       & mac != "00:0f:a3:39:dd:cd",
       layout = c(2,3))

summary(offline$signal)

densityplot( ~ signal | mac + factor(angle), data = offline,
             subset = posX == 24 & posY == 4 &
               mac != "00:0f:a3:39:dd:cd",
             bw = 0.5, plot.points = FALSE)

offline$posXY = paste(offline$posX, offline$posY, sep = "-")


byLocAngleAP = with(offline,
                    by(offline, list(posXY, angle, mac),
                       function(x) x))
# Then we can calculate summary statistics on each of these data frames with
signalSummary =
  lapply(byLocAngleAP,
         function(oneLoc) {
           ans = oneLoc[1, ]
           ans$medSignal = median(oneLoc$signal)
           ans$avgSignal = mean(oneLoc$signal)
           ans$num = length(oneLoc$signal)
           ans$sdSignal = sd(oneLoc$signal)
           ans$iqrSignal = IQR(oneLoc$signal)
           ans
         })
offlineSummary = do.call("rbind", signalSummary)

breaks = seq(-90, -30, by = 5)
bwplot(sdSignal ~ cut(avgSignal, breaks = breaks),
       data = offlineSummary,
       subset = mac != "00:0f:a3:39:dd:cd",
       xlab = "Mean Signal", ylab = "SD Signal")


oneAPAngle = subset(offline, mac == subMacs[5] & angle == 0)

oneAPAngle = subset(offlineSummary,
                    mac == subMacs[5] & angle == 0)


smoothSS = Tps(oneAPAngle[, c("posX","posY")],
               oneAPAngle$avgSignal)


vizSmooth = predictSurface(smoothSS)

plot.surface(vizSmooth, type = "C")


points(oneAPAngle$posX, oneAPAngle$posY, pch=19, cex = 0.5)




surfaceSS(data = offlineSummary, mac == subMacs[5] & angle == 0)


parCur = par(mfrow = c(2,2), mar = rep(1, 4))

mapply(surfaceSS, mac = subMacs[ rep(c(5, 1), each = 2) ],
       angle = rep(c(0, 135), 2),
       data = list(data = offlineSummary))

par(parCur)

offlineSummary = subset(offlineSummary, mac != subMacs[2])

AP = matrix( c( 7.5, 6.3, 2.5, -.8, 12.8, -2.8,
                1, 14, 33.5, 9.3, 33.5, 2.8),
             ncol = 2, byrow = TRUE,
             dimnames = list(subMacs[ -2 ], c("x", "y") ))

AP

diffs = offlineSummary[ , c("posX", "posY")] -
  AP[ offlineSummary$mac, ]


offlineSummary$dist = sqrt(diffs[ , 1]^2 + diffs[ , 2]^2)
xyplot(signal ~ dist | factor(mac) + factor(angle),
       data = offlineSummary, pch = 19, cex = 0.3,
       xlab ="distance")

macs = unique(offlineSummary$mac)