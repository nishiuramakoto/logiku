module Show
       ( translateWeekJp
       , showTime
       , showPermDirectory
       , showPermFile
       , showGroupPerm
       ) where

import  Import.NoFoundation hiding(FileInfo)
import  Prelude((!!))
import  Data.Time.LocalTime
import  Data.Time.Calendar.WeekDate
import  Text.Printf
import  Constructors


translateWeekJp :: Int -> Char
translateWeekJp n = ("日月火水木金土" ++ (repeat '?')) !! n

showTime :: TimeZone -> UTCTime -> String
showTime tz time = let lctime    = utcToLocalTime tz time
                       lday       = localDay lctime
                       (year,month,doy) = toGregorian lday
                       (_ , _, dow) = toWeekDate lday
                       dowjp = translateWeekJp dow
                       TimeOfDay hour minuit _sec  = localTimeOfDay lctime
                   in
                     printf "%04d/%02d/%02d(%c) %02d:%02d"  year month doy dowjp hour minuit

showPermDirectory :: DirectoryInfo -> String
showPermDirectory info = [or',ow',ox',ar',aw',ax']
  where
    or' = if directoryInfoOwnerR info then 'r' else '-'
    ow' = if directoryInfoOwnerW info then 'w' else '-'
    ox' = if directoryInfoOwnerX info then 'x' else '-'
    ar' = if directoryInfoEveryoneR info then 'r' else '-'
    aw' = if directoryInfoEveryoneW info then 'w' else '-'
    ax' = if directoryInfoEveryoneX info then 'x' else '-'

showPermFile :: FileInfo -> String
showPermFile info = [or',ow',ox',ar',aw',ax']
  where
    or' = if fileInfoOwnerR info then 'r' else '-'
    ow' = if fileInfoOwnerW info then 'w' else '-'
    ox' = if fileInfoOwnerX info then 'x' else '-'
    ar' = if fileInfoEveryoneR info then 'r' else '-'
    aw' = if fileInfoEveryoneW info then 'w' else '-'
    ax' = if fileInfoEveryoneX info then 'x' else '-'


showGroupPerm :: (Text,Perm) -> String
showGroupPerm (grp, Perm r w x) = show grp ++ [gr,gw,gx]
  where
    gr = if r then 'r' else '-'
    gw = if w then 'w' else '-'
    gx = if x then 'x' else '-'
