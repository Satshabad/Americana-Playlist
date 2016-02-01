{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( writeChart
    , second
    , segment
    , extractChartEntrys
    , topChartEntry
    , getAlbumId
    ) where

import Network.Wreq
import Control.Lens
import Data.ByteString.Lazy.Char8 (unpack)
import Text.HTML.TagSoup
import GHC.Generics
import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString.Lazy.Internal (ByteString)

type ChartEntry = ([Char], [Char])

segment :: Int -> [a] -> [[a]]
segment _ [] = []
segment n l = (take n l) : (segment n (drop n l))

second :: [a] -> a
second = head . drop 1 . take 2

extractChartEntrys :: [Tag [Char]] -> [ChartEntry]
extractChartEntrys tags = map extractChartEntry (segment 2 tags)
  where extractChartEntry = (\e-> (fromTagText (head e), fromTagText (last e)))

idOfFirstResult :: Response Data.ByteString.Lazy.Internal.ByteString -> [Char]
idOfFirstResult r = head (r ^.. responseBody . key "albums" . key "items" . values . key "id" . _String)

writeChart :: IO ()
writeChart = do topChartEntry <- getTopChartEntry
                albumId <- getAlbumId topChartEntry
                putStrLn $ "The album id is" ++ (show albumId)

getTopChartEntry :: IO ChartEntry
getTopChartEntry = do r <- get "http://americanaradio.org/ama/displaychart_beforetracks.asp"
                      let src = (unpack (r ^. responseBody))
                          tags = parseTags src
                          chartSection = takeWhile (~/= "<td valign=top align=center>") (dropWhile (~/= "<tr class=ListRow1>") tags)
                          chartTextTags = filterNonsense (map second (sections (~== "<b>") chartSection))
                          chartEntrys = extractChartEntrys chartTextTags
                       in return (head chartEntrys)
                         where filterNonsense = filter ((/= "^") . fromTagText)

getAlbumId :: ChartEntry -> IO [Char]
getAlbumId chartEntry = do spotifySearchResponse <- getWith opts "https://api.spotify.com/v1/search"
                           let albumId = idOfFirstResult spotifySearchResponse
                            in return albumId
                              where artist = fst chartEntry
                                    album = snd chartEntry
                                    opts = defaults & param "q" .~ [artist ++ album] & param "type" .~ ["album"]
