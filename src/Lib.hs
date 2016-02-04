{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( makePlaylist
    , second
    , segment
    , extractChartEntries
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
import qualified Data.Text as T
import Debug.Trace
import Control.Monad (sequence)
import Data.List (intersperse)
import Data.Maybe (catMaybes)

type ChartEntry = (T.Text, T.Text)

segment :: Int -> [a] -> [[a]]
segment _ [] = []
segment n l = (take n l) : (segment n (drop n l))

second :: [a] -> a
second = head . drop 1

extractChartEntries :: [Tag T.Text] -> [ChartEntry]
extractChartEntries tags = map extractChartEntry (segment 2 tags)
  where extractChartEntry e = (fromTagText (head e), fromTagText (last e))

head' :: [a] -> Maybe a
head' []     = Nothing
head' (x:xs) = Just x

makePlaylist :: IO ()
makePlaylist = do chartEntries <- getChartEntries
                  _ <- putStrLn $ "The chartEntries are " ++ (show chartEntries)
                  maybeAlbumIds <- sequence $ map getAlbumId chartEntries
                  maybeTrackIds <- sequence $ map getAlbumTracks maybeAlbumIds
                  putStrLn $ "The playlist is " ++ (show (constructPlaylistLink (concat (catMaybes maybeTrackIds))))

parseChartSection :: [Tag T.Text] -> [Tag T.Text]
parseChartSection =
    (takeWhile (~/= ("<td valign=top align=center>" :: String))) . (dropWhile (~/= ("<tr class=ListRow1>" :: String)))

parseChartEntries :: [Tag T.Text] -> [ChartEntry]
parseChartEntries tags =
    let chartSection = parseChartSection tags
        filterNonsense = filter ((/= "^") . fromTagText)
        chartTextTags = filterNonsense (map second (sections (~== ("<b>" :: String)) chartSection))
     in extractChartEntries chartTextTags

infixl 4 >$<
(>$<) :: Functor f => f a -> (a -> b) -> f b
(>$<) = flip (<$>)

getChartEntries :: IO [ChartEntry]
getChartEntries = get "http://americanaradio.org/ama/displaychart_beforetracks.asp" >$<
  (take 20) . parseChartEntries . parseTags . T.pack . unpack . (^. responseBody)


-- _ <- putStrLn $ "Got response for " ++ (show chartEntry) ++ (show $ (head $ spotifySearchResponse ^.. responseBody . key "albums") ^.. key "href")

idOfFirstResult :: Response Data.ByteString.Lazy.Internal.ByteString -> Maybe T.Text
idOfFirstResult r =  head' $ r ^.. responseBody . key "albums" . key "items" . values . key "id" . _String

getAlbumId :: ChartEntry -> IO (Maybe T.Text)
getAlbumId chartEntry = do spotifySearchResponse <- getWith opts "https://api.spotify.com/v1/search"
                           let maybeAlbumId = idOfFirstResult spotifySearchResponse
                            in return maybeAlbumId
                              where artist = fst chartEntry
                                    album = snd chartEntry
                                    opts = defaults & param "q" .~ [(T.append (T.append artist " ") album)] & param "type" .~ ["album"]

albumTrackIds :: Response Data.ByteString.Lazy.Internal.ByteString -> [T.Text]
albumTrackIds r =  r ^.. responseBody . key "items" . values . key "id" .  _String

getAlbumTracks :: Maybe T.Text -> IO (Maybe [T.Text])
getAlbumTracks Nothing = return Nothing
getAlbumTracks (Just albumId) = do albumTracksResponse <- get url
                                   let trackIds = take 2 $ albumTrackIds albumTracksResponse
                                    in return (Just trackIds)
                                      where url =  "https://api.spotify.com/v1/albums/"++ (T.unpack albumId) ++ "/tracks"

constructPlaylistLink :: [T.Text] -> T.Text
constructPlaylistLink trackIds = (T.append "spotify:trackset:Playlist:" (T.intercalate "," trackIds))
