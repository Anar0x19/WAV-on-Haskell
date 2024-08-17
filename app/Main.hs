{-# LANGUAGE OverloadedStrings #-}

import Data.Word (Word16)
import Data.Bits ((.&.))
import Data.ByteString.Lazy (ByteString, readFile, writeFile)
import Data.Binary.Get (runGet, getWord16le, getWord32le, getLazyByteString)
import Data.Binary.Put (runPut, putWord16le, putWord32le, putLazyByteString)
import Data.List (isPrefixOf, find)
import Data.Maybe (fromJust)

-- | Read WAV header
data WAVHeader = WAVHeader
    { chunkID :: ByteString
    , chunkSize :: Word32
    , format :: ByteString
    , subchunk1ID :: ByteString
    , subchunk1Size :: Word32
    , audioFormat :: Word16
    , numChannels :: Word16
    , sampleRate :: Word32
    , byteRate :: Word32
    , blockAlign :: Word16
    , bitsPerSample :: Word16
    , subchunk2ID :: ByteString
    , subchunk2Size :: Word32
    } deriving Show

getWAVHeader :: ByteString -> WAVHeader
getWAVHeader = runGet $ do
    chunkID <- getLazyByteString 4
    chunkSize <- getWord32le
    format <- getLazyByteString 4
    subchunk1ID <- getLazyByteString 4
    subchunk1Size <- getWord32le
    audioFormat <- getWord16le
    numChannels <- getWord16le
    sampleRate <- getWord32le
    byteRate <- getWord32le
    blockAlign <- getWord16le
    bitsPerSample <- getWord16le
    subchunk2ID <- getLazyByteString 4
    subchunk2Size <- getWord32le
    return WAVHeader{..}

-- | Extract audio data
extractAudioData :: WAVHeader -> ByteString -> ByteString
extractAudioData header bs = drop 44 bs

-- | Save WAV file
saveWAVFile :: FilePath -> WAVHeader -> ByteString -> IO ()
saveWAVFile path header audioData = writeFile path $ runPut $ do
    putLazyByteString $ chunkID header
    putWord32le $ chunkSize header
    putLazyByteString $ format header
    putLazyByteString $ subchunk1ID header
    putWord32le $ subchunk1Size header
    putWord16le $ audioFormat header
    putWord16le $ numChannels header
    putWord32le $ sampleRate header
    putWord32le $ byteRate header
    putWord16le $ blockAlign header
    putWord16le $ bitsPerSample header
    putLazyByteString $ subchunk2ID header
    putWord32le $ subchunk2Size header
    putLazyByteString audioData

-- | Find repeating segments
findRepeatingSegment :: ByteString -> (Int, Int, Int)
findRepeatingSegment audioData = (start1, start2, segmentSize)
  where
    audioList = chunkify 2 $ map fromIntegral $ toList audioData
    len = length audioList
    (start1, start2, segmentSize) = foldl findBest (0, 0, 0) [ (i, j) | i <- [0..len `div` 2], j <- [i + len `div` 4 .. len `div` 2] ]

    findBest (bestStart1, bestStart2, bestSize) (i, j) =
        let segmentSize = min (j - i) (len - i - (j - i))
            error = calculateError (take segmentSize $ drop i audioList) (take segmentSize $ drop j audioList)
        in if error < currentError
           then (i, j, segmentSize)
           else (bestStart1, bestStart2, bestSize)

    calculateError segment1 segment2 = sum [ abs (x - y) | (x, y) <- zip segment1 segment2 ]

    chunkify _ [] = []
    chunkify n xs = take n xs : chunkify n (drop n xs)

    toList = map fromIntegral . map fromEnum . B.unpack
    fromList = map (fromIntegral . fromEnum) . B.pack

main :: IO ()
main = do
    let inputFile = "Chopin.wav"
        outputFile = "NewChopin.wav"
    wavData <- readFile inputFile
    let header = getWAVHeader wavData
        audioData = extractAudioData header wavData
        (start1, start2, segmentSize) = findRepeatingSegment audioData
        newAudioData = B.concat [B.take start1 audioData, B.drop (start2 + segmentSize) audioData]
        newHeader = header { subchunk2Size = fromIntegral (B.length newAudioData) }
    saveWAVFile outputFile newHeader newAudioData
    putStrLn $ "Processing complete. Output file: " ++ outputFile

