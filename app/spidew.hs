{-# LANGUAGE OverloadedStrings #-}



module Spidew where

import System.Console.Haskeline ( getInputLine, InputT )
import Data.Aeson ( ToJSON, FromJSON (parseJSON), withObject, (.:), encode, decode )
import GHC.Generics ( Generic )
import Control.Monad.Trans (lift)
import Network.HTTP.Simple
    ( parseRequest, getResponseBody, httpJSON )
import Data.Aeson.Types (Value, Parser)
import qualified Data.ByteString.Lazy as B
import Control.Monad.State
    ( MonadIO(liftIO), MonadTrans(lift), MonadState(get, put), StateT )
import Data.Maybe (fromMaybe)
import qualified Data.String as B
import Data.List (delete)
import System.Directory (doesFileExist)

-- | Function types.

takeInputLoop :: SPiDeW ()
parseInput ::  String -> Maybe Command
executeInput :: Maybe Command -> SPiDeW ()

inspectQuote :: String -> SPiDeW ()
getQuote :: String -> SPiDeW GetInfo
processQuote :: GetInfo -> String

createLs :: String -> IO ()
addEntry, rmEntry :: String -> IO ()
inspectL :: SPiDeW ()

-- | Type definitions


type SPiDeW = StateT String (InputT IO)

data Command = Inspect {symbol :: String}
             -- | Create {watchlist :: Text}
             | Key {key :: String}
             | Add {symbol :: String}
             | RmEntry {symbol :: String}
             -- | RmLs {watchlist :: Text}
             | InspectLs
             -- | ShowLs
             | Help
             | Exit
             | CommandNull -- use only to convert to Nothing with toMaybe
             deriving Eq

-- | GetInfo type used to read from webAPI
--  Usable only with VantageAlpha's JSON.

data GetInfo = GetInfo
        {
            getSymbol :: String
        ,   getPrice :: String
        ,   getChange :: String
        }
        deriving (Generic, Show)

instance FromJSON GetInfo where
    parseJSON = withObject "GetInfo" (\outerObject -> (outerObject .: "Global Quote") >>= \obj -> 
        ((GetInfo) <$> (obj .: "01. symbol") <*> (obj .: "05. price") <*> (obj .: "09. change")))

newtype Watchlist = Watchlist {watchlist :: [String]} deriving (Show, Generic)

instance FromJSON Watchlist
instance ToJSON Watchlist


-- | Helper function used to handle exceptions built into custom datatypes


toMaybe :: Eq a => a -> a -> Maybe a
toMaybe k l = if k == l then Nothing else pure l

-- | List of built-in texts, strings


-- | helpText is the text called by the help command

helpText :: String
helpText =
    ("Welcome to SPiDeW! This application checks current stock prices and manages\n\
    \watchlists. The following is a list of commands:\n\n\
    \\tinspect symbol- checks AlphaVantage for the present information on the symbol\n\
    \--\tmakelist listname- adds a watchlist to the database\n\
    \\tadd listname symbol- adds a given symbol to the given watchlist\n\
    \\trmentry listname symbol- removes the given symbol from the given watchlist\n\
    \--\trmlist listname- removes the given watchlist\n\
    \--\tinspectlist listname- checks AlphaVantage for the present information on\n\
    \--\tall symbols in the given watchlist\n\
    \--\tshowlists- shows all watchlists in the given registry\n\
    \\texit- exits SPiDeW\n\
    \\thelp- displays this text")

-- | API location

apiLocation :: String
apiLocation = "https://www.alphavantage.co/query?function=GLOBAL_QUOTE&symbol="



-- | API key query URL

keyQuery :: String
keyQuery = "&apikey="


-- | Main loop, getInputLine from Haskeline is fed to parseInput, after which executeInput
-- runs it and then either calls takeInputLoop again or returns (), ending the program.
(takeInputLoop, parseInput, executeInput) = (takeInputLoop, parseInput, executeInput)
    where
    
    
    takeInputLoop = lift (getInputLine "SPiDeW> ") >>= executeInput . (>>= parseInput)


    -- | Splits input string based on the location of spaces, and if the format matches, converts
    -- it to a Command

    parseInput k = 
        toMaybe CommandNull $ case words k of
            ("inspect":x:[]) -> Inspect x
            --("makelist":x:[]) -> Create $ pack x
            ("add":x:[]) -> Add x
            ("rmentry":x:[]) -> RmEntry x
            --("rmlist":x:[]) -> RmLs $ pack x
            ["inspectlist"] -> InspectLs
            --["showlists"] -> ShowLs
            ("key":x:[]) -> Key x
            ["exit"] -> Exit
            ["help"] -> Help
            _ -> CommandNull

    -- | on Exit, gives pure (), closing the loop. Otherwise, executes some action based on the
    -- given command, then calls takeInputLoop. Technically a case of mutual recursion!

    executeInput Nothing = liftIO (putStrLn "Unrecognized Command") >> takeInputLoop
    executeInput (Just u) = case u of
        Exit -> pure ()
        _ -> (case u of
            --ShowLs -> pure ()
            --Create x -> pure ()
            Add x -> lift.lift $ addEntry x
            RmEntry x -> lift.lift $ rmEntry x
            --RmLs x -> pure ()
            Inspect x -> do apiKey <- get; (inspectQuote x)
            InspectLs -> inspectL
            Key x -> put x >> liftIO (putStrLn $ "Key " <> x <> " is in use.")
            Help -> liftIO (putStrLn helpText)
            ) >> takeInputLoop


            

-- | Functions that check the webAPI for the latest stock information.

(inspectQuote, getQuote, processQuote) = (inspectQuote, getQuote,
    processQuote)
    where
        -- | helpers for processQuote
    calculateChange chng base = (base/(base - chng)-1)*100
    addPlus u = if u >= 0 then "+" <> show u else show u

-- | inspectQuote calls getQuote, runs processQuote on it, then prints the resulting string.

    inspectQuote u = processQuote <$> getQuote u >>= liftIO.putStrLn

-- | getQuote, given a symbol, returns an IO Value wrapping the entry in the online
-- database for processQuote to convert.  

    getQuote u = do
        apiKey <- get
        lift ((getResponseBody <$>) $ httpJSON =<< parseRequest ("GET " <> apiLocation <> u <> keyQuery <> apiKey))

-- | processQuote creates a String from an Entry

    processQuote u = let (name, price, change) = (getSymbol u, read $ getPrice u,
                            read $ getChange u) in
        "\nTicker Symbol: "
        <> name
        <> "\nCurrent Price: "
        <> show price
        <> "\nChange: "
        <> show change
        <> "\nPercent Change: "
        <> (take 5 $ addPlus $ calculateChange change price)
        <> "%\n"
    

-- | Functions that create, modify, and delete an on-disk JSON database.

(createLs, addEntry, rmEntry, inspectL) = (createLs, addEntry, rmEntry, inspectL)
    where
    checkExistence = doesFileExist "watchlist.json"
    -- | General functions for a disk-based watchlist.
    addEntry u = do
        exists <- checkExistence
        if exists == True then
            do
            k <- (decode <$> B.readFile "watchlist.json") :: IO (Maybe Watchlist)
            if elem (B.fromString u) $ watchlist (fromMaybe (Watchlist []) k)
                then putStrLn $ u <> " is already in the watchlist."
                else B.writeFile "watchlist.json" $ encode $ Watchlist $ watchlist (fromMaybe (Watchlist []) k) <> [u]
        else do
            writeFile "watchlist.json" ""
            k <- (decode <$> B.readFile "watchlist.json") :: IO (Maybe Watchlist)
            B.writeFile "watchlist.json" $ encode $ Watchlist $ watchlist (fromMaybe (Watchlist []) k) <> [u]
    rmEntry u = do
        k <- (decode <$> B.readFile "watchlist.json") :: IO (Maybe Watchlist)
        if elem (B.fromString u) $ watchlist (fromMaybe (Watchlist []) k)
            then B.writeFile "store.json" $ encode $ Watchlist $ delete u $ watchlist
            $ fromMaybe (Watchlist []) k
            else putStrLn $ u <> " is not in the watchlist."
    inspectL = do
        k <- (lift.lift $ decode <$> B.readFile "watchlist.json") :: SPiDeW (Maybe Watchlist)
        sequence $ fmap inspectQuote <$> watchlist $ fromMaybe (Watchlist []) k
        pure ()
        


