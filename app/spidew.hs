{-# LANGUAGE OverloadedStrings #-}



module Spidew where



import GHC.Base (Any)
import System.Console.Haskeline ( getInputLine, InputT )
import Data.Aeson ( ToJSON, FromJSON (parseJSON), withObject, (.:), encode )
import GHC.Generics ( Generic )
import Data.Text (Text, pack)
import qualified Data.Text.IO as TIO (putStrLn)
import Control.Monad.Trans (lift)
import Control.Monad
import Network.HTTP.Simple
import Data.Aeson.Types (Value, Parser)
import qualified Data.ByteString as B
import Text.Parsec
import Control.Monad.State
import Control.Monad.State.Class
import Control.Monad.State



data Command = Inspect {symbol :: String}
             -- | Create {watchlist :: Text}
             | Key {key :: String}
             | Add {watchlist :: Text, symbol :: String}
             | RmEntry {watchlist :: Text, symbol :: String}
             -- | RmLs {watchlist :: Text}
             | InspectLs {watchlist :: Text}
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

takeInputLoop :: StateT String (InputT IO) ()
parseInput ::  String -> Maybe Command
executeInput :: Maybe Command -> StateT String (InputT IO) ()

inspectQuote :: String -> String ->  IO ()
getQuote :: String -> String -> IO GetInfo
processQuote :: GetInfo -> String

createLs :: String -> IO ()
addEntry, rmEntry, rmLs :: IO ()




-- | Helper function used to handle exceptions built into custom datatypes


toMaybe :: Eq a => a -> a -> Maybe a
toMaybe k l = if k == l then Nothing else pure l

-- | List of built-in texts, strings


-- | helpText is the text called by the help command

helpText :: Text
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
    
    
    takeInputLoop = lift (getInputLine "SPiDeW>") >>= executeInput . (>>= parseInput)


    -- | Splits input string based on the location of spaces, and if the format matches, converts
    -- it to a Command

    parseInput k = 
        toMaybe CommandNull $ case words k of
            ("inspect":x:[]) -> Inspect x
            --("makelist":x:[]) -> Create $ pack x
            ("add":x:y:[]) -> Add (pack x) $ y
            ("rmentry":x:y:[]) -> RmEntry (pack x) $ y
            --("rmlist":x:[]) -> RmLs $ pack x
            ("inspectlist":x:[]) -> InspectLs $ pack x
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
            Add x y -> pure ()
            RmEntry x y -> pure ()
            --RmLs x -> pure ()
            Inspect x -> do apiKey <- get; liftIO (inspectQuote x apiKey)
            InspectLs x -> pure ()
            Key x -> put x
            Help -> liftIO (TIO.putStrLn helpText)
            ) >> takeInputLoop


            

-- | Functions that check the webAPI for the latest stock information.

(inspectQuote, getQuote, processQuote) = (inspectQuote, getQuote,
    processQuote)
    where
        -- | helper for processQuote
    calculateChange chng base = (base/(base - chng)-1)*100

-- | inspectQuote calls getQuote, runs processQuote on it, then prints the resulting string.

    inspectQuote u apiKey = processQuote <$> getQuote u apiKey >>= putStrLn

-- | getQuote, given a symbol, returns an IO Value wrapping the entry in the online
-- database for processQuote to convert.  

    getQuote u apiKey = (getResponseBody <$>) $ httpJSON =<< parseRequest 
        ( "GET " <> apiLocation <> u <> keyQuery <> apiKey)

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
        <> (take 5 $ show $ calculateChange change price)
        <> "%\n"
    

-- | Functions that create, modify, and delete an on-disk JSON database.
(createLs, addEntry, rmEntry, rmLs) = (createLs, addEntry, rmEntry, rmLs)
    where
    createLs u = undefined -- B.writeFile "\\watchlists\\watchlist.json" =<< (B.toStrict.encode <$> (getQuote u))
    addEntry = undefined
    rmEntry = undefined
    rmLs = undefined


