{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TCT.ScenarioRunner where
import Network.HTTP.Simple
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict as Hm
import Data.Text as T
import Filesystem.Path
import System.Path.NameManip as FS (absolute_path)

-- Check out maybeT
runScenario :: IO ()
runScenario = do
  id <- createSession
  case id of
    Nothing -> putStrLn "Failed to retrieve sessionID."
    Just x -> (putStrLn $ "Session ID: " ++ x) >> do
      putStrLn "Initialising"
      initialise x
      simulate x


data CreateSession = CreateSession {sessionId :: String} deriving (Show, Generic)
instance FromJSON CreateSession

-- Create session for the co-simulation
createSession :: IO (Maybe String)
createSession =  do
  (request :: Request) <- parseRequest "http://localhost:8082/createSession"
  (response :: Response (Either JSONException CreateSession)) <- httpJSONEither request
  case getResponseBody response of
    Left ex -> (putStrLn $ "CreateSession Failed: " ++ show ex) >> return Nothing
    Right crSession -> (putStrLn $ "Session ID: " ++ sessionId crSession) >> return (Just $ sessionId crSession)

-- Initialise the co-simulation by sending configuration file
-- Object is type synonym for HashMap Text Value
initialise :: String -> IO ()
initialise sessionId = do
  (mJson :: Maybe Object) <- calcInitJson "resources/initialise_body.json"
  case mJson of
    Nothing -> putStrLn "Failed to calculate and load JSON"
    Just json -> putStrLn "Calculated and Loaded JSON" >>
        let url :: String = "http://localhost:8082/initialize/" ++ sessionId in
          postJSON url json

simulate :: String -> IO ()
simulate sessionId = do
  json <- loadJson "resources/simulate_body.json"
  case json of
    Left err -> putStrLn err 
    Right (Object rawJson) -> do
      putStrLn $ "simulate_body: " ++ show rawJson
      postJSON ("http://localhost:8082/simulate/" ++ sessionId) rawJson

postJSON :: String -> Object -> IO ()
postJSON url body = do
  request' :: Request <- parseRequest url
  let request = setRequestMethod "POST" $ setRequestBodyJSON body $ request'
  response <- httpLBS request
  putStrLn $ "The status code was: " ++ show (getResponseStatusCode response)
  print $ getResponseHeader "Content-Type" response
  putStr $ "Responsebody: "
  print $ getResponseBody response
  putStrLn "\r\n"

calcInitJson :: String -> IO (Maybe Object)
calcInitJson path = do
  (eitherJson :: Either String Value) <- loadJson path
  case eitherJson of
        Left err -> putStrLn err >> return Nothing
        Right (Object rawJson) -> -- json :: Value
          let (eitherFmusJson :: Either String (IO Object)) = updateJsonFmus rawJson in
            case eitherFmusJson of
              Left err -> putStrLn err >> return Nothing
              Right (ioFmusJson :: IO Object)  ->
                ioFmusJson >>=
                (\(fmusJson) ->
                   let (json' :: Object) = Hm.insert "fmus" (Object fmusJson) rawJson in
                     (putStrLn . show) json' >> (return . Just) json')

loadJson :: String -> IO (Either String Value)
loadJson path = B.readFile path >>= (return . eitherDecode )

-- Traverses the FMU object and creates full paths for the values.
updateJsonFmus :: Object -> Either String (IO Object)
updateJsonFmus (json :: Object) =
  case Hm.lookup "fmus" json of
    Nothing -> Left "key: \"fmus\" does not exist."
    Just (Object fmus) -> Right (traverse createFullPath fmus)
      where
        createFullPath :: Value -> IO Value
        createFullPath (String val) =
          (String . T.pack) <$> FS.absolute_path ("resources/" ++ (T.unpack val))
        createFullPath x = pure x -- Ignore non strings
