{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Avdou.Run
  ( build
  , clean
  , watch
  ) where

-- Required Imports
import           Prelude (print)
import           RIO
import           System.FSNotify
import qualified RIO.Directory as Dir
import qualified RIO.Text as T
import           Control.Concurrent (forkIO)
import           Network.Wai.Handler.Warp (run)
import           Avdou.Rule
import           WaiAppStatic.Types (toPiece, ssListing, StaticSettings)
import           Network.Wai.Application.Static (staticApp, defaultFileServerSettings, ssIndices)
import           Avdou.Types

build :: Site -> IO ()
build site = do
  forM_ (view copiesL site) (executeCopy site)  
  forM_ (view rulesL site) (executeRule site)
  
clean :: Site -> IO ()
clean site = do
  Dir.removeDirectoryRecursive $ view publicDirL site

startWatcher :: Site -> IO () -> IO ()
startWatcher site buildAction = 
    withManager $ \mgr -> do
        let 
            -- The directory containing source content (e.g., posts, templates)
            sourceDir = view siteDirL site
            
            -- Action to perform on file change (after a debounce period)
            action event = do
                -- It's often good practice to debounce events to avoid multiple builds
                -- from a single file save operation (which can trigger Added/Modified/Closed)
                print $ "Change detected: " ++ show (eventPath event)
                buildAction -- Rebuild the entire site

            -- Predicate to decide which events to act on
            -- We typically only care about file changes, not directory events, 
            -- and often only from specific file types (e.g., .md, .html).
            predicate event = case event of
                -- Only rebuild on modifications/additions/removals of files, not directories
                (Added { eventIsDirectory = IsFile })    -> True
                (Modified { eventIsDirectory = IsFile }) -> True
                (Removed { eventIsDirectory = IsFile })   -> True
                _ -> False
                
        print $ "Watching " ++ sourceDir ++ " for changes..."
        
        -- watchTree watches the directory and all its subdirectories
        _ <- watchTree mgr 
                     sourceDir 
                     predicate 
                     action
                     
        -- Keep the main watcher thread alive indefinitely
        forever $ threadDelay 1000000 -- Sleep for 1 second

watch :: Site -> Int -> IO ()
watch site port = do
    let 
        dir  = view publicDirL site -- dir is FilePath
        buildAction = build site 

        -- Define the settings VALUE by applying 'dir' to the function:
        serverSettingsValue :: StaticSettings
        serverSettingsValue = (defaultFileServerSettings dir) 
            { ssIndices = [fromMaybe (error "Wai failure to convert Text to Piece") (toPiece $ T.pack "index.html")]
            , ssListing = Nothing 
            }

    -- Initial Build
    print "Starting initial site build..."
    buildAction

    -- Start the watcher in a new thread 
    _ <- forkIO (startWatcher site buildAction) 

    -- Start the Web Server
    print $ "Serving " ++ dir ++ " at http://localhost:" ++ show port
    -- Use the configured VALUE
    run port (staticApp serverSettingsValue)

{-
watch :: Site -> IO ()
watch site = do
  let port = 8000
      dir  = view publicDirL site
  print $ "Serving " ++ dir ++ " at http://localhost:" ++ show port
  run port (staticApp (defaultWebAppSettings dir))
-}
