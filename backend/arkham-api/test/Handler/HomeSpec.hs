{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.HomeSpec (
  spec,
) where

import TestImport

spec :: Spec
spec = withApp $ do
  describe "Homepage" $ do
    it "loads the index and checks it looks right" $ do
      get HomeR
      statusIs 200
      htmlAnyContain "h1" "a modern framework for blazing fast websites"

      request $ do
        setMethod "POST"
        setUrl HomeR
        addToken
        fileByLabelExact "Choose a file" "test/Spec.hs" "text/plain" -- talk about self-reference
        byLabelExact "What's on the file?" "Some Content"

      statusIs 200
      -- more debugging printBody
      htmlAllContain ".upload-response" "text/plain"
      htmlAllContain ".upload-response" "Some Content"

    -- This is a simple example of using a database access in a test.  The
    -- test will succeed for a fresh scaffolded site with an empty database,
    -- but will fail on an existing database with a non-empty user table.
    it "leaves the user table empty" $ do
      get HomeR
      statusIs 200
      users <- runDB $ select ([] :: [Filter User]) []
      assertEq "user table empty" 0 $ length users
