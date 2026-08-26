module Arkham.Asset.Assets.DecoratedSkullSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Classes.HasGame (getGame)
import Arkham.Investigator.Cards (rolandBanks)
import Arkham.Matcher
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import TestImport.New

spec :: Spec
spec = describe "Decorated Skull" $ do
  hasUses @"charge" Assets.decoratedSkull 0

  it "places a charge after an Ally asset at your location is defeated" . gameTest $ \self -> do
    location <- testLocation
    run $ placedLocation location
    self `moveTo` location
    decoratedSkull <- self `putAssetIntoPlay` Assets.decoratedSkull
    beatCop <- self `putAssetIntoPlay` Assets.beatCop
    run $ DealAssetDamage beatCop (TestSource mempty) 3 0
    useReactionOf decoratedSkull
    decoratedSkull.charges `shouldReturn` 1

  it "resolves after the defeated ally has actually left play" . gameTest $ \self -> do
    location <- testLocation
    run $ placedLocation location
    self `moveTo` location
    decoratedSkull <- self `putAssetIntoPlay` Assets.decoratedSkull
    beatCop <- self `putAssetIntoPlay` Assets.beatCop
    run $ DealAssetDamage beatCop (TestSource mempty) 3 0
    useReactionOf decoratedSkull
    -- the reaction still matched it, but the ally is genuinely gone
    assert $ selectNone $ assetIs Assets.beatCop
    fmap (map toCardCode) self.discard `shouldReturn` ["01018"]

  it "does not trigger for an Ally at a different location" . gameTest $ \self -> do
    here <- testLocation
    there <- testLocation
    run $ placedLocation here
    run $ placedLocation there
    self `moveTo` here
    decoratedSkull <- self `putAssetIntoPlay` Assets.decoratedSkull
    other <- addInvestigator rolandBanks
    other `moveTo` there
    beatCop <- other `putAssetIntoPlay` Assets.beatCop
    run $ DealAssetDamage beatCop (TestSource mempty) 3 0
    decoratedSkull.charges `shouldReturn` 0

  -- The frontend does `game.assets[id]` for every id in `investigator.assets`, so
  -- an id published there that is missing from the assets map is a crash, not a
  -- cosmetic issue. Reviving a defeated asset for the reaction's benefit must not
  -- leak into the wire encoding. #5518
  it "publishes no asset id the frontend cannot resolve while the reaction is pending" . gameTest $ \self -> do
    location <- testLocation
    run $ placedLocation location
    self `moveTo` location
    decoratedSkull <- self `putAssetIntoPlay` Assets.decoratedSkull
    beatCop <- self `putAssetIntoPlay` Assets.beatCop
    run $ DealAssetDamage beatCop (TestSource mempty) 3 0
    game <- getGame
    let
      obj k (Object o) = KeyMap.lookup (Key.fromText k) o
      obj _ _ = Nothing
      publishedAssetIds = case toJSON (PublicGame (1 :: Int) "test" [] game) of
        published -> case obj "assets" published of
          Just (Object as) -> map Key.toText (KeyMap.keys as)
          _ -> []
      referencedAssetIds = case toJSON (PublicGame (1 :: Int) "test" [] game) of
        published -> case obj "investigators" published of
          Just (Object invs) ->
            [ aid
            | i <- KeyMap.elems invs
            , Array xs <- maybeToList (obj "assets" i)
            , String aid <- toList xs
            ]
          _ -> []
    unless (notNull referencedAssetIds)
      $ expectationFailure "vacuous: no assets were published at all"
    for_ referencedAssetIds \aid ->
      unless (aid `elem` publishedAssetIds)
        $ expectationFailure ("published unresolvable asset id: " <> show aid)
    -- Proves the sample above was taken with the reaction window still open --
    -- otherwise nothing would have been revived and the check would be vacuous.
    useReactionOf decoratedSkull
    decoratedSkull.charges `shouldReturn` 1
