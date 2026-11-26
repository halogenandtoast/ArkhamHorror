module Arkham.Enemy.Cards.LaidToRest.VengefulShadeSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.CardDefs.Standalone qualified as Enemies
import Arkham.Enemy.Types qualified as Field
import Arkham.Matcher
import Arkham.Placement
import Arkham.Projection
import TestImport.New

-- Laid to Rest (parallel Jim Culver): Vengeful Shade is a spirit, i.e. a card from
-- Jim's side deck, so it never reaches the discard pile -- The Beyond catches the
-- Discarded and returns it to the bottom of the spirit deck. The enemy discard path
-- used to push AddToDiscard unconditionally, which left the Shade in the discard
-- pile *and* the spirit deck at once (#6067).
spec :: Spec
spec = describe "Vengeful Shade" do
  it "returns to the spirit deck instead of the discard pile when discarded"
    $ gameTest
    $ \self -> do
      location <- testLocationWith id
      run $ placedLocation location
      moveTo self location

      -- The Beyond enters play first, so its spirit-deck meta starts empty; the
      -- Shade's own card is then registered as the one spirit-deck card.
      beyond <- putAssetIntoPlay self Assets.theBeyondBleakNetherworld

      shade <- createWeaknessEnemy self Enemies.vengefulShade
      field Field.EnemyCard (toId shade) >>= \case
        PlayerCard pc ->
          updateInvestigator self \attrs -> attrs {investigatorSideDeck = Just [pc]}
        other -> expectationFailure $ "expected a player card, got " <> show other

      run $ PlaceEnemy (toId shade) (AtLocation (toId location))
      run $ Discard Nothing GameSource (toTarget shade)

      assertNone (InPlayEnemy $ enemyIs Enemies.vengefulShade)
      fieldMap InvestigatorDiscard (map toCardId) (toId self) `shouldReturn` []

      -- It really is back in the spirit deck: The Beyond's forced effect attaches
      -- the top card, which can only be the Shade.
      run $ UseCardAbility (toId self) (toSource beyond) 1 [] NoPayment
      assertAny (EnemyAttachedToAsset (AssetWithId beyond) <> enemyIs Enemies.vengefulShade)
