module Arkham.Campaign.TheScarletKeys.ConcealedSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheScarletKeys.Concealed (mkConcealedCard)
import Arkham.Campaigns.TheScarletKeys.Concealed.Kind
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Enemy)
import Arkham.Matcher
import Arkham.Placement
import Arkham.Token (Token (Charge))
import TestImport.New

{- | Put Coterie Agent (A) in the shadows with its mini-card at @location@, the way resolving its
@concealed 2@ keyword would.
-}
concealAgentAt :: Investigator -> Location -> TestAppT Enemy
concealAgentAt self location = do
  agent <- testEnemyWithDef Enemies.coterieAgentA id
  run $ PlaceEnemy (toId agent) InTheShadows
  card <- mkConcealedCard CoterieAgentA
  run $ CreateConcealedCard card
  run $ PlaceConcealedCard (toId self) card.id (AtLocation $ toId location)
  pure agent

{- | Walk the prompts that follow choosing to expose: pick the mini-card, confirm the flip, then
decline Coterie Agent (A)'s own "when exposed" free reaction (discard itself), which pauses the
queue before the enemy moves out of the shadows.
-}
exposeConcealedCard :: HasCallStack => TestAppT ()
exposeConcealedCard = do
  clickLabel "$label.exposeConcealedCard"
  click "choose concealed card"
  click "flip concealed card"
  skip

spec :: Spec
spec = describe "Concealed mini-cards" do
  -- #5387: exposure by investigating used to hang off the clue-discovery pipeline, so an
  -- investigation that discovered nothing could never expose.
  context "exposing by investigating" do
    it "is offered when the location has no clues to discover" . gameTest $ \self -> do
      withProp @"intellect" 3 self
      setChaosTokens [Zero]
      location <- testLocation & prop @"clues" 0 & prop @"shroud" 0
      self `moveTo` location
      _ <- concealAgentAt self location

      self `investigate` location
      startSkillTest
      applyResults
      exposeConcealedCard

      assertNone $ EnemyWithPlacement InTheShadows
      assertNone ConcealedCardAny

    it "is offered when an empty Divination discovers no clues" . gameTest $ \self -> do
      withProp @"intellect" 3 self
      setChaosTokens [Zero]
      location <- testLocation & prop @"clues" 3 & prop @"shroud" 0
      self `moveTo` location
      _ <- concealAgentAt self location

      divination <- self `putAssetIntoPlay` Assets.divination1
      run $ SpendUses (toSource self) (toTarget divination) Charge 4

      [doInvestigate] <- self `getActionsFrom` divination
      self `useAbility` doInvestigate
      clickLabel "$label.cards.divination1.useIntellect"
      startSkillTest
      applyResults
      exposeConcealedCard

      assertNone $ EnemyWithPlacement InTheShadows
      assertNone ConcealedCardAny
      -- exposing replaces the standard effects of the ability that exposed it
      location.clues `shouldReturn` 3
      self.clues `shouldReturn` 0

    it "does not prompt twice when the investigation also discovers a clue" . gameTest $ \self -> do
      withProp @"intellect" 3 self
      setChaosTokens [Zero]
      location <- testLocation & prop @"clues" 1 & prop @"shroud" 0
      self `moveTo` location
      _ <- concealAgentAt self location

      self `investigate` location
      startSkillTest
      applyResults
      clickLabel "$label.doNotExposeConcealed"

      location.clues `shouldReturn` 0
      self.clues `shouldReturn` 1
      assertAny ConcealedCardAny
