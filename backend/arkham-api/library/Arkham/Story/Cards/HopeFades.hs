module Arkham.Story.Cards.HopeFades (hopeFades) where

import Arkham.Ability
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Helpers.ChaosBag (getBagChaosTokens)
import Arkham.Matcher
import Arkham.Placement
import Arkham.Scenarios.TheDoomOfArkhamPartII.Helpers
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype HopeFades = HopeFades StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hopeFades :: StoryCard HopeFades
hopeFades = persistStory $ story HopeFades Cards.hopeFades

instance HasAbilities HopeFades where
  getAbilities (HopeFades a) =
    [mkAbility a 1 $ forced $ ActAdvances #when AnyAct | a.placement == NextToAgenda]

instance RunMessage HopeFades where
  runMessage msg s@(HopeFades attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      retainCthulhuCard (toCard attrs)
      tokens <- getBagChaosTokens
      let numbered = [t | t <- tokens, isJust (chaosTokenFaceValue t.face)]
          byValue = sortOn (Down . chaosTokenFaceValue . (.face)) numbered
      for_ (find ((== ElderSign) . (.face)) tokens <|> listToMaybe byValue)
        $ sealChaosToken_ attrs
      pure $ HopeFades $ attrs & placementL .~ NextToAgenda
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      for_ attrs.sealedChaosTokens unsealChaosToken
      discardCthulhuCard attrs
      pure s
    _ -> HopeFades <$> liftRunMessage msg attrs

chaosTokenFaceValue :: ChaosTokenFace -> Maybe Int
chaosTokenFaceValue = \case
  PlusOne -> Just 1
  Zero -> Just 0
  MinusOne -> Just (-1)
  MinusTwo -> Just (-2)
  MinusThree -> Just (-3)
  MinusFour -> Just (-4)
  MinusFive -> Just (-5)
  MinusSix -> Just (-6)
  MinusSeven -> Just (-7)
  MinusEight -> Just (-8)
  _ -> Nothing
