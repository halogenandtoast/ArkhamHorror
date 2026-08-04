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
    -- "[Forced] - When the act advances: Discard this card (release the token
    -- sealed here)."
    [mkAbility a 1 $ forced $ ActAdvances #when AnyAct | a.placement == NextToAgenda]

instance RunMessage HopeFades where
  runMessage msg s@(HopeFades attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      -- "Put this card into play next to the agenda deck."
      retainCthulhuCard (toCard attrs)

      {- "Search the chaos bag and all in- and out-of-play areas for the [elder_sign]
      token and seal it on this card. (If you cannot, search the chaos bag for the
      numbered token with the highest positive value and seal it on this card
      instead.)"

      'getBagChaosTokens' spans the bag together with everything already sealed
      elsewhere, which is the "in- and out-of-play areas" sweep. The seal itself
      goes through the normal 'sealChaosToken_' path, so the token lands in this
      story's own 'sealedChaosTokens' and renders on the card. -}
      tokens <- getBagChaosTokens
      let numbered = [t | t <- tokens, isJust (chaosTokenFaceValue t.face)]
          byValue = sortOn (Down . chaosTokenFaceValue . (.face)) numbered
      for_ (find ((== ElderSign) . (.face)) tokens <|> listToMaybe byValue)
        $ sealChaosToken_ attrs
      pure $ HopeFades $ attrs & placementL .~ NextToAgenda
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      -- "(release the token sealed here)"
      for_ attrs.sealedChaosTokens unsealChaosToken
      toDiscard (attrs.ability 1) attrs
      pure s
    _ -> HopeFades <$> liftRunMessage msg attrs

{- | Numeric weight of a numbered chaos token face; 'Nothing' for the symbol tokens,
which the fallback search skips.
-}
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
