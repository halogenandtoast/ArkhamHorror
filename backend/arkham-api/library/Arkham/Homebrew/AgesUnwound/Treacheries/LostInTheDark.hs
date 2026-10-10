module Arkham.Homebrew.AgesUnwound.Treacheries.LostInTheDark (lostInTheDark) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Treachery.Import.Lifted

newtype LostInTheDark = LostInTheDark TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostInTheDark :: TreacheryCard LostInTheDark
lostInTheDark = treachery LostInTheDark Cards.lostInTheDark

{- | "As an additional cost to move, test [willpower] or [agility] (3)."

The cost rides on the investigator rather than on a location, so it is
'AdditionalCostToEnterMatching' (the only additional movement cost read off the
investigator's own modifiers) with 'Anywhere' as the destination.
-}
instance HasModifiersFor LostInTheDark where
  getModifiersFor (LostInTheDark a) = case a.placement of
    InThreatArea iid ->
      modified_
        a
        iid
        [ AdditionalCostToEnterMatching Anywhere
            $ OrCost
              [ SkillTestCost (toSource a) #willpower (Fixed 3)
              , SkillTestCost (toSource a) #agility (Fixed 3)
              ]
        ]
    _ -> pure mempty

-- | "Forced - At the end the round: Discard Lost in the Dark."
instance HasAbilities LostInTheDark where
  getAbilities (LostInTheDark a) = [mkAbility a 1 $ forced $ RoundEnds #when]

instance RunMessage LostInTheDark where
  runMessage msg t@(LostInTheDark attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    -- "If you fail, either take 2 horror or cancel the effects of the move."
    FailedThisSkillTest iid (isSource attrs -> True) -> withI18n do
      chooseOneM iid do
        chooseTakeHorror iid attrs 2
        labeled "cancelMove" $ cancelMovement attrs iid
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      toDiscard (attrs.ability 1) attrs
      pure t
    _ -> LostInTheDark <$> liftRunMessage msg attrs
