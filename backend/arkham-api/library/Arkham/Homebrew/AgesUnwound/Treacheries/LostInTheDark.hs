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

instance HasModifiersFor LostInTheDark where
  getModifiersFor (LostInTheDark a) = case a.placement of
    InThreatArea iid ->
      modified_
        a
        iid
        [ AdditionalCostToMove
            $ OrCost
              [ SkillTestCost (toSource a) #willpower (Fixed 3)
              , SkillTestCost (toSource a) #agility (Fixed 3)
              ]
        ]
    _ -> pure mempty

instance HasAbilities LostInTheDark where
  getAbilities (LostInTheDark a) = [mkAbility a 1 $ forced $ RoundEnds #when]

instance RunMessage LostInTheDark where
  runMessage msg t@(LostInTheDark attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> withI18n do
      chooseOneM iid do
        chooseTakeHorror iid attrs 2
        labeled "cancelMove" $ cancelMovement attrs iid
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      toDiscard (attrs.ability 1) attrs
      pure t
    _ -> LostInTheDark <$> liftRunMessage msg attrs
