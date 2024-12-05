module Arkham.Treachery.Cards.FogOverInnsmouth (fogOverInnsmouth, FogOverInnsmouth (..)) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype FogOverInnsmouth = FogOverInnsmouth TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fogOverInnsmouth :: TreacheryCard FogOverInnsmouth
fogOverInnsmouth = treachery FogOverInnsmouth Cards.fogOverInnsmouth

instance HasModifiersFor FogOverInnsmouth where
  getModifiersFor (FogOverInnsmouth a) = modifySelect a Anywhere [ShroudModifier 1]

instance HasAbilities FogOverInnsmouth where
  getAbilities (FogOverInnsmouth a) =
    [ limitedAbility (MaxPer Cards.fogOverInnsmouth PerRound 1) $ mkAbility a 1 $ forced $ RoundEnds #when
    ]

instance RunMessage FogOverInnsmouth where
  runMessage msg t@(FogOverInnsmouth attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      chooseOneM iid do
        labeled "Take 1 horror" $ assignHorror iid (attrs.ability 1) 1
        labeled "Put Fog over Innsmouth into play next to the agenda deck."
          $ placeTreachery attrs NextToAgenda
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignHorror iid (attrs.ability 1) 1
      placeTreachery attrs NextToAgenda
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      toDiscard (attrs.ability 1) attrs
      pure t
    _ -> FogOverInnsmouth <$> liftRunMessage msg attrs
