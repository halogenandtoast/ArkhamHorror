module Arkham.Treachery.Cards.UnhallowedLand (unhallowedLand) where

import Arkham.Card
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype UnhallowedLand = UnhallowedLand TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unhallowedLand :: TreacheryCard UnhallowedLand
unhallowedLand = treachery UnhallowedLand Cards.unhallowedLand

instance RunMessage UnhallowedLand where
  runMessage msg t@(UnhallowedLand attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed 5)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      discardTopOfEncounterDeckAndHandle iid attrs n attrs
      pure t
    DiscardedTopOfEncounterDeck iid cards _ (isTarget attrs -> True) -> do
      when (any (`cardMatch` card_ (#treachery <> #curse)) cards) do
        assets <-
          select
            $ assetControlledBy iid
            <> #ally
            <> AssetWithSanity
            <> AssetCanBeDamagedBySource (toSource attrs)
        chooseOrRunOneAtATimeM iid do
          targeting iid $ directHorror iid attrs 1
          targets assets \asset -> dealAssetDirectHorror asset attrs 1
      pure t
    _ -> UnhallowedLand <$> liftRunMessage msg attrs
