module Arkham.Treachery.Cards.PoisonousSpores (poisonousSpores) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Helpers.Location
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Treachery.Import.Lifted

newtype PoisonousSpores = PoisonousSpores TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

poisonousSpores :: TreacheryCard PoisonousSpores
poisonousSpores = treachery PoisonousSpores Cards.poisonousSpores

instance HasAbilities PoisonousSpores where
  getAbilities (PoisonousSpores a) = [mkAbility a 1 $ forced $ RoundEnds #when]

instance RunMessage PoisonousSpores where
  runMessage msg t@(PoisonousSpores attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      withLocationOf iid $ attachTreachery attrs
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      case attrs.placement.attachedTo of
        Just (LocationTarget lid) -> do
          selectEach
            (investigatorAt lid <> HasMatchingTreachery (treacheryIs Treacheries.poisoned))
            \iid -> assignHorror iid attrs 2

          selectEach
            (investigatorAt lid <> not_ (HasMatchingTreachery $ treacheryIs Treacheries.poisoned))
            \iid -> do
              poisoned <- getSetAsidePoisoned
              createWeaknessInThreatArea poisoned iid
          toDiscard (attrs.ability 1) attrs
          pure t
        _ -> error "invalid attachment of treachery, expected location"
    _ -> PoisonousSpores <$> liftRunMessage msg attrs
