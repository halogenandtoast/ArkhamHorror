module Arkham.Treachery.Cards.ElectrostaticDischarge (electrostaticDischarge) where

import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ElectrostaticDischarge = ElectrostaticDischarge TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

electrostaticDischarge :: TreacheryCard ElectrostaticDischarge
electrostaticDischarge = treachery ElectrostaticDischarge Cards.electrostaticDischarge

instance RunMessage ElectrostaticDischarge where
  runMessage msg t@(ElectrostaticDischarge attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      locations <-
        select
          $ oneOf
            [ LocationWithAnySeal <> LocationWithInvestigator Anyone
            , LocationWithInvestigator InvestigatorWithAnySeal
            ]

      investigators <- select $ InvestigatorAt $ mapOneOf LocationWithId locations

      chooseOneAtATimeM iid do
        targets investigators \iid' -> do
          active <-
            iid'
              <=~> InvestigatorAt
                (oneOf [LocationWithAnyActiveSeal, LocationWithInvestigator InvestigatorWithAnyActiveSeal])
          if active
            then assignDamageAndHorror iid' attrs 1 1
            else assignDamage iid' attrs 1
      pure t
    _ -> ElectrostaticDischarge <$> liftRunMessage msg attrs
