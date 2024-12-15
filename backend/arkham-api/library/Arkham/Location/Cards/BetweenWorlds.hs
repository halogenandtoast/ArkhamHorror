module Arkham.Location.Cards.BetweenWorlds (betweenWorlds, BetweenWorlds (..)) where

import Arkham.Ability
import Arkham.Card
import Arkham.Card.EncounterCard
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Treachery.Cards qualified as Treacheries

newtype BetweenWorlds = BetweenWorlds LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

betweenWorlds :: LocationCard BetweenWorlds
betweenWorlds = location BetweenWorlds Cards.betweenWorlds 3 (Static 1)

instance HasAbilities BetweenWorlds where
  getAbilities (BetweenWorlds a) = extend1 a $ mkAbility a 1 $ forced $ RoundEnds #when

instance RunMessage BetweenWorlds where
  runMessage msg l@(BetweenWorlds attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      iids <- select $ investigatorAt attrs
      if null iids
        then do
          let asTreachery = lookupEncounterCard Treacheries.betweenWorlds (toCardId attrs)
          nexus <- selectJust $ locationIs Cards.nexusOfNKai
          selectEach (enemyAt attrs) (`enemyMoveTo` nexus)
          removeLocation attrs
          push $ AddToEncounterDiscard asTreachery
        else for_ iids \iid -> assignDamageAndHorror iid (attrs.ability 1) 1 1
      pure l
    _ -> BetweenWorlds <$> liftRunMessage msg attrs
