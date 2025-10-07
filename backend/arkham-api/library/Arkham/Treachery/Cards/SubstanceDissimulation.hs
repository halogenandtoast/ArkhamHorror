module Arkham.Treachery.Cards.SubstanceDissimulation (substanceDissimulation) where

import Arkham.Campaigns.TheScarletKeys.Helpers (hollow)
import Arkham.Helpers.Modifiers (ModifierType (..), getModifiers)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype SubstanceDissimulation = SubstanceDissimulation TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

substanceDissimulation :: TreacheryCard SubstanceDissimulation
substanceDissimulation = treachery SubstanceDissimulation Cards.substanceDissimulation

instance RunMessage SubstanceDissimulation where
  runMessage msg t@(SubstanceDissimulation attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      mods <- getModifiers iid
      let hollowed = [cardId | Hollow cardId <- mods]
      hollowCards <- traverse fetchCard hollowed
      hollowedMatches <- forMaybeM hollowCards \card -> do
        others <-
          select $ oneOf [inHandOf NotForPlay iid, inPlayAreaOf iid] <> basic (CardWithTitle card.title)
        pure $ guard (notNull others) $> (card, others)

      if null hollowedMatches
        then do
          cards <- select $ basic NonWeakness <> oneOf [inHandOf NotForPlay iid, inPlayAreaOf iid]
          focusCards cards $ chooseTargetM iid cards $ hollow iid
        else do
          focusCards hollowCards do
            chooseOneAtATimeM iid do
              for_ hollowedMatches \(card, same) -> do
                targeting card do
                  traverse_ discard same
                  assignDamageAndHorror iid attrs 1 1

      pure t
    _ -> SubstanceDissimulation <$> liftRunMessage msg attrs
