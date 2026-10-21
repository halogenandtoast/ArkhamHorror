module Arkham.Treachery.Cards.TracesToNowhere.HeavyRain (heavyRain) where

import Arkham.Ability
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype HeavyRain = HeavyRain TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

heavyRain :: TreacheryCard HeavyRain
heavyRain = treachery HeavyRain Cards.heavyRain

{- | "Attached location gets +1 shroud. No more than 1 clue at a time may be
discovered from attached location."
-}
instance HasModifiersFor HeavyRain where
  getModifiersFor (HeavyRain a) = for_ a.attached \case
    LocationTarget lid -> modified_ a lid [ShroudModifier 1, MaxCluesDiscovered 1]
    _ -> pure ()

instance HasAbilities HeavyRain where
  getAbilities (HeavyRain a) = [mkAbility a 1 $ forced $ RoundEnds #when]

instance RunMessage HeavyRain where
  runMessage msg t@(HeavyRain attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      locations <-
        select
          $ NearestLocationTo iid
          $ LocationWithAnyClues
          <> not_ (LocationWithTreachery $ treacheryIs Cards.heavyRain)
      if null locations
        then gainSurge attrs
        else chooseOrRunOneM iid $ targets locations $ attachTreachery attrs
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      toDiscard (attrs.ability 1) attrs
      pure t
    _ -> HeavyRain <$> liftRunMessage msg attrs
