module Arkham.Homebrew.AgesUnwound.Treacheries.AspectsOfInfinity (aspectsOfInfinity) where

import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AgesUnwound.Helpers (campaignI18n)
import Arkham.I18n
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted

newtype AspectsOfInfinity = AspectsOfInfinity TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aspectsOfInfinity :: TreacheryCard AspectsOfInfinity
aspectsOfInfinity = treachery AspectsOfInfinity Cards.aspectsOfInfinity

{- | "Revelation - Choose two. Perform the first part of one and the second part
of the other: Take 2 damage. / Heal 2 damage. -- Take 2 horror. / Heal 2 horror.
-- Lose 2 actions. / Gain 2 actions. -- Draw the top card of the encounter deck.
/ This card loses surge."
-}
instance RunMessage AspectsOfInfinity where
  runMessage msg t@(AspectsOfInfinity attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      let
        -- the second part of a different option than the one just chosen
        secondPart chosen = chooseOneM iid $ campaignI18n do
          when (chosen /= 0) $ unscoped $ countVar 2 $ labeled "healDamage" $ healDamageIfCan iid attrs 2
          when (chosen /= 1) $ unscoped $ countVar 2 $ labeled "healHorror" $ healHorrorIfCan iid attrs 2
          when (chosen /= 2) $ countVar 2 $ labeled "gainActions" $ gainActions iid attrs 2
          when (chosen /= 3) $ labeled "aspectsOfInfinity.loseSurge" $ push $ CancelSurge (toSource attrs)
      chooseOneM iid $ campaignI18n do
        unscoped $ countVar 2 $ labeled "takeDamage" do
          assignDamage iid attrs 2
          secondPart (0 :: Int)
        unscoped $ countVar 2 $ labeled "takeHorror" do
          assignHorror iid attrs 2
          secondPart 1
        unscoped $ countVar 2 $ labeled "loseActions" do
          loseStandardActions iid attrs 2
          secondPart 2
        unscoped $ countVar 1 $ labeled "drawTopCardOfEncounterDeck" do
          drawEncounterCard iid attrs
          secondPart 3
      pure t
    _ -> AspectsOfInfinity <$> liftRunMessage msg attrs
