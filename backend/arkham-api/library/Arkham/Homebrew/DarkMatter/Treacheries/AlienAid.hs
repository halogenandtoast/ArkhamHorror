module Arkham.Homebrew.DarkMatter.Treacheries.AlienAid (alienAid) where

import Arkham.Ability
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Treachery.Import.Lifted

newtype AlienAid = AlienAid TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alienAid :: TreacheryCard AlienAid
alienAid = treachery AlienAid Cards.alienAid

{- | "Revelation - Put Alien Aid into play into your threat area. / [free] Take 2
horror: If Mi-Go Scientist is in play, you may ready it and move it to any
location. Discard Alien Aid."
-}
instance HasAbilities AlienAid where
  getAbilities (AlienAid a) =
    [restricted a 1 Here $ freeReaction AnyWindow]

instance RunMessage AlienAid where
  runMessage msg t@(AlienAid attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid (attrs.ability 1) 2
      selectOne (enemyIs Enemies.miGoScientist) >>= traverse_ \scientist -> do
        readyThis scientist
        locations <- select Anywhere
        chooseTargetM iid locations \lid -> enemyMoveTo (attrs.ability 1) scientist lid
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> AlienAid <$> liftRunMessage msg attrs
