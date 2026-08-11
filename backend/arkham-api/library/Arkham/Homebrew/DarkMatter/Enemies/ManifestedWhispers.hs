module Arkham.Homebrew.DarkMatter.Enemies.ManifestedWhispers (manifestedWhispers) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Placement

newtype ManifestedWhispers = ManifestedWhispers EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

manifestedWhispers :: EnemyCard ManifestedWhispers
manifestedWhispers = enemy ManifestedWhispers Cards.manifestedWhispers

{- | "Hidden. Peril. Revelation - Secretly add this card to your hand.
Forced - At the end of the round, if there are no other investigators at your
location and Manifested Whispers is in your hand: Spawn it at your location."
-}
instance HasAbilities ManifestedWhispers where
  getAbilities (ManifestedWhispers a) = case a.placement of
    HiddenInHand iid ->
      [ restricted
          a
          1
          ( youExist
              $ InvestigatorWithId iid
              <> at_ (not_ $ LocationWithInvestigator (not_ $ InvestigatorWithId iid))
          )
          $ forced
          $ RoundEnds #when
      ]
    _ -> getAbilities a

instance RunMessage ManifestedWhispers where
  runMessage msg e@(ManifestedWhispers attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      place attrs (HiddenInHand iid)
      pure e
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withLocationOf iid \lid -> push $ EnemySpawnAtLocationMatching Nothing (LocationWithId lid) attrs.id
      pure e
    _ -> ManifestedWhispers <$> liftRunMessage msg attrs
