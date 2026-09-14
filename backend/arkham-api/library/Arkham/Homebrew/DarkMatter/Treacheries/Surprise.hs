module Arkham.Homebrew.DarkMatter.Treacheries.Surprise (surprise) where

import Arkham.Helpers.Location (withLocationOf)
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (campaignI18n)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Treachery.Import.Lifted

newtype Surprise = Surprise TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

surprise :: TreacheryCard Surprise
surprise = treachery Surprise Cards.surprise

{- | "Peril. Revelation - Test [willpower] or [agility] (4). If you fail, either
move The Boogeyman to your current location, or The Boogeyman immediately
attacks you. (from any location)"
-}
instance RunMessage Surprise where
  runMessage msg t@(Surprise attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      chooseOneM iid $ for_ [#willpower, #agility] \skill ->
        skillLabeled skill $ revelationSkillTest sid iid attrs skill (Fixed 4)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      selectOne (enemyIs Enemies.theBOOGEYMAN) >>= traverse_ \boogeyman -> do
        chooseOneM iid $ campaignI18n do
          labeled "surprise.moveBoogeyman"
            $ withLocationOf iid
            $ enemyMoveTo attrs boogeyman
          labeled "surprise.boogeymanAttacks" $ initiateEnemyAttack boogeyman attrs iid
      pure t
    _ -> Surprise <$> liftRunMessage msg attrs
