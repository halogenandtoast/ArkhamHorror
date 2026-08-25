module Arkham.Homebrew.DarkMatter.Treacheries.SceneShifting (sceneShifting) where

import Arkham.Helpers.Location (withLocationOf)
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (campaignI18n)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Treachery.Import.Lifted

newtype SceneShifting = SceneShifting TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sceneShifting :: TreacheryCard SceneShifting
sceneShifting = treachery SceneShifting Cards.sceneShifting

{- | "Surge. Revelation - You must either (choose one):
- Place 1 doom on the current agenda.
- Move each enemy 1 location toward you. Each enemy at your location immediately
  attacks you."
-}
instance RunMessage SceneShifting where
  runMessage msg t@(SceneShifting attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      chooseOneM iid $ campaignI18n do
        labeled' "sceneShifting.placeDoom" $ placeDoomOnAgendaBy attrs 1
        labeled' "sceneShifting.moveEnemies" do
          withLocationOf iid \lid -> do
            enemies <- select $ UnengagedEnemy <> not_ (EnemyAt $ LocationWithId lid)
            for_ enemies \enemy -> moveTowardsMatching attrs enemy (LocationWithId lid)
          -- the moves resolve from the queue, so the enemies that arrive can
          -- only be seen once they have happened
          doStep 1 msg
      pure t
    DoStep 1 (Revelation iid (isSource attrs -> True)) -> do
      here <- select $ enemyAtLocationWith iid
      for_ here \enemy -> initiateEnemyAttack enemy attrs iid
      pure t
    _ -> SceneShifting <$> liftRunMessage msg attrs
