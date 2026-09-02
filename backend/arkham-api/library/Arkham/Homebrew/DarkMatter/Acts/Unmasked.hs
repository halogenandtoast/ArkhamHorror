module Arkham.Homebrew.DarkMatter.Acts.Unmasked (unmasked) where

import Arkham.Ability
import Arkham.Act.Import.Lifted hiding (InvestigatorDefeated)
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Matcher
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype Unmasked = Unmasked ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unmasked :: ActCard Unmasked
unmasked = act (3, A) Unmasked Cards.unmasked Nothing

instance HasAbilities Unmasked where
  getAbilities (Unmasked a) =
    [ mkAbility a 1
        $ forced
        $ InvestigatorDefeated #when ByAny (InvestigatorEngagedWith $ enemyIs Enemies.yourOtherSelf)
    , onlyOnce
        $ restricted
          a
          2
          (not_ (exists $ enemyIs Enemies.yourOtherSelf) <> exists UneliminatedInvestigator)
        $ Objective
        $ forced AnyWindow
    ]

defeatedInvestigators :: [Window] -> [InvestigatorId]
defeatedInvestigators ws = [who | Window _ (Window.InvestigatorDefeated _ who) _ _ <- ws]

instance RunMessage Unmasked where
  runMessage msg a@(Unmasked attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (defeatedInvestigators -> iids) _ -> do
      for_ iids \iid -> do
        copies <- select $ enemyIs Enemies.yourOtherSelf <> EnemyIsEngagedWith (InvestigatorWithId iid)
        for_ copies \copy -> defeatEnemy copy iid attrs
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advanceVia #other attrs attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R2
      pure a
    _ -> Unmasked <$> liftRunMessage msg attrs
