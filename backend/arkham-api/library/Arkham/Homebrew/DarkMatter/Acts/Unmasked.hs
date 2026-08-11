module Arkham.Homebrew.DarkMatter.Acts.Unmasked (unmasked) where

import Arkham.Ability
import Arkham.Act.Import.Lifted hiding (InvestigatorDefeated)
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Matcher

newtype Unmasked = Unmasked ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unmasked :: ActCard Unmasked
unmasked = act (3, A) Unmasked Cards.unmasked Nothing

{- | "Forced - When an investigator is defeated, if there is a copy of Your Other
Self in their threat area: That copy is defeated as well." /
"Objective - If there are no copies of Your Other Self in play, and at least 1
investigator is undefeated, advance."
-}
instance HasAbilities Unmasked where
  getAbilities (Unmasked a) =
    [ restricted a 1 (exists $ enemyIs Enemies.yourOtherSelf)
        $ forced
        $ InvestigatorDefeated #when ByAny Anyone
    , restricted
        a
        2
        (not_ (exists $ enemyIs Enemies.yourOtherSelf) <> exists UneliminatedInvestigator)
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage Unmasked where
  runMessage msg a@(Unmasked attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      copies <- select $ enemyIs Enemies.yourOtherSelf <> EnemyIsEngagedWith (InvestigatorWithId iid)
      for_ copies \eid -> push $ DefeatEnemy eid iid (toSource attrs)
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advanceVia #other attrs attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> Unmasked <$> liftRunMessage msg attrs
