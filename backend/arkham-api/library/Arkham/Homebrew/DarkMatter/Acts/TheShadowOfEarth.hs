module Arkham.Homebrew.DarkMatter.Acts.TheShadowOfEarth (theShadowOfEarth) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Matcher

newtype TheShadowOfEarth = TheShadowOfEarth ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theShadowOfEarth :: ActCard TheShadowOfEarth
theShadowOfEarth = act (3, A) TheShadowOfEarth Cards.theShadowOfEarth Nothing

{- | "Objective - If The Entity is defeated, advance.
Objective - If each undefeated investigator has resigned: (-> R3)"
-}
instance HasAbilities TheShadowOfEarth where
  getAbilities (TheShadowOfEarth a) =
    [ mkAbility a 1
        $ Objective
        $ forced
        $ EnemyDefeated #after Anyone ByAny (enemyIs Enemies.theEntity)
    , restricted a 2 (not_ $ exists $ UneliminatedInvestigator <> not_ ResignedInvestigator)
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage TheShadowOfEarth where
  runMessage msg a@(TheShadowOfEarth attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advanceVia #other attrs attrs
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      push R3
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> TheShadowOfEarth <$> liftRunMessage msg attrs
