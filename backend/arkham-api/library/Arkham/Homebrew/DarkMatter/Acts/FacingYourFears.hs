module Arkham.Homebrew.DarkMatter.Acts.FacingYourFears (facingYourFears) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.DarkMatter.Traits (pattern School)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype FacingYourFears = FacingYourFears ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

facingYourFears :: ActCard FacingYourFears
facingYourFears = act (3, A) FacingYourFears Cards.facingYourFears Nothing

{- | "[free] Spend 1[per_investigator] clues, as a group: Switch two adjacent
locations with each other.
Objective - Save as many of the children as you can! If The Boogeyman is
defeated, advance."
-}
instance HasAbilities FacingYourFears where
  getAbilities (FacingYourFears a) =
    [ mkAbility a 1 $ FastAbility (GroupClueCost (PerPlayer 1) Anywhere)
    , mkAbility a 2
        $ Objective
        $ forced
        $ EnemyDefeated #after Anyone ByAny (enemyIs Enemies.theBOOGEYMAN)
    ]

instance RunMessage FacingYourFears where
  runMessage msg a@(FacingYourFears attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <- select $ LocationWithTrait School
      chooseOneM iid $ targets locations \first' -> do
        adjacent <- select $ connectedFrom (LocationWithId first') <> LocationWithTrait School
        chooseOneM iid $ targets adjacent \second' ->
          push $ ScenarioSpecific "switchLocations" (toJSON (first', second'))
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advanceVia #other attrs attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R3
      pure a
    _ -> FacingYourFears <$> liftRunMessage msg attrs
