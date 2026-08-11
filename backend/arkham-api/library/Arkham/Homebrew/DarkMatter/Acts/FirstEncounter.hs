module Arkham.Homebrew.DarkMatter.Acts.FirstEncounter (firstEncounter) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Matcher

newtype FirstEncounter = FirstEncounter ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

firstEncounter :: ActCard FirstEncounter
firstEncounter = act (1, A) FirstEncounter Cards.firstEncounter Nothing

-- "You cannot leave the Entrance Tunnel."
instance HasModifiersFor FirstEncounter where
  getModifiersFor (FirstEncounter a) =
    modifySelect a (InvestigatorAt $ locationIs Locations.entranceTunnel) [CannotMove]

{- | "Objective - If there are 1[per_investigator] clues on this act, or after The
Greys is defeated, advance."
-}
instance HasAbilities FirstEncounter where
  getAbilities (FirstEncounter a) =
    [ restricted a 1 (CluesOnThis $ AtLeast $ PerPlayer 1)
        $ Objective
        $ forced AnyWindow
    , mkAbility a 2
        $ Objective
        $ forced
        $ EnemyDefeated #after Anyone ByAny (enemyIs Enemies.theGreys)
    ]

instance RunMessage FirstEncounter where
  runMessage msg a@(FirstEncounter attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) n | n `elem` [1, 2] -> do
      advanceVia #other attrs attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> FirstEncounter <$> liftRunMessage msg attrs
