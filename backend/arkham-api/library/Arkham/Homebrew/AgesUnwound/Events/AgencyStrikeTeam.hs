module Arkham.Homebrew.AgesUnwound.Events.AgencyStrikeTeam (agencyStrikeTeam) where

import Arkham.Event.Import.Lifted
import Arkham.Homebrew.AgesUnwound.CardDefs.Events qualified as Cards
import Arkham.Homebrew.AgesUnwound.Helpers (campaignI18n)
import Arkham.Matcher

newtype AgencyStrikeTeam = AgencyStrikeTeam EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | TODO(ages-unwound): "This action does not provoke attacks of opportunity"
lives on the def as @cdAttackOfOpportunityModifiers =
[DoesNotProvokeAttacksOfOpportunity]@, and @CardDefs/Events.hs@ is the
orchestrator's. Playing it currently provokes.
-}
agencyStrikeTeam :: EventCard AgencyStrikeTeam
agencyStrikeTeam = event AgencyStrikeTeam Cards.agencyStrikeTeam

{- | "Choose either your location or a connecting location. Either deal 3 damage
to each enemy at the chosen location, or discover 3 clues at the chosen
location. This action does not provoke attacks of opportunity."
-}
instance RunMessage AgencyStrikeTeam where
  runMessage msg e@(AgencyStrikeTeam attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      locations <- select $ orConnected_ (locationWithInvestigator iid)
      chooseTargetM iid locations \lid -> chooseOneM iid $ campaignI18n do
        labeled "agencyStrikeTeam.damageEachEnemy"
          $ selectEach (enemyAt lid)
          $ nonAttackEnemyDamage (Just iid) attrs 3
        labeled "agencyStrikeTeam.discoverClues" $ discoverAt NotInvestigate iid attrs 3 lid
      pure e
    _ -> AgencyStrikeTeam <$> liftRunMessage msg attrs
