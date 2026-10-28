module Arkham.Homebrew.AgesUnwound.Events.AgencyStrikeTeam (agencyStrikeTeam) where

import Arkham.Event.Import.Lifted
import Arkham.Helpers.Investigator (getCanDiscoverClues)
import Arkham.Homebrew.AgesUnwound.CardDefs.Events qualified as Cards
import Arkham.Homebrew.AgesUnwound.Helpers (campaignI18n)
import Arkham.Matcher
import Arkham.Modifier (ModifierType (CannotDealDamage))

newtype AgencyStrikeTeam = AgencyStrikeTeam EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | "This action does not provoke attacks of opportunity" rides on the def as
@cdAttackOfOpportunityModifiers@, which is where the play action reads it.
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
      canDealDamage <- iid <=~> InvestigatorWithoutModifier CannotDealDamage
      let damageableEnemy = EnemyCanBeDamagedBySource (toSource attrs)
      locations <-
        select
          $ orConnected_ (locationWithInvestigator iid)
          <> oneOf
            ([LocationWithEnemy damageableEnemy | canDealDamage] <> [locationWithDiscoverableCluesBy iid])
      chooseTargetM iid locations \lid -> do
        enemies <- if canDealDamage then select (enemyAt lid <> damageableEnemy) else pure []
        canDiscover <- getCanDiscoverClues NotInvestigate iid lid
        chooseOneM iid $ campaignI18n do
          when (notNull enemies)
            $ labeled "agencyStrikeTeam.damageEachEnemy"
            $ for_ enemies
            $ nonAttackEnemyDamage (Just iid) attrs 3
          when canDiscover
            $ labeled "agencyStrikeTeam.discoverClues"
            $ discoverAt NotInvestigate iid attrs 3 lid
      pure e
    _ -> AgencyStrikeTeam <$> liftRunMessage msg attrs
