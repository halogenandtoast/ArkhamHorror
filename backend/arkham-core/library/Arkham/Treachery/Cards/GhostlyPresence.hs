module Arkham.Treachery.Cards.GhostlyPresence (
  ghostlyPresence,
  GhostlyPresence (..),
) where

import Arkham.Prelude

import Arkham.Attack
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner
import Arkham.Zone

newtype GhostlyPresence = GhostlyPresence TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

ghostlyPresence :: TreacheryCard GhostlyPresence
ghostlyPresence = treachery GhostlyPresence Cards.ghostlyPresence

instance RunMessage GhostlyPresence where
  runMessage msg t@(GhostlyPresence attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      mNahab <- selectOne $ enemyIs Enemies.nahab
      case mNahab of
        Just nahab -> pushAll [Ready (toTarget nahab), HunterMove nahab, DoStep 1 msg]
        Nothing -> push $ findEncounterCard iid attrs [FromEncounterDeck, FromEncounterDiscard] Enemies.nahab
      pure t
    DoStep 1 (Revelation _iid (isSource attrs -> True)) -> do
      -- unlikely but perhaps nahab died from the hunter move
      mNahab <- selectOne $ enemyIs Enemies.nahab
      for_ mNahab $ \nahab -> do
        lead <- getLeadPlayer
        iids <- selectList $ InvestigatorAt $ locationWithEnemy nahab
        atSiteOfTheSacrifice <-
          selectAny $ locationIs Locations.siteOfTheSacrifice <> locationWithEnemy nahab
        pushAll
          $ chooseOneAtATime
            lead
            [targetLabel iid [InitiateEnemyAttack $ enemyAttack nahab attrs iid] | iid <- iids]
          : [PlaceDoom (toSource attrs) (toTarget nahab) 1 | atSiteOfTheSacrifice]
      pure t
    FoundEncounterCard iid (isTarget attrs -> True) card -> do
      pushM $ createEnemy card (locationWithInvestigator iid)
      pure t
    _ -> GhostlyPresence <$> runMessage msg attrs
