module Arkham.Campaigns.BrethrenOfAsh.Key where

import Arkham.Prelude

data BrethrenOfAshKey
  = InvestigatorsDefeatedTheirMaskedPursuer
  | InvestigatorsSavedMiskatonicUniversity
  | MiskatonicUniversityBurned
  | ServantOfElokoss
  | InvestigatorsDiscoveredTheCultsWhereabouts
  | InvestigatorsFailedInTheirSearch
  | InvestigatorsKilledTheServantOfFlame
  | InvestigatorsScouredArkhamForAnswers
  | InvestigatorsStirredUpTrouble
  | TheServantOfFlameEscaped
  | ElokossWasReborn
  | InvestigatorsDefeatedElokossAndTheBrethrenOfAsh
  | InvestigatorsStoppedElokosssGloriousRebirth
  | InvestigatorsFloodedTheBrethrenOfAshsSummoningRitual
  | InvestigatorsWereMarkedByElokoss
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)
