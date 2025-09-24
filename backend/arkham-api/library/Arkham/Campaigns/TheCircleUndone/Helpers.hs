module Arkham.Campaigns.TheCircleUndone.Helpers (module Arkham.Campaigns.TheCircleUndone.Helpers, module Arkham.Campaigns.TheCircleUndone.I18n) where

import Arkham.Ability
import Arkham.Campaigns.TheCircleUndone.I18n
import Arkham.Card.CardDef
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Helpers.Log ()
import Arkham.Id
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Queue
import Arkham.Prelude

getHauntedAbilities :: HasGame m => InvestigatorId -> m [Ability]
getHauntedAbilities iid = select $ HauntedAbility <> AbilityOnLocation (locationWithInvestigator iid)

runHauntedAbilities :: ReverseQueue m => InvestigatorId -> m ()
runHauntedAbilities iid = do
  hauntedAbilities <- getHauntedAbilities iid
  chooseOneAtATimeM iid $ for_ hauntedAbilities \ab -> abilityLabeled iid ab nothing

runLocationHauntedAbilities :: ReverseQueue m => InvestigatorId -> LocationId -> m ()
runLocationHauntedAbilities iid lid = do
  hauntedAbilities <- select $ HauntedAbility <> AbilityOnLocation (LocationWithId lid)
  chooseOneAtATimeM iid $ for_ hauntedAbilities \ab -> abilityLabeled iid ab nothing

allPrologueInvestigators :: [CardDef]
allPrologueInvestigators =
  [ Investigators.gavriellaMizrah
  , Investigators.jeromeDavids
  , Investigators.pennyWhite
  , Investigators.valentinoRivas
  ]
