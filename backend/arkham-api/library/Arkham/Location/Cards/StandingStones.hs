module Arkham.Location.Cards.StandingStones (standingStones) where

import Arkham.Ability
import Arkham.Card.CardDef
import Arkham.Helpers.SkillTest
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted hiding (RevealChaosToken)
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Scenarios.FatalMirage.Helpers
import Arkham.Story.Cards qualified as Stories

newtype StandingStones = StandingStones LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

standingStones :: LocationCard StandingStones
standingStones = location StandingStones Cards.standingStones 2 (PerPlayer 4)

mirageCards :: [CardDef]
mirageCards = [Cards.theBlackStone, Cards.dyersClassroom]

instance HasModifiersFor StandingStones where
  getModifiersFor (StandingStones a) = clearedOfMirages a mirageCards

instance HasAbilities StandingStones where
  getAbilities (StandingStones a) =
    extendRevealed
      a
      [ mirage a 2 mirageCards
      , playerLimit PerTestOrAbility
          $ restricted a 1 (DuringSkillTest $ WhileInvestigating (be a))
          $ forced
          $ RevealChaosToken #after You #frost
      ]

instance RunMessage StandingStones where
  runMessage msg l@(StandingStones attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid (attrs.ability 1) 1
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) iid (DiscoveredClues 1)
      pure l
    _ -> StandingStones <$> mirageRunner Stories.standingStones mirageCards 2 msg attrs
